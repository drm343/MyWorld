#include "main.h"

typedef SDL_Window *SDL_Window_Access;
typedef SDL_Renderer *SDL_Renderer_Access;
typedef SDL_Event *SDL_Event_Access;


ImmutableString ROOT_DIR = NULL;
ImmutableString FONT_FAMILY = NULL;
ImmutableString GAME_TITLE = NULL;


int WIDTH = 800;
int HEIGHT = 600;


int GRID_LENGTH = 24;
SDL_Rect position = {
    .x = 0,
    .y = 0,
    .w = 24,
    .h = 24
};

Camera_Access camera_1 = NULL;
Morph_SubWindow box_message = NULL;
Morph_SubWindow box_main = NULL;
Map_Type *map_1;
Game_Status *game_status_pool = NULL;

TTF_Font *USE_FONT = NULL;
Style_Pool_Access style_pool = NULL;


/** @brief 讀取設定檔，設定視窗畫面所需要的資料
 */
void setup_window(config_setting_t ** setting)
{
    const char *tmp;

    config_setting_lookup_int(*setting, "width", &WIDTH);
    config_setting_lookup_int(*setting, "height", &HEIGHT);

    config_setting_lookup_string(*setting, "font-family", &tmp);
    FONT_FAMILY = String_append_c_str(ROOT_DIR, tmp);

    config_setting_lookup_string(*setting, "title", &tmp);
    GAME_TITLE = String_create(tmp);
}


//Setup player and dead mark for now
void setup_mark(config_setting_t ** setting)
{
    int total_counter = config_setting_length(*setting);
    const char *value;

    for (int counter = 0; counter < total_counter; counter++) {
        config_setting_t *style_setting =
            config_setting_get_elem(*setting, counter);

        const char *key = NULL;

        Style_Access style_access = Style_Pool_malloc(style_pool);
        config_setting_lookup_string(style_setting, "key", &key);
        style_access->name = String_create(key);

        config_setting_lookup_string(style_setting, "mark", &key);
        style_access->mark = String_create(key);

        int is_attackable = 0;
        config_setting_lookup_bool(style_setting, "attackable",
                                   &is_attackable);
        style_access->attackable = is_attackable;

        int is_crossable = 0;
        config_setting_lookup_bool(style_setting, "crossable",
                                   &is_crossable);
        style_access->crossable = is_crossable;
    }
}


// Default Execute_Result value is EXECUTE_FAILED.
// Only run success will change the variable.
Execute_Result setup_style()
{
    Execute_Result result = EXECUTE_FAILED;
    config_t cfg;
    config_setting_t *setting;
    const char *str;

    config_init(&cfg);
    ImmutableString INIT_CONFIG =
        String_append_c_str(ROOT_DIR, "/USER/config/init.cfg");

    /* Read the file. If there is an error, report it and exit. */
    if (!config_read_file(&cfg, INIT_CONFIG->str)) {
        fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
                config_error_line(&cfg), config_error_text(&cfg));

        goto DONE;
    }
    /* check and setup window */
    setting = config_lookup(&cfg, "window");

    if (setting != NULL) {
        setup_window(&setting);
    } else {
        goto DONE;
    }

    /* check and setup mark */
    setting = config_lookup(&cfg, "style");

    if (setting != NULL) {
        setup_mark(&setting);
    } else {
        goto DONE;
    }

    result = EXECUTE_SUCCESS;
  DONE:
    config_destroy(&cfg);
    String_free(INIT_CONFIG);
    return result;;
}


void draw_view(SDL_Renderer_Access render)
{
    SDL_SetRenderDrawColor(render, 0, 0, 0, SDL_ALPHA_OPAQUE);
    //SDL_RenderClear(render);

    SDL_Rect clean_character_area = {
        .x = 0,
        .y = 0,
        .w = 799,
        .h = 20 * 24
    };
    SDL_RenderFillRect(render, &clean_character_area);

    Point graph_point = camera_1->player->Graph_Position;
    position.x = GRID_LENGTH * Point_x(graph_point);
    position.y = GRID_LENGTH * Point_y(graph_point);
    SDL_RenderCopy(render, camera_1->player->Mark->access, NULL,
                   &(position));

    Character_Access npc = Game_Status_reset_iterator(game_status_pool);
    SDL_Rect rect = {.x = 0,.y = 0,.w = GRID_LENGTH,.h = GRID_LENGTH };

    for (npc = Game_Status_next(game_status_pool, npc); npc != NULL;
         npc = Game_Status_next(game_status_pool, npc)) {
        if (npc->status->status == IN_USE) {
            if (!Point_equal
                (npc->Real_Position, camera_1->player->Real_Position)) {

                graph_point = npc->Graph_Position;
                rect.x = GRID_LENGTH * Point_x(graph_point);
                rect.y = GRID_LENGTH * Point_y(graph_point);
                SDL_RenderCopy(render, npc->Mark->access, NULL, &(rect));
            }
        }
    }

    box_message->draw(box_message);

    SDL_RenderPresent(render);
}

Execute_Result init_view(SDL_Renderer_Access render)
{
    SDL_Color white = { 255, 255, 255 };

    USE_FONT = TTF_OpenFont(FONT_FAMILY->str, 512);
    String_free(FONT_FAMILY);
    if (!USE_FONT) {
        DEBUG_PRINT("TTF_OpenFont: %s\n", TTF_GetError());
        return EXECUTE_FAILED;
    }
    SDL_Surface *surfaceMessage = NULL;
    uint8_t counter = 0;
    Style_Access result = Style_Pool_next(style_pool, &counter);

    while (result != NULL) {
        surfaceMessage =
            TTF_RenderUTF8_Solid(USE_FONT, result->mark->str, white);
        result->access =
            SDL_CreateTextureFromSurface(render, surfaceMessage);

        SDL_FreeSurface(surfaceMessage);

        result = Style_Pool_next(style_pool, &counter);
    }

    result = Style_Pool_find(style_pool, "player");
    Character_set_style(camera_1->player, result);

    Style_Access dead = Style_Pool_find(style_pool, "dead");
    Graphic_Camera_set_dead_style(camera_1, dead);

    box_message = Message_Box_start(render, USE_FONT);
    return EXECUTE_SUCCESS;
}

void submain()
{
    Message_Type message = DO_NOTHING;

    Execute_Result result = EXECUTE_FAILED;
    srand(time(NULL));

    SDL_Window_Access win;
    SDL_Renderer_Access render;
    bool running = true;

    style_pool = Style_Pool_start(256);
    game_status_pool = Game_Status_create(20, 100);

    camera_1 = Graphic_Camera_start();

    SDL_Init(SDL_INIT_EVERYTHING);
    if (TTF_Init() != 0) {
        goto INIT_FAILED;
    }
    if (game_status_pool == NULL) {
        goto INIT_FAILED;
    }
    String_reset_width(GRID_LENGTH);

    Character_Access Player = Game_Status_use_player(game_status_pool);
    Status_set_name(Player->status, "雜魚");
    Graphic_Camera_set_player(camera_1, Player);

    result = setup_style();

    if (result == EXECUTE_FAILED) {
        DEBUG_MESSAGE("config setup failed\n");
        goto INIT_FAILED;
    }
    // 取得 config
    ImmutableString NPC_CONFIG =
        String_append_c_str(ROOT_DIR, "/USER/config/npc.cfg");
    Game_Status_parse_npc_config(game_status_pool, NPC_CONFIG->str,
                                 style_pool);
    String_free(NPC_CONFIG);

    Graphic_Camera_set_map(camera_1, map_1);

    Game_Status_use_enemy(game_status_pool, "goblin", "g 1",
                          camera_1->map);
    Game_Status_use_enemy(game_status_pool, "goblin", "g 2",
                          camera_1->map);
    Game_Status_use_neutral(game_status_pool, "villager", "v 1",
                            camera_1->map);
    Game_Status_use_neutral(game_status_pool, "villager", "v 2",
                            camera_1->map);

    win =
        SDL_CreateWindow(GAME_TITLE->str, 0, 0, WIDTH, HEIGHT,
                         SDL_WINDOW_OPENGL);
    String_free(GAME_TITLE);
    render =
        SDL_CreateRenderer(win, -1,
                           SDL_RENDERER_ACCELERATED |
                           SDL_RENDERER_PRESENTVSYNC);

    if (init_view(render) != EXECUTE_SUCCESS) {
        goto INIT_FAILED;
    }
    Character_Access current = NULL;
    Iterator iterator = NULL;
    while (running) {
        if (iterator == NULL) {
            iterator = Game_Status_action_order(game_status_pool);
        }
        current = ITER(reset_iterator) (iterator);

        message = Game_Status_action(game_status_pool, current);

        switch (message) {
            case QUIT:
                running = false;
            case DO_NOTHING:
                goto DRAW;
                break;
            default:
                running =
                    Graphic_Camera_take(camera_1, game_status_pool,
                                        box_message, current, message);
                goto NPC_ACTION;
        }

      NPC_ACTION:
        for (current = ITER(next) (iterator);
             ITER(done) (iterator) != true;
             current = ITER(next) (iterator)) {
            if (current->status->is_alive == false) {
                continue;
            }
            message = Game_Status_action(game_status_pool, current);
            running =
                Graphic_Camera_take(camera_1, game_status_pool,
                                    box_message, current, message);
        }
        iterator = NULL;
      DRAW:
        draw_view(render);
    }
    Style_Pool_free_texture(style_pool);
    SDL_DestroyRenderer(render);
    SDL_DestroyWindow(win);
    TTF_CloseFont(USE_FONT);
  INIT_FAILED:
    TTF_Quit();
    SDL_Quit();
  DONE:
    if (iterator != NULL) {
        iterator->free(iterator);
        iterator = NULL;
    }
    box_message->free(box_message);
    Graphic_Camera_stop(camera_1);
    Game_Status_free(game_status_pool);
    Style_Pool_stop(style_pool);
}

int main(int argc, char *argv[])
{
    // 取得 root_dir
    char execution_path[1024];
    char *exist;
    exist = realpath(argv[0], execution_path);
    ROOT_DIR = String_create(dirname(dirname(exist)));
    map_1 = Map_Type_create();
    Map_Type_set_position(map_1, 0, 0);
    Map_Type_set_bottom_right(map_1, 40, 30);
    submain();
    Map_Type_free(map_1);
    // 釋放已完成的 config
    String_free(ROOT_DIR);
}
