#include "main.h"


typedef SDL_Window *SDL_Window_Access;
typedef SDL_Renderer *SDL_Renderer_Access;
typedef SDL_Event *SDL_Event_Access;

int GRID_LENGTH = 24;
SDL_Rect position = {
    .x = 0,
    .y = 0,
    .w = 24,
    .h = 24
};

Camera_Access camera_1 = NULL;
Message_Box_Access box_1 = NULL;
Map_Type *map_1;
Game_Status *game_status_pool = NULL;
struct strings *global_repo = NULL;


TTF_Font *USE_FONT = NULL;


void draw_message_box(SDL_Renderer_Access render)
{
    SDL_Color white = { 255, 255, 255 };
    const char *item = NULL;

    uint8_t current = Message_Box_history_count(box_1);
    for (int counter = 0; counter < 5; counter++) {
        item = Message_Box_get_history_by_index(box_1, &current);

        SDL_SetRenderDrawColor(render, white.r, white.g, white.b, 0);
        SDL_RenderDrawLines(render, Message_Box_box(box_1), 5);

        SDL_Surface *surfaceMessage = NULL;

        if (item != NULL) {
            SDL_Rect box = {
                .x = 0,
                .y = (20 + counter) * 24,
                .w = String_width_length(item, 24),
                .h = 24
            };

            surfaceMessage = TTF_RenderUTF8_Solid(USE_FONT, item, white);
            SDL_Texture_Access access =
                SDL_CreateTextureFromSurface(render, surfaceMessage);
            SDL_FreeSurface(surfaceMessage);

            SDL_RenderCopy(render, access, NULL, &(box));
            SDL_DestroyTexture(access);
        }
    }
}


void draw_view(SDL_Renderer_Access render)
{
    SDL_Rect rect = {.x = 0,.y = 0,.w = GRID_LENGTH,.h = GRID_LENGTH };
    SDL_SetRenderDrawColor(render, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(render);

    Point_Access graph_point = camera_1->player->Graph_Position;
    position.x = GRID_LENGTH * Point_Type_x(graph_point);
    position.y = GRID_LENGTH * Point_Type_y(graph_point);
    SDL_RenderCopy(render, camera_1->player->Mark->access, NULL,
                   &(position));

    uint8_t used = GAME(instance_count) (game_status_pool);

    for (int next = 1; next < used; next++) {
        Character_Access npc =
            GAME(get_instance_by_index) (game_status_pool, next);

        if (npc->status->status == IN_USE) {
            if (!Point_Type_eq
                (npc->Real_Position, camera_1->player->Real_Position)) {

                graph_point = npc->Graph_Position;
                rect.x = GRID_LENGTH * Point_Type_x(graph_point);
                rect.y = GRID_LENGTH * Point_Type_y(graph_point);
                SDL_RenderCopy(render, npc->Mark->access, NULL, &(rect));
            }
        }
    }
    draw_message_box(render);

    SDL_RenderPresent(render);
}


Execute_Result init_view(SDL_Renderer_Access render)
{
    SDL_Color white = { 255, 255, 255 };

    USE_FONT = TTF_OpenFont(FONT_FAMILY, 512);
    if (!USE_FONT) {
#ifdef DEBUG
        DEBUG_PRINT("TTF_OpenFont: %s\n", TTF_GetError());
#endif
        return EXECUTE_FAILED;
    }

    SDL_Surface *surfaceMessage = NULL;
    uint8_t counter = 0;
    Style_Access result = STYLE_P(next) (style_pool, &counter);

    while (result != NULL) {
        surfaceMessage = TTF_RenderUTF8_Solid(USE_FONT,
                                              result->mark, white);
        result->access =
            SDL_CreateTextureFromSurface(render, surfaceMessage);

        SDL_FreeSurface(surfaceMessage);

        result = STYLE_P(next) (style_pool, &counter);
    }

    result = STYLE_P(find) (style_pool, "player");
    CHARA(set_style) (camera_1->player, result);

    Style_Access dead = STYLE_P(find) (style_pool, "dead");
    CAMERA(set_dead_style) (camera_1, dead);
    return EXECUTE_SUCCESS;
}


void submain(const char *root_dir, const char *init_cfg,
             const char *npc_cfg)
{
    camera_1->player;
    Message_Type message = DO_NOTHING;

    CONF_PATH = root_dir;
    Execute_Result result = EXECUTE_FAILED;
    srand(time(NULL));

    SDL_Window_Access win;
    SDL_Renderer_Access render;
    bool running = true;

    style_pool = STYLE_P(start) (256);
    game_status_pool = GAME(create) (20, 100);

    camera_1 = CAMERA(start) ();
    box_1 = BOX(start) ();

    SDL_Init(SDL_INIT_EVERYTHING);
    if (TTF_Init() != 0) {
        goto INIT_FAILED;
    }
    if (game_status_pool == NULL) {
        goto INIT_FAILED;
    }

    Character_Access Player = GAME(use_player) (game_status_pool);
    STATUS(set_name) (Player->status, "雜魚");
    CAMERA(set_player) (camera_1, Player);

    result = setup_style(init_cfg);

    if (result == EXECUTE_FAILED) {
#ifdef DEBUG
        DEBUG_PRINT("config setup failed\n", NULL);
#endif

        goto INIT_FAILED;
    }

    GAME(parse_npc_config) (game_status_pool, npc_cfg, style_pool);

    CAMERA(set_map) (camera_1, map_1);

    GAME(use_enemy) (game_status_pool, "goblin", "g 1", camera_1->map);
    GAME(use_enemy) (game_status_pool, "goblin", "g 2", camera_1->map);
    GAME(use_neutral) (game_status_pool, "villager", "v 1", camera_1->map);
    GAME(use_neutral) (game_status_pool, "villager", "v 2", camera_1->map);

    win =
        SDL_CreateWindow(GAME_TITLE, 0, 0, WIDTH, HEIGHT,
                         SDL_WINDOW_OPENGL);
    render =
        SDL_CreateRenderer(win, -1,
                           SDL_RENDERER_ACCELERATED |
                           SDL_RENDERER_PRESENTVSYNC);

    if (init_view(render) != EXECUTE_SUCCESS) {
        goto INIT_FAILED;
    }

    Character_Access current = NULL;
    uint8_t instance_count = GAME(instance_count) (game_status_pool);
    uint8_t index = instance_count;
    while (running) {
        current = camera_1->player;
        message = GAME(action) (game_status_pool, current);

        switch (message) {
            case QUIT:
                running = false;
            case DO_NOTHING:
                index = instance_count + 1;
                break;
            default:
                index = 1;
                running =
                    CAMERA(take) (camera_1, game_status_pool, box_1,
                                  current, message);
                break;
        }

        for (index; index < instance_count; index++) {
            current =
                GAME(get_instance_by_index) (game_status_pool, index);

            if (current->status->is_alive == false) {
                continue;
            }
            message = GAME(action) (game_status_pool, current);
            running =
                CAMERA(take) (camera_1, game_status_pool, box_1, current,
                              message);
        }
        draw_view(render);
    }
    STYLE_P(free_texture) (style_pool);

    SDL_DestroyRenderer(render);
    SDL_DestroyWindow(win);
    TTF_CloseFont(USE_FONT);

  INIT_FAILED:
    TTF_Quit();
    SDL_Quit();

  DONE:
    BOX(stop) (box_1);
    CAMERA(stop) (camera_1);
    GAME(free) (game_status_pool);
    STYLE_P(stop) (style_pool);
}


int main(int argc, char *argv[])
{
    global_repo = strings_new();
    String_Repo_change(global_repo);

    // 求出執行檔所在位置，根據此位置求出 root_dir
    char execution_path[1024];
    char *exist;
    exist = realpath(argv[0], execution_path);

    map_1 = MAP(create) ();
    MAP(set_top_left) (map_1, 0, 0);
    MAP(set_bottom_right) (map_1, 40, 30);


    // 建立 config file 的路徑
    char *init_cfg_path = "/config/init.cfg";
    char *npc_cfg_path = "/config/npc.cfg";

    char *root_dir = dirname(dirname(exist));
    int counter = String_ascii_length(root_dir);

    int total = counter + String_ascii_length(init_cfg_path);
    char init_cfg[total];
    snprintf(init_cfg, total + 1, "%s%s", root_dir, init_cfg_path);

    total = counter + String_ascii_length(npc_cfg_path);
    char npc_cfg[total];
    snprintf(npc_cfg, total + 1, "%s%s", root_dir, npc_cfg_path);

    submain(root_dir, init_cfg, npc_cfg);

    MAP(free) (map_1);
    strings_free(global_repo);
}
