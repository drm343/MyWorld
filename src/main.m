#import <Foundation/Foundation.h>

#include "main.h"

#define CP(name) Character_Pool_##name

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
Character_Pool *character_pool = NULL;
struct strings *global_repo = NULL;


TTF_Font *USE_FONT = NULL;

void draw_message_box(SDL_Renderer_Access render)
{
    SDL_Color white = { 255, 255, 255 };
    const char *item = NULL;

    int current = strings_count(box_1->history);
    for (int counter = 0; counter < 5; counter++) {
        if (current >= 1) {
            item = strings_lookup_id(box_1->history, current);
        } else {
            item = "";
        }

        current--;

        SDL_SetRenderDrawColor(render, white.r, white.g, white.b, 0);
        SDL_RenderDrawLines(render, box_1->box, 5);

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

    Point_Access_change(camera_1->player->base->Graph_Position);
    position.x = GRID_LENGTH * Point_Access_x();
    position.y = GRID_LENGTH * Point_Access_y();
    SDL_RenderCopy(render, camera_1->player->base->Mark->access, NULL,
                   &(position));

    uint8_t used = CP(instance_count) (character_pool);

    for (int next = 1; next < used; next++) {
        Status_Access npc =
            CP(get_instance_by_index) (character_pool, next);

        if (npc->base->status == IN_USE) {
            if (!Point_Type_eq(npc->base->Real_Position,
                               camera_1->player->base->Real_Position)) {
                Point_Access_change(npc->base->Graph_Position);
                rect.x = GRID_LENGTH * Point_Access_x();
                rect.y = GRID_LENGTH * Point_Access_y();
                SDL_RenderCopy(render, npc->base->Mark->access, NULL,
                               &(rect));
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
    Style_Access result = Style_Pool_Interface.next(style_pool, &counter);

    while (result != NULL) {
        surfaceMessage = TTF_RenderUTF8_Solid(USE_FONT,
                                              result->mark, white);
        result->access =
            SDL_CreateTextureFromSurface(render, surfaceMessage);

        SDL_FreeSurface(surfaceMessage);

        result = Style_Pool_Interface.next(style_pool, &counter);
    }

    result = Style_Pool_Interface.find(style_pool, "player");
    character.set_style(camera_1->player, result);

    Style_Access dead = Style_Pool_Interface.find(style_pool, "dead");
    camera.set_dead_style(camera_1, dead);
    return EXECUTE_SUCCESS;
}


void submain(const char *root_dir, const char *init_cfg,
             const char *npc_cfg)
{
    Status_Access current = NULL;
    camera_1->player;
    Message_Type message = DO_NOTHING;

    CONF_PATH = root_dir;
    Execute_Result result = EXECUTE_FAILED;
    srand(time(NULL));

    SDL_Window_Access win;
    SDL_Renderer_Access render;
    bool running = true;

    style_pool = Style_Pool_Interface.start(256);
    character_pool = CP(create) (20, 100);
    camera_1 = camera.start();
    box_1 = message_box.start();

    SDL_Init(SDL_INIT_EVERYTHING);
    if (TTF_Init() != 0) {
        goto INIT_FAILED;
    }
    if (character_pool == NULL) {
        goto INIT_FAILED;
    }
    Status_Access Player = CP(use_player) (character_pool);
    character.set_name(Player, "雜魚");
    camera.set_player(camera_1, Player);

    result = setup_style(init_cfg);

    if (result == EXECUTE_FAILED) {
        goto INIT_FAILED;
    }

    CP(parse_npc_config) (character_pool, npc_cfg, style_pool);

    camera.set_map(camera_1, map_1);

    CP(use_enemy) (character_pool, "goblin", "g 1", camera_1->map);
    CP(use_enemy) (character_pool, "goblin", "g 2", camera_1->map);
    CP(use_neutral) (character_pool, "villager", "v 1", camera_1->map);
    CP(use_neutral) (character_pool, "villager", "v 2", camera_1->map);

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

    uint8_t instance_count = CP(instance_count) (character_pool);
    uint8_t index = instance_count;
    while (running) {
        current = camera_1->player;
        message = CP(action) (character_pool, current);

        switch (message) {
            case QUIT:
                running = false;
            case DO_NOTHING:
                index = instance_count + 1;
                break;
            default:
                index = 1;
                running =
                    camera.take(camera_1, character_pool, box_1, current,
                                message);
                break;
        }

        for (index; index < instance_count; index++) {
            current = CP(get_instance_by_index) (character_pool, index);

            if (current->base->is_alive == false) {
                continue;
            }
            message = CP(action) (character_pool, current);
            running =
                camera.take(camera_1, character_pool, box_1, current,
                            message);
        }
        draw_view(render);
    }
    Style_Pool_Interface.gc(style_pool);

    SDL_DestroyRenderer(render);
    SDL_DestroyWindow(win);
    TTF_CloseFont(USE_FONT);

  INIT_FAILED:
    TTF_Quit();
    SDL_Quit();

  DONE:
    message_box.stop(box_1);
    camera.stop(camera_1);
    Style_Pool_Interface.stop(style_pool);
}


int main(int argc, char *argv[])
{
    global_repo = strings_new();
    String_Repo_change(global_repo);

    // 求出執行檔所在位置，根據此位置求出 root_dir
    char execution_path[1024];
    char *exist;
    exist = realpath(argv[0], execution_path);

    // 建立自動釋放池物件， alloc 為配置記憶體區域， init 為初始化物件
    NSAutoreleasePool *pool =[[NSAutoreleasePool alloc] init];
    map_1 =[Map_Type create];
  [[map_1 init_start: 0 and: 0] init_end: 40 and:30];


    // 建立 config file 的路徑
    char *init_cfg_path = "/config/init.cfg";
    char *npc_cfg_path = "/config/npc.cfg";

    char *root_dir = dirname(dirname(exist));
    int counter = String_ascii_length(root_dir);

    // 在 Stack 分配固定長度的空間來初始化字串
    int total = counter + String_ascii_length(init_cfg_path);
    char init_cfg[total];
    snprintf(init_cfg, total + 1, "%s%s", root_dir, init_cfg_path);

    total = counter + String_ascii_length(npc_cfg_path);
    char npc_cfg[total];
    snprintf(npc_cfg, total + 1, "%s%s", root_dir, npc_cfg_path);

    submain(root_dir, init_cfg, npc_cfg);


    // 傳遞 drain 訊息給自動釋放池物件
    [pool drain];

    strings_free(global_repo);
}
