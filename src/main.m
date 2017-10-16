// 引入 Foundation.h
#import <Foundation/Foundation.h>

#include "main.h"


typedef SDL_Window * SDL_Window_Access;
typedef SDL_Renderer * SDL_Renderer_Access;
typedef SDL_Event * SDL_Event_Access;

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


TTF_Font* USE_FONT = NULL;

void draw_message_box(SDL_Renderer_Access render) {
  SDL_Color white = {255, 255, 255};
  NSString *item = NULL;
  const char *text = NULL;

  int current = [box_1->history count];
  for (int counter = 0; counter < 5; counter++) {
    if (current >= 1) {
      item = [box_1->history objectAtIndex: current - 1];
    }
    else {
      item = @"";
    }

    current--;

    SDL_SetRenderDrawColor(render, white.r, white.g, white.b, 0);
    SDL_RenderDrawLines(render, box_1->box, 5);

    SDL_Surface* surfaceMessage = NULL;

    if (item != NULL) {
      text = [item UTF8String];
      SDL_Rect box = {
		      .x = 0,
		      .y = (20 + counter) * 24,
		      .w = string.count_width(text, 24),
		      .h = 24
      };

      surfaceMessage = TTF_RenderUTF8_Solid(USE_FONT,
              text,
					    white);
      SDL_Texture_Access access = SDL_CreateTextureFromSurface(render, surfaceMessage);
      SDL_FreeSurface(surfaceMessage);

      SDL_RenderCopy(render, access, NULL, &(box));
      SDL_DestroyTexture(access);
    }
  }
}

void draw_view(SDL_Renderer_Access render) {
  SDL_Rect rect = {.x = 0, .y = 0, .w = GRID_LENGTH, .h = GRID_LENGTH};
  SDL_SetRenderDrawColor(render, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(render);

  position.x = GRID_LENGTH * camera_1->player->base->Graph_Position.x;
  position.y = GRID_LENGTH * camera_1->player->base->Graph_Position.y;
  SDL_RenderCopy(render, camera_1->player->base->Mark->access, NULL, &(position));

  uint8_t used = [character_pool instance_count];
  for (int next = 1; next < used; next++) {
    Status_Access npc = [character_pool get_instance_by_index: next];

    if (npc->base->status == IN_USE) {
      if (![npc->base->Real_Position eq: camera_1->player->base->Real_Position]) {
	rect.x = GRID_LENGTH * npc->base->Graph_Position.x;
	rect.y = GRID_LENGTH * npc->base->Graph_Position.y;
	SDL_RenderCopy(render, npc->base->Mark->access, NULL, &(rect));
      }
    }
  }
  draw_message_box(render);

  SDL_RenderPresent(render);
}

Execute_Result init_view(SDL_Renderer_Access render) {
  SDL_Color white = {255, 255, 255};

  USE_FONT = TTF_OpenFont(FONT_FAMILY, 512);
  if (!USE_FONT) {
    printf("TTF_OpenFont: %s\n", TTF_GetError());
    return EXECUTE_FAILED;
  }

  SDL_Surface* surfaceMessage = NULL;
  uint8_t counter = 0;
  Style_Access result = Style_Pool_Interface.next(style_pool, &counter);

  while (result != NULL) {
    surfaceMessage = TTF_RenderUTF8_Solid(USE_FONT,
					  [result->mark UTF8String],
					  white);
    result->access = SDL_CreateTextureFromSurface(render, surfaceMessage);

    SDL_FreeSurface(surfaceMessage);

    result = Style_Pool_Interface.next(style_pool, &counter);
  }

  result = Style_Pool_Interface.find(style_pool, @"player");
  character.set_style(camera_1->player, result);

  Style_Access dead = Style_Pool_Interface.find(style_pool, @"dead");
  camera.set_dead_style(camera_1, dead);
  return EXECUTE_SUCCESS;
}


void submain(const char *root_dir, const char *init_cfg, const char *npc_cfg) {
  Status_Access current = NULL;camera_1->player;
  Message_Type message = DO_NOTHING;

  CONF_PATH = root_dir;
  Execute_Result result = EXECUTE_FAILED;
  srand(time(NULL));

  SDL_Window_Access win;
  SDL_Renderer_Access render;
  bool running = true;

  style_pool = Style_Pool_Interface.start(256);
  character_pool = [Character_Pool create: 20 with_instance_size: 100];
  camera_1 = camera.start();
  box_1 = message_box.start();

  SDL_Init(SDL_INIT_EVERYTHING);
  if (TTF_Init() != 0) {
    goto INIT_FAILED;
  }
  Status_Access Player = [character_pool use_player];
  character.set_name(Player, @"雜魚");
  camera.set_player(camera_1, Player);

  result = setup_style(init_cfg);

  if (result == EXECUTE_FAILED) {
    goto DONE;
  }

  [character_pool parse_npc_config: npc_cfg with_style: style_pool];

  camera.set_map(camera_1, map_1);
  [character_pool use_enemy: @"goblin"
   with_name: @"g 1"
   and_map: camera_1->map];
  [character_pool use_enemy: @"goblin"
   with_name: @"g 2"
   and_map: camera_1->map];

  win = SDL_CreateWindow(GAME_TITLE, 0, 0, WIDTH, HEIGHT, SDL_WINDOW_OPENGL);
  render = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED|SDL_RENDERER_PRESENTVSYNC);

  if (init_view(render) != EXECUTE_SUCCESS) {
    goto INIT_FAILED;
  }

  uint8_t instance_count = [character_pool instance_count];
  uint8_t index = instance_count;
  while (running) {
    current = camera_1->player;
    message = [character_pool action: current];

    switch (message) {
      case QUIT:
        running = false;
      case DO_NOTHING:
        index = instance_count + 1;
        break;
      default:
        index = 1;
        running = camera.take(camera_1, character_pool, box_1, current, message);
        break;
    }

    for (index; index < instance_count; index++) {
      current = [character_pool get_instance_by_index: index];

      if (current->base->is_alive == false) {
        continue;
      }
      message = [character_pool action: current];
      running = camera.take(camera_1, character_pool, box_1, current, message);
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

// 定義 main() 函數
int main(int argc, char *argv[]) {
    // 建立自動釋放池物件， alloc 為配置記憶體區域， init 為初始化物件
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
    map_1 = [Map_Type create];
    [[map_1 init_start: 0 and: 0] init_end: 40 and: 30];


    // 建立 config file 的路徑
    NSString *root_dir = [[[[[NSBundle mainBundle] bundlePath]
      stringByDeletingLastPathComponent]
      stringByDeletingLastPathComponent]
      stringByDeletingLastPathComponent];
    NSString *init_cfg = [NSString stringWithFormat:@"%@/%@", root_dir, @"config/init.cfg"];
    NSString *npc_cfg = [NSString stringWithFormat:@"%@/%@", root_dir, @"config/npc.cfg"];

    submain(
        [root_dir UTF8String],
        [init_cfg UTF8String],
        [npc_cfg UTF8String]);

    // 傳遞 drain 訊息給自動釋放池物件
    [pool drain];

    // 程式結束，回傳整數 0 給作業系統
    return 0;
}
