#include "main.h"

extern void adainit(void);
extern void adafinal(void);
extern void get_dir(void);

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
Map_Type map_1 = {
		  .start = {
			    .x = 0,
			    .y = 0
		  },
		  .end = {
			  .x = 40,
			  .y = 30
		  }
};
TTF_Font* USE_FONT = NULL;

void draw_message_box(SDL_Renderer_Access render) {
  SDL_Color white = {255, 255, 255};
  String text = NULL;

  int current = box_1->current - 4;
  if (current < 0) {
    current += 10;
  }
  for (int counter = 0; counter < 5; counter++) {
    if (current >= 1) {
      text = box_1->history[current - 1];
    }
    else {
      text = box_1->history[9];
    }
    current++;
    if (current >= 10) {
      current = 0;
    }

    SDL_SetRenderDrawColor(render, white.r, white.g, white.b, 0);
    SDL_RenderDrawLines(render, box_1->box, 5);

    SDL_Surface* surfaceMessage = NULL;

    if (text != NULL) {
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

void draw_view( SDL_Renderer_Access render) {
  SDL_Rect rect = {.x = 0, .y = 0, .w = GRID_LENGTH, .h = GRID_LENGTH};
  SDL_SetRenderDrawColor(render, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(render);

  position.x = GRID_LENGTH * camera_1->player->base->Graph_Position.x;
  position.y = GRID_LENGTH * camera_1->player->base->Graph_Position.y;
  SDL_RenderCopy(render, camera_1->player->base->Mark->access, NULL, &(position));

  uint8_t used = character_use_pool->status->max_size - character_use_pool->status->current_size;
  for (int next = 1; next < used; next++) {
    Status_Access npc = &(character_use_pool->status->pool[next]);

    if (npc->base->status == IN_USE) {
      if (!Point.eq(&(npc->base->Real_Position), &(camera_1->player->base->Real_Position))) {
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
					  result->mark,
					  white);
    result->access = SDL_CreateTextureFromSurface(render, surfaceMessage);

    SDL_FreeSurface(surfaceMessage);

    result = Style_Pool_Interface.next(style_pool, &counter);
  }

  result = Style_Pool_Interface.find(style_pool, "player");
  character.set_style(camera_1->player, result);

  Style_Access dead = Style_Pool_Interface.find(style_pool, "dead");
  camera.set_dead_style(camera_1, dead);
  return EXECUTE_SUCCESS;
}

void submain(char *root_dir, char *init_cfg, char *npc_cfg) {
  CONF_PATH = root_dir;
  Execute_Result result = EXECUTE_FAILED;
  srand(time(NULL));

  SDL_Window_Access win;
  SDL_Renderer_Access render;
  bool running = true;

  config_pool = string_pool.start(1000);
  style_pool = Style_Pool_Interface.start(256);
  character_prepare_pool = character_pool.start(20);
  character_use_pool = character_pool.start(100);
  camera_1 = camera.start();
  box_1 = message_box.start();

  SDL_Init(SDL_INIT_EVERYTHING);
  if (TTF_Init() != 0) {
    goto INIT_FAILED;
  }
  Status_Access Player = character_pool.malloc(character_use_pool);
  camera.set_player(camera_1, Player);

  result = setup_style(init_cfg);

  if (result == EXECUTE_FAILED) {
    goto DONE;
  }

  setup_npc(npc_cfg);

  camera.set_map(camera_1, &map_1);
  use_npc("goblin", "random-name", camera_1->map);

  win = SDL_CreateWindow(GAME_TITLE, 0, 0, WIDTH, HEIGHT, SDL_WINDOW_OPENGL);
  render = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED|SDL_RENDERER_PRESENTVSYNC);

  if (init_view(render) != EXECUTE_SUCCESS) {
    goto INIT_FAILED;
  }

  while (running) {
    SDL_Event event;

    while (SDL_PollEvent(&event)) {
      switch (event.type) {
	case SDL_QUIT:
	  running = false;
	  break;
	case SDL_KEYDOWN:
	  running = camera.take(camera_1, character_use_pool, box_1, &event);
	  break;
	default:
	  break;
      }
      draw_view(render);
    }
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
  character_pool.stop(character_use_pool);
  character_pool.stop(character_prepare_pool);
  Style_Pool_Interface.stop(style_pool);
  string_pool.stop(config_pool);
}
