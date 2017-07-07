#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "setup_config.h"
#include "mpc.h"

#include "string_pool.h"
#include "character-ability.h"
#include "character-pool.h"
#include "graphic.h"

#include "main.h"

ACCESS_TYPE(SDL_Window, SDL_Window_Access);
ACCESS_TYPE(SDL_Renderer, SDL_Renderer_Access);
ACCESS_TYPE(SDL_Event, SDL_Event_Access);

int GRID_LENGTH = 24;
int MAX_COUNT_X = 25;
int MAX_COUNT_y = 20;
SDL_Rect position = {
  .x = 0,
  .y = 0,
  .w = 24,
  .h = 24 
};

void set_player_center(void) {
  int CENTER_X = (MAX_COUNT_X + 1) / 2;
  int CENTER_Y = MAX_COUNT_y / 2;

  Player->Graph_Position.x =  CENTER_X;
  Player->Real_Position.x = CENTER_X;

  Player->Graph_Position.y = CENTER_Y;
  Player->Real_Position.y = CENTER_Y;
}

bool key_process(SDL_Event_Access event) {
  bool can_move_left  = true;
  bool can_move_right = true;
  bool can_move_up    = true;
  bool can_move_down  = true;

  Point_Type origin_point = {
    Player->Real_Position.x,
    Player->Real_Position.y
  };

  Point_Type point = {
    origin_point.x - 1,
    origin_point.y
  };

  if ((Player->Graph_Position.x < 1) ||
      (character_use_pool->find_position(character_use_pool, &point))) {
    can_move_left = false;
  }

  point.x = origin_point.x + 1;
  point.y = origin_point.y;

  if ((Player->Graph_Position.x > 23) ||
      (character_use_pool->find_position(character_use_pool, &point))) {
    can_move_right = false;
  }

  point.x = origin_point.x;
  point.y = origin_point.y - 1;

  if ((Player->Graph_Position.y < 1) ||
      (character_use_pool->find_position(character_use_pool, &point))) {
    can_move_up = false;
  }

  point.x = origin_point.x;
  point.y = origin_point.y + 1;

  if ((Player->Graph_Position.y > 18) ||
      (character_use_pool->find_position(character_use_pool, &point))) {
    can_move_down = false;
  }

  switch (ACCESS_FIELD(event, key).keysym.sym) {
    case SDLK_UP:
      if (can_move_up) {
        Player->Graph_Position.y = Player->Graph_Position.y - 1;
        Player->Real_Position.y = Player->Real_Position.y - 1;
      }
      break;
    case SDLK_DOWN:
      if (can_move_down) {
        Player->Graph_Position.y = Player->Graph_Position.y + 1;
        Player->Real_Position.y = Player->Real_Position.y + 1;
      }
      break;
    case SDLK_LEFT:
      if (can_move_left) {
        Player->Graph_Position.x = Player->Graph_Position.x - 1;
        Player->Real_Position.x = Player->Real_Position.x - 1;
      }
      break;
    case SDLK_RIGHT:
      if (can_move_right) {
        Player->Graph_Position.x = Player->Graph_Position.x + 1;
        Player->Real_Position.x = Player->Real_Position.x + 1;
      }
      break;
    case SDLK_q:
      return false;
      break;
    default:
      printf("other\n");
      break;
  }
  return true;
}

void draw_view( SDL_Renderer_Access render) {
  SDL_Rect rect = {.x = 0, .y = 0, .w = GRID_LENGTH, .h = GRID_LENGTH};
  SDL_SetRenderDrawColor(render, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(render);

  for (int count = 0; count <= MAX_COUNT_X - 1; count++) {
    rect.x = count * GRID_LENGTH;
    rect.y = 0;

    SDL_SetRenderDrawColor(render, 255, 255, 255, SDL_ALPHA_OPAQUE);
    SDL_RenderDrawRect(render, ADDRESS(rect));
  }
  for (int count = 0; count <= MAX_COUNT_y - 1; count++) {
    rect.x = 0;
    rect.y = count * GRID_LENGTH;

    SDL_SetRenderDrawColor(render, 255, 255, 255, SDL_ALPHA_OPAQUE);
    SDL_RenderDrawRect(render, ADDRESS(rect));
  }

  position.x = GRID_LENGTH * Player->Graph_Position.x;
  position.y = GRID_LENGTH * Player->Graph_Position.y;
  SDL_RenderCopy(render, Player->Mark->access, NULL, ADDRESS(position));

  uint8_t used = character_use_pool->max_size - character_use_pool->current_size;
  for(int next = 1; next < used; next++) {
    Status_Access npc = &(character_use_pool->pool[next]);

    if (npc->status == IN_USE) {
      rect.x = GRID_LENGTH * npc->Graph_Position.x;
      rect.y = GRID_LENGTH * npc->Graph_Position.y;
      SDL_RenderCopy(render, npc->Mark->access, NULL, ADDRESS(rect));
    }
  }

  SDL_RenderPresent(render);
}

Execute_Result init_view(SDL_Renderer_Access render) {
  SDL_Color white = {255, 255, 255};

  TTF_Font* font = TTF_OpenFont(FONT_FAMILY, 512);
  if (!font) {
    printf("TTF_OpenFont: %s\n", TTF_GetError());
    return EXECUTE_FAILED;
  }

  SDL_Surface* surfaceMessage = NULL;
  uint8_t counter = 0;
  Style_Access result = Style_Pool_Interface.next(style_pool, &counter);

  while (result != NULL) {
    surfaceMessage = TTF_RenderUTF8_Solid(font,
        result->mark,
        white);
    SET_ACCESS_FIELD(result, access, SDL_CreateTextureFromSurface(render, surfaceMessage));

    SDL_FreeSurface(surfaceMessage);

    result = Style_Pool_Interface.next(style_pool, &counter);
  }
  TTF_CloseFont(font);

  result = Style_Pool_Interface.find(style_pool, "player");
  Player->set_style(Player, result);
  return EXECUTE_SUCCESS;
}

int main(int argc, char *argv[]) {
  SDL_Window_Access win;
  SDL_Renderer_Access render;
  bool running = true;

  String_Pool_start_stack(string_pool, 1000);
  style_pool = Style_Pool_Interface.start(256);
  character_pool = Character_Pool_start_heap(20);
  character_use_pool = Character_Pool_start_heap(100);

  Player = character_use_pool->malloc(character_use_pool);

  Ability_Access access = ADDRESS(Player->Ability);
  Ability.Set_Rank(access, 2);
  Ability.power->Set_Vary_Powerful(access);
  Ability.power->Set_Vary_Powerful(access);
  set_player_center();

  char *CONFIG_FILE = NULL;
  if (argc >= 2) {
    size_t path_lang = strlen(argv[1]);
    CONF_PATH = MAIN_STRING_malloc(path_lang);
    strcpy(CONF_PATH, argv[1]);

    char *CONFIG_NAME = "/config/init.css";
    CONFIG_FILE = use_path(CONFIG_NAME);
    setup_character_config_path(argv[1]);
  }
  else {
    goto DONE;
  }

  setup_style(CONFIG_FILE);
  setup_npc_by_dir();
  use_npc("goblin", "random-name");

  SDL_Init(SDL_INIT_EVERYTHING);
  if (TTF_Init() != 0) {
    goto INIT_FAILED;
  }

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
          running = key_process(&event);
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

INIT_FAILED:
  TTF_Quit();
  SDL_Quit();

DONE:
  character_use_pool->stop(character_use_pool);
  character_pool->stop(character_pool);
  Style_Pool_Interface.stop(style_pool);
  string_pool->stop(string_pool);
}
