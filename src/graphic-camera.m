#include "graphic-camera.h"

static int MAX_X = 0;
static int MAX_Y = 0;


static void print_mode(char *str, Camera_Mode mode) {
  printf("%s", str);
  switch (mode) {
    case CAMERA_MOVE:
      printf("MOVE\n");
      break;
    default:
      printf("FIX\n");
      break;
  }
}

static void print_horizon_mode(Camera_Access access) {
  print_mode("horizon: ", access->horizon);
}

static void print_vertical_mode(Camera_Access access) {
  print_mode("vertical: ", access->horizon);
}


static Yes_No occupy_position_by_others(Character_Pool_Access access,
    Point_Access point, Status_Access *result) {
  if ([access find_character: result with_position: point] == FOUND) {
    return YES;
  }
  else {
    return NO;
  }
}


static void debug(Camera_Access access) {
  Status_Access Player = access->player;
  Point_Access point = Player->base->Real_Position;

  Point_Access center = access->center;

  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;
  Yes_No result = YES;

  printf("------------------------\n");
  printf("[map](%d, %d)\n", [map get_end_x], [map get_end_y]);
  printf("[center](%d, %d)\n", center.x, center.y);
  printf("[start](%d, %d)\n", from.x, from.y);
  printf("[end](%d, %d)\n", to.x, to.y);
  printf("[position](%d, %d)\n", point.x, point.y);
  printf("[left check] %d\n",
      (point.x > (center.x - 1)));
  printf("[right check] %d\n",
      ((point.x + 1) < ([map get_end_x] - (center.x - 1))));
  printf("[from end] %d\n",
      (from.x <= 0));
  printf("[to end] %d\n",
      (to.x >= [map get_end_x]));
  printf("[up check] %d\n",
      (point.y > (center.y - 1)));
  printf("[down check] %d\n",
      ((point.y + 1) < ([map get_end_y] - (center.y - 1))));
  printf("[from end] %d\n",
      (from.y <= 0));
  printf("[to end] %d\n",
      (to.y >= [map get_end_y]));
}


static Yes_No can_move_horizon(Camera_Access access,
    Point_Access point) {
  Point_Access center = access->center;

  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;
  Yes_No result = YES;

  if (([point x] > ([center x] - 1))
      && ((point.x + 1)< ([map get_end_x] - (center.x - 1)))) {
    access->horizon = CAMERA_MOVE;
  }
  else if (from.x <= 0) {
    access->horizon = CAMERA_FIX;

    if (point.x < 0) {
      result = NO;
    }
  }
  else if (to.x >= [map get_end_x]) {
    access->horizon = CAMERA_FIX;

    if (point.x >= [map get_end_x]) {
      result = NO;
    }
  }
  else {
    access->horizon = CAMERA_MOVE;
  }
  return result;
}

static Yes_No can_move_vertical(Camera_Access access,
    Point_Access point) {
  Point_Access center = access->center;

  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;
  Yes_No result = YES;

  if (([point y] > ([center y] - 1))
      && ((point.y + 1)< ([map get_end_y] - (center.y - 1)))) {
    access->vertical = CAMERA_MOVE;
  }
  else if (from.y <= 0) {
    access->vertical = CAMERA_FIX;

    if (point.y < 0) {
      result = NO;
    }
  }
  else if (to.y >= [map get_end_y]) {
    access->vertical = CAMERA_FIX;

    if (point.y >= [map get_end_y]) {
      result = NO;
    }
  }
  else {
    access->vertical = CAMERA_MOVE;
  }
  return result;
}

static bool key_process(Camera_Access access, Character_Pool_Access from_pool,
    Message_Box_Access box_access, SDL_Event *event) {
  Status_Access Player = access->player;
  Status_Access npc = NULL;
  Style_Access dead = access->dead;

  Rectangle_Access rectangle = [Rectangle_Type create];
  Point_Access max_point = [Point_Type create];
  [max_point setX: access->max_x];
  [max_point setY: access->max_y];

  Point_Access point = [Point_Type create];
  [point setX: [Player->base->Real_Position x]];
  [point setY: [Player->base->Real_Position y]];

  Point_Access vector = [Point_Type create];

  if ((event->key).keysym.sym == SDLK_q) {
    return false;
  }

  switch ((event->key).keysym.sym) {
    case SDLK_UP:
      [point addY: -1];
      [vector setY: -1];
      break;
    case SDLK_DOWN:
      [point addY: 1];
      [vector setY: 1];
      break;
    case SDLK_LEFT:
      [point addX: -1];
      [vector setX: -1];
      break;
    case SDLK_RIGHT:
      [point addX: 1];
      [vector setX: 1];
      break;
    default:
      break;
  }

  if (occupy_position_by_others(from_pool, point, &npc) == NO) {
    if (([vector y] != 0) && (can_move_vertical(access, point) == YES)) {
      switch (access->vertical) {
        case CAMERA_MOVE:
          [access->start addY: [vector y]];
          [access->end addY: [vector y]];
          break;
        default:
          break;
      }

      [Player->base->Real_Position addX: [vector x]];
      [Player->base->Real_Position addY: [vector y]];
    }
    else if (([vector x] != 0) && (can_move_horizon(access, point) == YES)) {
      switch (access->horizon) {
        case CAMERA_MOVE:
          [access->start addX: [vector x]];
          [access->end addX: [vector x]];
          break;
        default:
          break;
      }

      [Player->base->Real_Position addX: [vector x]];
      [Player->base->Real_Position addY: [vector y]];
    }
  }
  else {
    NSString *message = NULL;

    if (character.attack(Player, npc) == DEAD) {
      character.set_style(npc, dead);

      message = [NSString stringWithFormat: @"%@ %@", npc->base->name, @"死亡"];
    }
    else {
      message = [NSString stringWithFormat: @"%@ %@%@", @"攻擊", npc->base->name, @"，造成 1 點傷害"];
    }
    message_box.add(box_access, message);
  }

  [rectangle set_top_left_point: access->start];
  [rectangle set_down_right_point: max_point];
  [from_pool calculate_graph_position: rectangle];
  return true;
}


static Camera_Access camera_start(void) {
  Camera_Access access = calloc(1, sizeof(Camera_Type));
  MAX_X = 25;
  MAX_Y = 21;
  access->start = [Point_Type create];
  access->end = [Point_Type create];
  access->center = [Point_Type create];

  camera.set_max_x(access, MAX_X);
  camera.set_max_y(access, MAX_Y);

  [access->start setX: 0];
  [access->start setY: 0];
  [access->end setX: MAX_X - 1];
  [access->end setY: MAX_Y - 1];

  access->horizon = CAMERA_UNDEFINE;
  access->vertical = CAMERA_UNDEFINE;
  access->map = NULL;
  return access;
}


static void camera_stop(Camera_Access access) {
  free(access);
}


static void camera_set_max_x(Camera_Access access, int x) {
  access->max_x = x;
  [access->center setX: (x - 1) / 2];
  MAX_X = x;
}

static void camera_set_max_y(Camera_Access access, int y) {
  access->max_y = y;
  [access->center setY: (y - 1) / 2];
  MAX_Y = y;
}


static void set_player(Camera_Access access, Status_Access player) {
  Point_Access center = access->center;

  [player->base->Real_Position setX: [center x]];
  [player->base->Real_Position setY: [center y]];

  [player->base->Graph_Position setX: [center x]];
  [player->base->Graph_Position setY: [center y]];

  access->player = player;
}


static void set_map(Camera_Access access, Map_Access map) {
  [map add_end_x: -1 and_y: -1];

  access->map = map;
}


static void set_dead_style(Camera_Access access, Style_Access style) {
  access->dead = style;
}


Graphic_Camera_API camera = {
  .start = camera_start,
  .stop = camera_stop,

  .set_player = set_player,
  .set_map = set_map,

  .set_max_x = camera_set_max_x,
  .set_max_y = camera_set_max_y,
  .set_dead_style = set_dead_style,

  .take = key_process
};
