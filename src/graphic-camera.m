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


// --------------------------------------------------
// Setup camera mode
// --------------------------------------------------
static void camera_horizon_mode_setup(Camera_Access access,
    Point_Access point,
    int32_t x) {
  Point_Access center = access->center;

  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;

  if (([point x] > ([center x] - 1))
      && (([point x] + 1) < ([map get_end_x] - ([center x] - 1)))) {
    access->horizon = CAMERA_MOVE;

    [access->start addX: x];
    [access->end addX: x];
  }
  else if ([from x] <= 0) {
    access->horizon = CAMERA_FIX;
  }
  else if ([to x] >= [map get_end_x]) {
    access->horizon = CAMERA_FIX;
  }
  else {
    access->horizon = CAMERA_MOVE;

    [access->start addX: x];
    [access->end addX: x];
  }
}


static void camera_vertical_mode_setup(Camera_Access access,
    Point_Access point,
    int32_t y) {
  Point_Access center = access->center;

  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;

  if (([point y] > ([center y] - 1))
      && (([point y] + 1) < ([map get_end_y] - ([center y] - 1)))) {
    access->vertical = CAMERA_MOVE;

    [access->start addY: y];
    [access->end addY: y];
  }
  else if ([from y] <= 0) {
    access->vertical = CAMERA_FIX;
  }
  else if ([to y] >= [map get_end_y]) {
    access->vertical = CAMERA_FIX;
  }
  else {
    access->vertical = CAMERA_MOVE;

    [access->start addY: y];
    [access->end addY: y];
  }
}


// --------------------------------------------------
// Check move status
// --------------------------------------------------
static Yes_No can_move_horizon(Camera_Access access,
    Point_Access point) {
  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;
  Yes_No result = YES;

  if ((from.x <= 0) && (point.x < 0)) {
    result = NO;
  }
  else if ((to.x >= [map get_end_x])
   && (point.x >= [map get_end_x])) {
    result = NO;
  }
  return result;
}


static Yes_No can_move_vertical(Camera_Access access,
    Point_Access point) {
  Point_Access from = access->start;
  Point_Access to = access->end;

  Map_Access map = access->map;
  Yes_No result = YES;

  if ((from.y <= 0) && (point.y < 0)) {
    result = NO;
  }
  else if ((to.y >= [map get_end_y]) && (point.y >= [map get_end_y])) {
    result = NO;
  }
  return result;
}


static bool message_process(Camera_Access access,
                            Character_Pool_Access from_pool,
                            Message_Box_Access box_access,
                            Status_Access current,
                            Message_Type message) {
  Status_Access npc = NULL;
  Style_Access dead = access->dead;

  Rectangle_Access rectangle = [Rectangle_Type create];
  Point_Access max_point = [Point_Type create];
  [max_point setX: access->max_x];
  [max_point setY: access->max_y];

  Point_Access point = [Point_Type create];
  [point setX: [current->base->Real_Position x]];
  [point setY: [current->base->Real_Position y]];

  Point_Access vector = [Point_Type create];

  switch (message) {
    case TOP:
      [point addY: -1];
      [vector setY: -1];
      break;
    case DOWN:
      [point addY: 1];
      [vector setY: 1];
      break;
    case LEFT:
      [point addX: -1];
      [vector setX: -1];
      break;
    case RIGHT:
      [point addX: 1];
      [vector setX: 1];
      break;
    default:
      return true;
      break;
  }

  Found_Result result = occupy_position_by_others(from_pool, point, &npc);
  if (result == NO) {
    if (([vector y] != 0) && (can_move_vertical(access, point) == YES)) {
      if (current->faction == FACTION_PLAYER) {
        camera_vertical_mode_setup(access, point, [vector y]);
      }

      [current->base->Real_Position addX: [vector x]];
      [current->base->Real_Position addY: [vector y]];
    }
    else if (([vector x] != 0)
        && (can_move_horizon(access, point) == YES)) {
      if (current->faction == FACTION_PLAYER) {
        camera_horizon_mode_setup(access, point, [vector x]);
      }

      [current->base->Real_Position addX: [vector x]];
      [current->base->Real_Position addY: [vector y]];
    }
  }
  else {
    NSString *box_message = NULL;
    Is_Alive is_alive = ALIVE;

    box_message = [NSString stringWithFormat: @"%@(%@) 攻擊 %@(%@)，造成 1 點傷害",
                  current->base->name,
                  character.get_relation_string(current),
                  npc->base->name,
                  character.get_relation_string(npc)];
    message_box.add(box_access, box_message);

    switch (npc->faction) {
      case FACTION_PLAYER:
        is_alive = [from_pool attack_player_by: current];
        break;
      case FACTION_ALLY:
        is_alive = [from_pool attack_ally_by: current with_target: npc];
        break;
      case FACTION_ENEMY:
        is_alive = [from_pool attack_enemy_by: current with_target: npc];
        break;
      case FACTION_NEUTRAL:
        is_alive = [from_pool attack_neutral_by: current with_target: npc];
        break;
      default:
        break;
    }

    if (is_alive == DEAD) {
      character.set_style(npc, dead);

      if (npc->faction == FACTION_PLAYER) {
        box_message = [NSString stringWithFormat: @"%@ 已死亡，任意按鍵離開遊戲", npc->base->name];
      }
      else {
        box_message = [NSString stringWithFormat: @"%@ 死亡", npc->base->name];
      }
      message_box.add(box_access, box_message);
    }
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

  .take = message_process
};
