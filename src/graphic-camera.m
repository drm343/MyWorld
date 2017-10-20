#include "graphic-camera.h"

#define MAP(name) Map_Type_##name


static int MAX_X = 0;
static int MAX_Y = 0;


#ifdef DEBUG
static void print_mode(char *str, Camera_Mode mode)
{
    switch (mode) {
        case CAMERA_MOVE:
            DEBUG_PRINT("%s MOVE\n", str);
            break;
        default:
            DEBUG_PRINT("%s FIX\n", str);
            break;
    }
}

static void print_horizon_mode(Camera_Access access)
{
    print_mode("horizon: ", access->horizon);
}

static void print_vertical_mode(Camera_Access access)
{
    print_mode("vertical: ", access->horizon);
}
#endif


static Yes_No occupy_position_by_others(Character_Pool_Access access,
                                        Point_Access point,
                                        Status_Access * result)
{
#define POOL(function) Character_Pool_##function

    if (POOL(find_character) (access, result, point) == FOUND) {
        return YES;
    } else {
        return NO;
    }
#undef POOL
}


// --------------------------------------------------
// Setup camera mode
// --------------------------------------------------
static void camera_horizon_mode_setup(Camera_Access access,
                                      Point_Access point, int32_t x)
{
    Point_Access center = access->center;

    Point_Access from = access->start;
    Point_Access to = access->end;

    Map_Access map = access->map;
    int32_t map_end_x = MAP(bottom_right_x)(map);

    if ((point->x > (center->x - 1))
        && ((point->x + 1) < (map_end_x - (center->x - 1)))) {
        access->horizon = CAMERA_MOVE;

        Point_Type_add_x(access->start, x);
        Point_Type_add_x(access->end, x);
    } else if (from->x <= 0) {
        access->horizon = CAMERA_FIX;
    } else if (to->x >= map_end_x) {
        access->horizon = CAMERA_FIX;
    } else {
        access->horizon = CAMERA_MOVE;

        Point_Type_add_x(access->start, x);
        Point_Type_add_x(access->end, x);
    }
}


static void camera_vertical_mode_setup(Camera_Access access,
                                       Point_Access point, int32_t y)
{
    Point_Access center = access->center;

    Point_Access from = access->start;
    Point_Access to = access->end;

    Map_Access map = access->map;
    int32_t map_end_y = MAP(bottom_right_y)(map);

    if ((point->y > (center->y - 1))
        && ((point->y + 1) < (map_end_y - (center->y - 1)))) {
        access->vertical = CAMERA_MOVE;

        Point_Type_add_y(access->start, y);
        Point_Type_add_y(access->end, y);
    } else if (from->y <= 0) {
        access->vertical = CAMERA_FIX;
    } else if (to->y >= map_end_y) {
        access->vertical = CAMERA_FIX;
    } else {
        access->vertical = CAMERA_MOVE;

        Point_Type_add_y(access->start, y);
        Point_Type_add_y(access->end, y);
    }
}


// --------------------------------------------------
// Check move status
// --------------------------------------------------
static Yes_No can_move_horizon(Camera_Access access, Point_Access point)
{
    Point_Access from = access->start;
    Point_Access to = access->end;

    Map_Access map = access->map;
    int32_t map_end_x = MAP(bottom_right_x)(map);
    Yes_No result = YES;

    if ((from->x <= 0) && (point->x < 0)) {
        result = NO;
    } else if ((to->x >= map_end_x)
               && (point->x >= map_end_x)) {
        result = NO;
    }
    return result;
}


static Yes_No can_move_vertical(Camera_Access access, Point_Access point)
{
    Point_Access from = access->start;
    Point_Access to = access->end;

    Map_Access map = access->map;
    int32_t map_end_y = MAP(bottom_right_y)(map);
    Yes_No result = YES;

    if ((from->y <= 0) && (point->y < 0)) {
        result = NO;
    } else if ((to->y >= map_end_y) && (point->y >= map_end_y)) {
        result = NO;
    }
    return result;
}


static bool message_process(Camera_Access access,
                            Character_Pool_Access from_pool,
                            Message_Box_Access box_access,
                            Status_Access current, Message_Type message)
{
    Status_Access npc = NULL;
    Style_Access dead = access->dead;

    Rectangle_Access rectangle = Rectangle_Type_create();
    Point_Access max_point = Point_Type_create();
    Point_Access_change(max_point);
    Point_Access_set_x(access->max_x);
    Point_Access_set_y(access->max_y);

    Point_Access point = Point_Type_create();
    Point_Access_change(point);
    Point_Access_set_x(Point_Type_x(current->base->Real_Position));
    Point_Access_set_y(Point_Type_y(current->base->Real_Position));

    Point_Access vector = Point_Type_create();

    switch (message) {
        case TOP:
            Point_Type_add_y(point, -1);
            Point_Type_set_y(vector, -1);
            break;
        case DOWN:
            Point_Type_add_y(point, 1);
            Point_Type_set_y(vector, 1);
            break;
        case LEFT:
            Point_Type_add_x(point, -1);
            Point_Type_set_x(vector, -1);
            break;
        case RIGHT:
            Point_Type_add_x(point, 1);
            Point_Type_set_x(vector, 1);
            break;
        default:
            goto DONE;
            break;
    }

    Found_Result result =
        occupy_position_by_others(from_pool, point, &npc);
    if (result == NO) {
        int32_t y = Point_Type_y(vector);

        if ((y != 0) && (can_move_vertical(access, point) == YES)) {
            if (current->faction == FACTION_PLAYER) {
                camera_vertical_mode_setup(access, point, y);
            }

            Point_Access_change(current->base->Real_Position);
            Point_Access_add_x(Point_Type_x(vector));
            Point_Access_add_y(y);
        } else if ((Point_Type_x(vector) != 0)
                   && (can_move_horizon(access, point) == YES)) {
            if (current->faction == FACTION_PLAYER) {
                camera_horizon_mode_setup(access, point,
                                          Point_Type_x(vector));
            }

            Point_Access_change(current->base->Real_Position);
            Point_Access_add_x(Point_Type_x(vector));
            Point_Access_add_y(Point_Type_y(vector));
        }
    } else {
        Is_Alive is_alive = ALIVE;

        char *format = "%s(%s) 攻擊 %s(%s),造成 1 點傷害";
        int counter = snprintf(NULL, 0, format,
                               current->base->name,
                               character.get_relation_string(current),
                               npc->base->name,
                               character.get_relation_string(npc));

        char attack_message[counter];
        snprintf(attack_message, counter + 1, format,
                 current->base->name,
                 character.get_relation_string(current),
                 npc->base->name, character.get_relation_string(npc));
        message_box.add(box_access, attack_message);

#define PKG(function) Character_Pool_##function
        switch (npc->faction) {
            case FACTION_PLAYER:
                is_alive = PKG(attack_player_by) (from_pool, current);
                break;
            case FACTION_ALLY:
                is_alive = PKG(attack_ally_by) (from_pool, current, npc);
                break;
            case FACTION_ENEMY:
                is_alive = PKG(attack_enemy_by) (from_pool, current, npc);
                break;
            case FACTION_NEUTRAL:
                is_alive =
                    PKG(attack_neutral_by) (from_pool, current, npc);
                break;
            default:
                break;
        }
#undef PKG

        if (is_alive == DEAD) {
            character.set_style(npc, dead);

            counter = String_ascii_length(npc->base->name);

            if (npc->faction == FACTION_PLAYER) {
                format = " 已死亡，任意按鍵離開遊戲";
                counter = counter + String_ascii_length(format);
            } else {
                format = " 死亡";
                counter = counter + String_ascii_length(format);
            }
            char death_message[counter];
            snprintf(death_message, counter + 1, "%s%s",
                     npc->base->name, format);
            message_box.add(box_access, death_message);
        }
    }

    Rectangle_Access_change(rectangle);
    Rectangle_Access_set_top_left_point(access->start);
    Rectangle_Access_set_down_right_point(max_point);

    Character_Pool_calculate_graph_position(from_pool, rectangle);

  DONE:
    Point_Type_free(vector);
    Point_Type_free(point);
    Point_Type_free(max_point);
    Rectangle_Type_free(rectangle);
    return true;
}


static Camera_Access camera_start(void)
{
    Camera_Access access = calloc(1, sizeof(Camera_Type));
    MAX_X = 25;
    MAX_Y = 21;
    access->start = Point_Type_create();
    access->end = Point_Type_create();
    access->center = Point_Type_create();

    camera.set_max_x(access, MAX_X);
    camera.set_max_y(access, MAX_Y);

    Point_Access_change(access->start);
    Point_Access_set_x(0);
    Point_Access_set_y(0);

    Point_Access_change(access->end);
    Point_Access_set_x(MAX_X - 1);
    Point_Access_set_y(MAX_Y - 1);

    access->horizon = CAMERA_UNDEFINE;
    access->vertical = CAMERA_UNDEFINE;
    access->map = NULL;
    return access;
}


static void camera_stop(Camera_Access access)
{
    Point_Type_free(access->start);
    Point_Type_free(access->end);
    Point_Type_free(access->center);
    free(access);
}


static void camera_set_max_x(Camera_Access access, int x)
{
    access->max_x = x;
    Point_Type_set_x(access->center, (x - 1) / 2);
    MAX_X = x;
}

static void camera_set_max_y(Camera_Access access, int y)
{
    access->max_y = y;
    Point_Type_set_y(access->center, (y - 1) / 2);
    MAX_Y = y;
}


static void set_player(Camera_Access access, Status_Access player)
{
    Point_Access center = access->center;
    int32_t x = Point_Type_x(center);
    int32_t y = Point_Type_y(center);

    Point_Access_change(player->base->Real_Position);
    Point_Access_set_x(x);
    Point_Access_set_y(y);

    Point_Access_change(player->base->Graph_Position);
    Point_Access_set_x(x);
    Point_Access_set_y(y);

    access->player = player;
}


static void set_map(Camera_Access access, Map_Access map)
{
    MAP(move_bottom_right)(map, -1, -1);
    access->map = map;
}


static void set_dead_style(Camera_Access access, Style_Access style)
{
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

#undef MAP