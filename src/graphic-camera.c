#include "graphic-camera.h"


/** @brief Namespace CAMERA
 */
#define EXPORT(name) CAMERA(name)

static int MAX_X = 0;
static int MAX_Y = 0;
static Rectangle rectangle = NULL;


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

static Yes_No occupy_position_by_others(Game_Status_Access access,
                                        Point point,
                                        Character_Access * result)
{
    if (GAME(find_character) (access, result, point) == FOUND) {
        return YES;
    } else {
        return NO;
    }
}

//--------------------------------------------------
//Setup camera mode
// --------------------------------------------------
static void
camera_horizon_mode_setup(Camera_Access access, Point point, int32_t x)
{
    Point center = access->center;

    Point from = access->start;
    Point to = access->end;

    Map_Access map = access->map;
    int32_t map_end_x = MAP(bottom_right_x) (map);

    if ((point->x > (center->x - 1))
        && ((point->x + 1) < (map_end_x - (center->x - 1)))) {
        access->horizon = CAMERA_MOVE;

        Point_move(access->start,.x = x);
        Point_move(access->end,.x = x);
    } else if (from->x <= 0) {
        access->horizon = CAMERA_FIX;
    } else if (to->x >= map_end_x) {
        access->horizon = CAMERA_FIX;
    } else {
        access->horizon = CAMERA_MOVE;

        Point_move(access->start,.x = x);
        Point_move(access->end,.x = x);
    }
}

static void
camera_vertical_mode_setup(Camera_Access access, Point point, int32_t y)
{
    Point center = access->center;

    Point from = access->start;
    Point to = access->end;

    Map_Access map = access->map;
    int32_t map_end_y = MAP(bottom_right_y) (map);

    if ((point->y > (center->y - 1))
        && ((point->y + 1) < (map_end_y - (center->y - 1)))) {
        access->vertical = CAMERA_MOVE;

        Point_move(access->start,.y = y);
        Point_move(access->end,.y = y);
    } else if (from->y <= 0) {
        access->vertical = CAMERA_FIX;
    } else if (to->y >= map_end_y) {
        access->vertical = CAMERA_FIX;
    } else {
        access->vertical = CAMERA_MOVE;

        Point_move(access->start,.y = y);
        Point_move(access->end,.y = y);
    }
}

//--------------------------------------------------
//Check move status
// --------------------------------------------------
static Yes_No can_move_horizon(Camera_Access access, Point point)
{
    Point from = access->start;
    Point to = access->end;

    Map_Access map = access->map;
    int32_t map_end_x = MAP(bottom_right_x) (map);
    Yes_No result = YES;

    if ((from->x <= 0) && (point->x < 0)) {
        result = NO;
    } else if ((to->x >= map_end_x)
               && (point->x >= map_end_x)) {
        result = NO;
    }
    return result;
}

static Yes_No can_move_vertical(Camera_Access access, Point point)
{
    Point from = access->start;
    Point to = access->end;

    Map_Access map = access->map;
    int32_t map_end_y = MAP(bottom_right_y) (map);
    Yes_No result = YES;

    if ((from->y <= 0) && (point->y < 0)) {
        result = NO;
    } else if ((to->y >= map_end_y) && (point->y >= map_end_y)) {
        result = NO;
    }
    return result;
}

//---------------------------------------------------
//Export API
// ---------------------------------------------------
/** @brief 建立新的 Camera 物件
 * @return Camera 物件
 *
 * MAX_X 的預設值為 25，MAX_Y 的預設值為 21，可透過 EXPORT(set_max_x) 跟 EXPORT(set_max_y)
 * 來修改。
*/
Camera_Access EXPORT(start) (void) {
    Camera_Access access = calloc(1, sizeof(Camera_Type));
    MAX_X = 25;
    MAX_Y = 21;
    access->start = Point_create();
    access->end = Point_create();
    access->center = Point_create();

    EXPORT(set_max) (access,.x = MAX_X,.y = MAX_Y);

    Point_set(access->start, 0, 0);
    Point_set(access->end,.x = MAX_X - 1,.y = MAX_Y - 1);

    access->horizon = CAMERA_UNDEFINE;
    access->vertical = CAMERA_UNDEFINE;
    access->map = NULL;
    rectangle = RECT(create) ();
    return access;
}

/** @brief 釋放 Camera 物件
 * @param self 要釋放的角色物件
*/
void EXPORT(stop) (Camera_Access self) {
    RECT(free) (rectangle);
    Point_free(self->start);
    Point_free(self->end);
    Point_free(self->center);
    free(self);
}

/** @brief 重設螢幕顯示最大值
 * @param self Camera 物件
 * @param other 要設定的數值
*/
void EXPORT(set_max_by_point) (Camera_Access self, Point other) {
    int x = other->x;
    int y = other->y;

    self->max_x = x;
    self->max_y = y;
    Point_set(self->center,.x = (x - 1) / 2,.y = (y - 1) / 2);
    MAX_X = x;
    MAX_Y = y;
}

/** @brief 設定玩家角色
 * @param self Camera 物件
 * @param player 指定的玩家物件
 *
 * @warning 初始化時會將真實座標直接當成圖形座標，因為初始座標是固定在螢幕中間的點，這個點會固定不變，之後會修改
*/
void EXPORT(set_player) (Camera_Access self, Character_Access player) {
    Point center = self->center;
    int32_t x = Point_x(center);
    int32_t y = Point_y(center);

    Point_set(player->Real_Position,.x = x,.y = y);
    Point_set(player->Graph_Position,.x = x,.y = y);

    self->player = player;
}

/** @brief 設定死亡時顯示的圖形
 * @param self Camera 物件
 * @param dead 指定的圖形
*/
void EXPORT(set_dead_style) (Camera_Access self, Style_Access dead) {
    self->dead = dead;
}

/** @brief 設定地圖物件
 * @param self Camera 物件
 * @param map 指定的地圖物件
*/
void EXPORT(set_map) (Camera_Access self, Map_Access map) {
    MAP(move_bottom_right) (map, -1, -1);
    self->map = map;
}

/** @brief 處理訊息並更改角色資料
 * @param self Camera 物件
 * @param from_pool 角色池
 * @param box_access 訊息視窗
 * @param current 當前發出訊息的角色
 * @param message 角色發出的訊息
 * @return 當前必定回傳 true
*/
bool
EXPORT(take) (Camera_Access self,
              Game_Status_Access from_pool,
              Message_Box_Access box_access,
              Character_Access current, Message_Type message) {
    Character_Access npc = NULL;
    Style_Access dead = self->dead;

    struct Point raw_max_point;
    Point max_point = &raw_max_point;
    Point_set(max_point,.x = self->max_x,.y = self->max_y);

    struct Point raw_point;
    Point point = &raw_point;
    Point_set_by_point(point, current->Real_Position);

    struct Point raw_vector;
    Point vector = &raw_vector;

    switch (message) {
        case TOP:
            Point_move(point,.y = -1);
            Point_set(vector,.y = -1);
            break;
        case DOWN:
            Point_move(point,.y = 1);
            Point_set(vector,.y = 1);
            break;
        case LEFT:
            Point_move(point,.x = -1);
            Point_set(vector,.x = -1);
            break;
        case RIGHT:
            Point_move(point,.x = 1);
            Point_set(vector,.x = 1);
            break;
        default:
            goto DONE;
            break;
    }

    Yes_No result = occupy_position_by_others(from_pool, point, &npc);
    if (result == NO) {
        int32_t y = Point_y(vector);
        Faction_Type current_relation;
        Status_get_relation(current->status, current_relation);

        if ((y != 0) && (can_move_vertical(self, point) == YES)) {
            if (current_relation == FACTION_PLAYER) {
                camera_vertical_mode_setup(self, point, y);
            }
            Point_move(current->Real_Position,.x = Point_x(vector),.y = y);
        } else if ((Point_x(vector) != 0)
                   && (can_move_horizon(self, point) == YES)) {
            if (current_relation == FACTION_PLAYER) {
                camera_horizon_mode_setup(self, point, Point_x(vector));
            }
            Point_move_by_point(current->Real_Position, vector);
        }
    } else {
        Is_Alive is_alive = ALIVE;

        char *attack_message;
        {
            int len = snprintf(NULL, 0,
                               "%s(%s) 攻擊 %s(%s) 造成 1 點傷害",
                               current->status->name->str,
                               STATUS(get_relation_string) (current->
                                                            status),
                               npc->status->name->str,
                               STATUS(get_relation_string) (npc->status));
            attack_message = malloc((len + 1) * sizeof(char));
            snprintf(attack_message, len + 1,
                     "%s(%s) 攻擊 %s(%s) 造成 1 點傷害",
                     current->status->name->str,
                     STATUS(get_relation_string) (current->status),
                     npc->status->name->str,
                     STATUS(get_relation_string) (npc->status));
        }

        BOX(add_message) (box_access, attack_message);
        free(attack_message);
        Faction_Type npc_relation;
        Status_get_relation(npc->status, npc_relation);

        switch (npc_relation) {
            case FACTION_PLAYER:
                is_alive = GAME(attack_player_by) (from_pool, current);
                break;
            case FACTION_ALLY:
                is_alive = GAME(attack_ally_by) (from_pool, current, npc);
                break;
            case FACTION_ENEMY:
                is_alive = GAME(attack_enemy_by) (from_pool, current, npc);
                break;
            case FACTION_NEUTRAL:
                is_alive =
                    GAME(attack_neutral_by) (from_pool, current, npc);
                break;
            default:
                break;
        }

        if (is_alive == DEAD) {
            CHARA(set_style) (npc, dead);

            const char *format = NULL;
            if (npc->status->faction == FACTION_PLAYER) {
                format = " 已死亡，任意按鍵離開遊戲";
            } else {
                format = " 死亡";
            }

            char *death_message;
            {
                int len = snprintf(NULL, 0, "%s%s",
                                   npc->status->name->str,
                                   format);
                death_message = malloc((len + 1) * sizeof(char));
                snprintf(NULL, 0, "%s%s", npc->status->name->str, format);
            }
            BOX(add_message) (box_access, death_message);
            free(death_message);
        }
    }

    RECT(set_position) (rectangle, self->start);
    RECT(set_extent) (rectangle, max_point);

    GAME(calculate_graph_position) (from_pool, rectangle);

  DONE:
    return true;
}

#undef EXPORT
