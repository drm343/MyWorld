#include "graphic-camera.h"

#define MAP(name) Map_Type_##name
#define STATUS(name) Status_##name

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
    if (CP(find_character) (access, result, point) == FOUND) {
        return YES;
    } else {
        return NO;
    }
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
    int32_t map_end_x = MAP(bottom_right_x) (map);

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
    int32_t map_end_y = MAP(bottom_right_y) (map);

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


static Yes_No can_move_vertical(Camera_Access access, Point_Access point)
{
    Point_Access from = access->start;
    Point_Access to = access->end;

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



// ---------------------------------------------------
// Export API
// ---------------------------------------------------
/** @brief Namespace Graphic_Camera_
 *
 * 當使用 EXPORT 的函數時，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如 start 必須寫成 Graphic_Camera_start，如果外部程式要簡化呼叫，
 * 可以在程式中自行定義新的 macro，例如下面範例。
 *
 * #define G_C(name) Graphic_Camera_##name<br>
 * G_C(init)(self);
 */
#define EXPORT(name) Graphic_Camera_##name


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
    access->start = Point_Type_create();
    access->end = Point_Type_create();
    access->center = Point_Type_create();

    EXPORT(set_max_x) (access, MAX_X);
    EXPORT(set_max_y) (access, MAX_Y);

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


  /** @brief 釋放 Camera 物件
   * @param self 要釋放的角色物件
  */
void EXPORT(stop) (Camera_Access self) {
    Point_Type_free(self->start);
    Point_Type_free(self->end);
    Point_Type_free(self->center);
    free(self);
}


  /** @brief 重設 max_x 的值
   * @param self Camera 物件
   * @param x 要設定的 max 值
  */
void EXPORT(set_max_x) (Camera_Access self, int x) {
    self->max_x = x;
    Point_Type_set_x(self->center, (x - 1) / 2);
    MAX_X = x;
}


  /** @brief 重設 max_y 的值
   * @param self Camera 物件
   * @param y 要設定的 max 值
  */
void EXPORT(set_max_y) (Camera_Access self, int y) {
    self->max_y = y;
    Point_Type_set_y(self->center, (y - 1) / 2);
    MAX_Y = y;
}


  /** @brief 設定玩家角色
   * @param self Camera 物件
   * @param player 指定的玩家物件
   *
   * @warning 初始化時會將真實座標直接當成圖形座標，因為初始座標是固定在螢幕中間的點，這個點會固定不變，之後會修改
  */
void EXPORT(set_player) (Camera_Access self, Status_Access player) {
    Point_Access center = self->center;
    int32_t x = Point_Type_x(center);
    int32_t y = Point_Type_y(center);

    Point_Access_change(player->Real_Position);
    Point_Access_set_x(x);
    Point_Access_set_y(y);

    Point_Access_change(player->Graph_Position);
    Point_Access_set_x(x);
    Point_Access_set_y(y);

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
bool EXPORT(take) (Camera_Access self,
                   Character_Pool_Access from_pool,
                   Message_Box_Access box_access,
                   Status_Access current, Message_Type message) {
    Status_Access npc = NULL;
    Style_Access dead = self->dead;

    Point_Access max_point = Point_Type_create();
    Point_Access_change(max_point);
    Point_Access_set_x(self->max_x);
    Point_Access_set_y(self->max_y);

    Point_Access point = Point_Type_create();
    Point_Access_change(point);
    Point_Access_set_x(Point_Type_x(current->Real_Position));
    Point_Access_set_y(Point_Type_y(current->Real_Position));

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

        if ((y != 0) && (can_move_vertical(self, point) == YES)) {
            if (current->faction == FACTION_PLAYER) {
                camera_vertical_mode_setup(self, point, y);
            }

            Point_Access_change(current->Real_Position);
            Point_Access_add_x(Point_Type_x(vector));
            Point_Access_add_y(y);
        } else if ((Point_Type_x(vector) != 0)
                   && (can_move_horizon(self, point) == YES)) {
            if (current->faction == FACTION_PLAYER) {
                camera_horizon_mode_setup(self, point,
                                          Point_Type_x(vector));
            }

            Point_Access_change(current->Real_Position);
            Point_Access_add_x(Point_Type_x(vector));
            Point_Access_add_y(Point_Type_y(vector));
        }
    } else {
        Is_Alive is_alive = ALIVE;

        char *format = "%s(%s) 攻擊 %s(%s),造成 1 點傷害";
        int counter = snprintf(NULL, 0, format,
                               current->name,
                               STATUS(get_relation_string) (current),
                               npc->name,
                               STATUS(get_relation_string) (npc));

        char attack_message[counter];
        snprintf(attack_message, counter + 1, format,
                 current->name,
                 STATUS(get_relation_string) (current),
                 npc->name, STATUS(get_relation_string) (npc));
        BOX(add_message) (box_access, attack_message);

        switch (npc->faction) {
            case FACTION_PLAYER:
                is_alive = CP(attack_player_by) (from_pool, current);
                break;
            case FACTION_ALLY:
                is_alive = CP(attack_ally_by) (from_pool, current, npc);
                break;
            case FACTION_ENEMY:
                is_alive = CP(attack_enemy_by) (from_pool, current, npc);
                break;
            case FACTION_NEUTRAL:
                is_alive = CP(attack_neutral_by) (from_pool, current, npc);
                break;
            default:
                break;
        }

        if (is_alive == DEAD) {
            STATUS(set_style) (npc, dead);

            counter = String_ascii_length(npc->name);

            if (npc->faction == FACTION_PLAYER) {
                format = " 已死亡，任意按鍵離開遊戲";
                counter = counter + String_ascii_length(format);
            } else {
                format = " 死亡";
                counter = counter + String_ascii_length(format);
            }
            char death_message[counter];
            snprintf(death_message, counter + 1, "%s%s",
                     npc->name, format);
            BOX(add_message) (box_access, death_message);
        }
    }

    Rectangle_Access_auto_create(rectangle, origin);
    Rectangle_Access_set_top_left_point(self->start);
    Rectangle_Access_set_down_right_point(max_point);

    CP(calculate_graph_position) (from_pool, &rectangle);

  DONE:
    Point_Type_free(vector);
    Point_Type_free(point);
    Point_Type_free(max_point);
    return true;
}

#undef STATUS
#undef MAP
