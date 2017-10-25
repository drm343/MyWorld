#include "room.h"


/** @brief 設定隨機初始位置
 * @param self 要設定的房間
 * @param map_size 地圖的大小
 * @param setting 房間閥值
 */
void Room_set_random_position(Room * self, Point_Type * map_size,
                              Therehold * setting)
{
    Point_Type start = { };
    start.x = rand() % map_size->x;
    start.y = rand() % map_size->y;

    Point_Type end = { };
    end.x = rand() % setting->end->x + setting->start->x;
    end.y = rand() % setting->end->y + setting->start->y;

    Two_Point_set_start(self->position, &start);
    Two_Point_set_end(self->position, &end);
}


/** @brief 比較兩個房間，確認哪個房間在左邊
 * @param self 自己的房間
 * @param other 比較的房間
 * @return 如果自己的房間在左邊就回傳 true，否則回傳 false
 */
bool Room_compare_small(Room * self, Room * other)
{
    Point_Access self_start = Two_Point_get_start(self->position);
    Point_Access other_start = Two_Point_get_start(other->position);

    if (self_start->x < other_start->x) {
        return true;
    }
    return false;
}
