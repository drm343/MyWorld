#include "room.h"

/** @brief 設定隨機初始位置
 * @param self 要設定的房間
 * @param map_size 地圖的大小
 * @param setting 房間閥值
 */
void
Room_set_random_position(Room * self, Point map_size,
                         Therehold setting)
{
    RECT_set_position(self->super, .x = rand() % map_size->x, .y = rand() % map_size->y);
    RECT_set_extent(self->super, .x = rand() % RECT(extent)(setting)->x + RECT(position)(setting)->x,
                      .y = RECT(extent)(setting)->y + RECT(position)(setting)->y);
    self->diagonal_length = RECT(diagonal_length)(self->super);
}

/** @brief 比較兩個房間，確認哪個房間在左邊
 * @param self 自己的房間
 * @param other 比較的房間
 * @return 如果自己的房間在左邊就回傳 true，否則回傳 false
 */
bool Room_compare_small(Room * self, Room * other)
{
    Point self_position = RECT(position)(self->super);
    Point other_position = RECT(position)(other->super);

    if (self_position->x < other_position->x) {
        return true;
    }
    return false;
}
