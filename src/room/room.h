#ifndef HEADER_ROOM
#define HEADER_ROOM

#include <stdbool.h>
#include "Rectangle.h"

/**@brief 房間閥值
 *
 * 用來設定房間右下角 x 跟 y 座標的範圍
 */
typedef Rectangle Therehold;

typedef struct Room {
    Rectangle super;
    int16_t diagonal_length;
} Room;

/** @brief 設定隨機初始位置
 * @param self 要設定的房間
 * @param map_size 地圖的大小
 * @param setting 房間閥值
 */
void
Room_set_random_position(Room * self, Point map_size, Therehold setting);

/** @brief 比較兩個房間，確認哪個房間在左邊
 * @param self 自己的房間
 * @param other 比較的房間
 * @return 如果自己的房間在左邊就回傳 true，否則回傳 false
 */
bool Room_compare_small(Room * self, Room * other);
#endif
