#include "Point.h"
#include "base/Class.h"

/** @brief 建立新的 Point 物件
 * @return 新建立的物件
 */
Point Point_create(void)
{
    Point self = NEW(Point);
    self->x = 0;
    self->y = 0;
    return self;
}

/** @brief 釋放 Point 物件
 * @param self Point 物件
 */
void Point_free(Point self)
{
    free(self);
}

/** @brief 取出 Point x 欄位的數值
 * @param self Point 物件
 * @return 欄位 x 的數值
 */
int32_t Point_x(Point self)
{
    return self->x;
}

/** @brief 取出 Point y 欄位的數值
 * @param self Point 物件
 * @return 欄位 y 的數值
 */
int32_t Point_y(Point self)
{
    return self->y;
}

/** @brief 重設點座標
 * @param self 原始點座標
 * @param other 想重設的數值
 */
void Point_set_by_point(Point self, Point other)
{
    self->x = other->x;
    self->y = other->y;
}

/** @brief 移動點座標
 * @param self 原始點座標
 * @param other 想存入的數值
 */
void Point_move_by_point(Point self, Point other)
{
    self->x = self->x + other->x;
    self->y = self->y + other->y;
}

/** @brief 比較兩點是否相等
 * @param self Point 物件
 * @param other 想比較的 Point 物件
 * @return 是否相等
 */
bool Point_equal(Point self, Point other)
{
    if ((self->x == other->x) && (self->y == other->y)) {
        return true;
    }
    return false;
}

/** @brief 顯示內容
 * @param self Point 物件
 */
void Point_print(Point self)
{
#ifdef DEBUG
    DEBUG_PRINT("[point type] x:%d y:%d\n", self->x, self->y);
#endif
}
