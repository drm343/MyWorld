#include "point.h"


/** @brief 建立新的 Point_Type 物件
 * @return 物件的 Access
 */
Point_Type *Point_Type_create(void)
{
    Point_Type *self = calloc(1, sizeof(Point_Type));
    self->x = 0;
    self->y = 0;
    return self;
}


/** @brief 釋放 Point_Type 物件
 * @param self Point_Type 物件的 Access
 */
void Point_Type_free(Point_Type * self)
{
    free(self);
}


/** @brief 取出 Point_Type x 欄位的數值
 * @param self Point_Type 物件的 Access
 * @return 欄位 x 的數值
 */
int32_t Point_Type_x(Point_Type * self)
{
    return self->x;
}


/** @brief 取出 Point_Type y 欄位的數值
 * @param self Point_Type 物件的 Access
 * @return 欄位 y 的數值
 */
int32_t Point_Type_y(Point_Type * self)
{
    return self->y;
}


/** @brief 重設點座標
 * @param self 原始點座標
 * @param other 想重設的數值
 */
void Point_Type_set_by_point(Point_Type * self, Point_Type * other)
{
    self->x = other->x;
    self->y = other->y;
}


/** @brief 移動點座標
 * @param self 原始點座標
 * @param other 想存入的數值
 */
void Point_Type_move_by_point(Point_Type * self, Point_Type * other)
{
    self->x = self->x + other->x;
    self->y = self->y + other->y;
}


/** @brief 比較兩點是否相等
 * @param self Point_Type 物件的 Access
 * @param other Point_Type 物件的 Access
 * @return 是否相等
 */
bool Point_Type_eq(Point_Type * self, Point_Type * other)
{
    if ((self->x == other->x) && (self->y == other->y)) {
        return true;
    }
    return false;
}


/** @brief 顯示內容
 * @param self Point_Type 物件的 Access
 */
void Point_Type_print(Point_Type * self)
{
#ifdef DEBUG
    DEBUG_PRINT("[point type] x:%d y:%d\n", self->x, self->y);
#endif
}
