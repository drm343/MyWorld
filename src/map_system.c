#include "map_system.h"


#define SUPER(name) Two_Point_##name
#define EXPORT(name) Map_Type_##name


/** @brief 建立地圖系統
 * @return 地圖物件
 */
Map_Type * EXPORT(create)(void)
{
    Map_Type *map = SUPER(create)();
    return map;
}


/** @brief 釋放地圖系統
 * @param self 地圖物件
 */
void EXPORT(free)(Map_Type *self) {
    SUPER(free)(self);
}


/** @brief 設定左上角的點座標
 * @param self 地圖物件
 * @param x X 座標位置
 * @param y Y 座標位置
 */
void EXPORT(set_top_left)(Map_Type *self, int32_t x, int32_t y)
{
    SUPER(set_start_x_and_y)(self, x, y);
}


/** @brief 取出左上角的點物件
 * @param self 地圖物件
 *
 * 請不要釋放回傳的物件
 */
Point_Access EXPORT(top_left)(Map_Type *self)
{
    return SUPER(get_start)(self);
}


/** @brief 設定右下角的點座標
 * @param self 地圖物件
 * @param x X 座標位置
 * @param y Y 座標位置
 */
void EXPORT(set_bottom_right)(Map_Type *self, int32_t x, int32_t y)
{
    SUPER(set_end_x_and_y)(self, x, y);
}


/** @brief 取出右下角的點物件
 * @param self 地圖物件
 *
 * 請不要釋放回傳的物件
 */
Point_Access EXPORT(bottom_right)(Map_Type *self)
{
    return SUPER(get_end)(self);
}


/** @brief 取得左上角的 X 座標
 * @param self 地圖物件
 */
int32_t EXPORT(top_left_x)(Map_Type *self)
{
    return Point_Type_x(SUPER(get_start)(self));
}


/** @brief 取得左上角的 Y 座標
 * @param self 地圖物件
 */
int32_t EXPORT(top_left_y)(Map_Type *self)
{
    return Point_Type_y(SUPER(get_start)(self));
}


/** @brief 取得右下角的 X 座標
 * @param self 地圖物件
 */
int32_t EXPORT(bottom_right_x)(Map_Type *self)
{
    return Point_Type_x(SUPER(get_end)(self));
}


/** @brief 取得右下角的 Y 座標
 * @param self 地圖物件
 */
int32_t EXPORT(bottom_right_y)(Map_Type *self)
{
    return Point_Type_y(SUPER(get_end)(self));
}


/** @brief 移動左上角的點座標
 * @param self 地圖物件
 * @param x 需要移動的 X 值
 * @param y 需要移動的 Y 值
 */
void EXPORT(move_top_left)(Map_Type *self, int32_t x, int32_t y)
{
    Point_Access_change(SUPER(get_start)(self));
    Point_Access_add_x(x);
    Point_Access_add_y(y);
}


/** @brief 移動右下角的點座標
 * @param self 地圖物件
 * @param x 需要移動的 X 值
 * @param y 需要移動的 Y 值
 */
void EXPORT(move_bottom_right)(Map_Type *self, int32_t x, int32_t y)
{
    Point_Access_change(SUPER(get_end)(self));
    Point_Access_add_x(x);
    Point_Access_add_y(y);
}

#undef EXPORT
#undef SUPER