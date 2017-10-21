#include "two_point.h"

#define EXPORT(name) Two_Point_##name


/** @brief 建立新的 Two_Point 物件
 * @return 物件的 Access
 */
Two_Point *EXPORT(create) (void) {
    Two_Point *self = calloc(1, sizeof(Two_Point));
    self->start = Point_Type_create();
    self->end = Point_Type_create();
    return self;
}


/** @brief 釋放 Two_Point 物件
 * @param self Two_Point 物件的 Access
 */
void EXPORT(free) (Two_Point * self) {
    Point_Type_free(self->start);
    Point_Type_free(self->end);
    free(self);
}


/** @brief 取出 Two_Point 的 start Point
 * @param self Two_Point 物件的 Access
 * @return start Point
 *
 * 本函數會直接傳出點座標的 Access，請不要直接釋放點座標
 */
Point_Access EXPORT(get_start) (Two_Point * self) {
    return self->start;
}


/** @brief 存入 Point 到 start
 * @param self Two_Point 物件的 Access
 * @param point Point 物件的 Access
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_start) (Two_Point * self, Point_Access point) {
    Point_Access_change(self->start);
    Point_Access_set_x(point->x);
    Point_Access_set_y(point->y);
}


/** @brief 存入 Point 到 start
 * @param self Two_Point 物件的 Access
 * @param x X 座標位置
 * @param y Y 座標位置
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_start_x_and_y) (Two_Point * self, int32_t x, int32_t y) {
    Point_Access_change(self->start);
    Point_Access_set_x(x);
    Point_Access_set_y(y);
}


/** @brief 取出 Two_Point 的 end Point
 * @param self Two_Point 物件的 Access
 * @return end Point
 */
Point_Access EXPORT(get_end) (Two_Point * self) {
    return self->end;
}


/** @brief 存入 Point 到 end
 * @param self Two_Point 物件的 Access
 * @param point Point 物件的 Access
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_end) (Two_Point * self, Point_Access point) {
    Point_Access_change(self->end);
    Point_Access_set_x(point->x);
    Point_Access_set_y(point->y);
}


/** @brief 存入 Point 到 end
 * @param self Two_Point 物件的 Access
 * @param x X 座標位置
 * @param y Y 座標位置
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_end_x_and_y) (Two_Point * self, int32_t x, int32_t y) {
    Point_Access_change(self->end);
    Point_Access_set_x(x);
    Point_Access_set_y(y);
}

#undef EXPORT
