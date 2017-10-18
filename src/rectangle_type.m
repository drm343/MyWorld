#include "object/rectangle.h"

/** @brief 建立新的 Rectangle_Type 物件
 * @return 物件的 Access
 */
Rectangle_Type* Rectangle_Type_create(void) {
  Rectangle_Type *self = calloc(1, sizeof(Rectangle_Type));
  self->top_left_point = Point_Type_create();
  self->down_right_point = Point_Type_create();
  return self;
}

/** @brief 釋放 Rectangle_Type 物件
 * @param self Rectangle_Type 物件的 Access
 */
void Rectangle_Type_free(Rectangle_Type *self) {
  Point_Type_free(self->top_left_point);
  Point_Type_free(self->down_right_point);
  free(self);
}

/** @brief 取出 Rectangle 左上角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @return 左上角的座標
 */
Point_Access Rectangle_Type_top_left_point(Rectangle_Type *self) {
  return self->top_left_point;
}


/** @brief 存入 Rectangle 左上角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @param point Point_Type 物件的 Access
 * 
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Type_set_top_left_point(Rectangle_Type *self, Point_Access point) {
  Point_Access_change(self->top_left_point);
  Point_Access_set_x(point->x);
  Point_Access_set_y(point->y);
}


/** @brief 取出 Rectangle 右下角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @return 右下角的座標
 */
Point_Access Rectangle_Type_down_right_point(Rectangle_Type *self) {
  return self->down_right_point;
}

/** @brief 存入 Rectangle 右下角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @param point Point_Type 物件的 Access
 * 
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Type_set_down_right_point(Rectangle_Type *self,
   Point_Access point) {
  Point_Access_change(self->down_right_point);
  Point_Access_set_x(point->x);
  Point_Access_set_y(point->y);
}