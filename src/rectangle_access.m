#include "instance/rectangle.h"

static Rectangle_Access self = NULL;

static bool is_not_null(void) {
  if (self != NULL) {
    return true;
  }
  return false;
}


/** @brief 替換程式內部 Rectangle_Type 物件
 * @param this 需要使用的新物件
 * @return 回傳原物件的 Access
 *
 * 此函數會將新的 Rectangle_Type 物件的 Access 存在內部使用，然後將舊的
 * 物件 Access 回傳出來。
 */
Rectangle_Access Rectangle_Access_change(Rectangle_Access this) {
  Rectangle_Access origin = self;
  self = this;
  return origin;
}

/** @brief 取出 Rectangle 左上角的 Point_Type 物件
 * @return 左上角的座標
 */
Point_Access Rectangle_Access_top_left_point(void) {
  Point_Access result = NULL;

  if (is_not_null()) {
    result = Rectangle_Type_top_left_point(self);
  }
  return result;
}

/** @brief 存入 Rectangle 左上角的 Point_Type 物件
 * @param point Point_Type 物件的 Access
 * 
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Access_set_top_left_point(Point_Access point) {
  if (is_not_null()) {
    Rectangle_Type_set_top_left_point(self, point);
  }
}

/** @brief 取出 Rectangle 右下角的 Point_Type 物件
 * @return 右下角的座標
 */
Point_Access Rectangle_Access_down_right_point(void) {
  Point_Access result = NULL;

  if (is_not_null()) {
    result = Rectangle_Type_down_right_point(self);
  }
  return result;
}

/** @brief 存入 Rectangle 右下角的 Point_Type 物件
 * @param point Point_Type 物件的 Access
 * 
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Access_set_down_right_point(Point_Access point) {
  if (is_not_null()) {
    Rectangle_Type_set_down_right_point(self, point);
  }
}