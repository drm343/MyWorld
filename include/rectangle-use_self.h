#ifndef HEADER_OBJECT_RECTANGLE_ACCESS
#define HEADER_OBJECT_RECTANGLE_ACCESS

#include "rectangle.h"

/** @brief 建立新的 Rectangle_Type 物件
 * @return 物件的 Access
 */
#define Rectangle_Access_auto_create(name, return_name) \
  Point_Type auto_create_##name##_1 = {}; \
  Point_Type auto_create_##name##_2 = {}; \
  Rectangle_Type name = {.start = &auto_create_##name##_1, .end = &auto_create_##name##_2}; \
  Rectangle_Access return_name = Rectangle_Access_change(&name)

/** @brief 替換程式內部 Rectangle_Type 物件
 * @param this 需要使用的新物件
 * @return 回傳原物件的 Access
 *
 * 此函數會將新的 Rectangle_Type 物件的 Access 存在內部使用，然後將舊的
 * 物件 Access 回傳出來。
 */
Rectangle_Access Rectangle_Access_change(Rectangle_Access this);

/** @brief 取出 Rectangle 左上角的 Point_Type 物件
 * @return 左上角的座標
 */
Point_Access Rectangle_Access_top_left_point(void);

/** @brief 存入 Rectangle 左上角的 Point_Type 物件
 * @param point Point_Type 物件的 Access
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Access_set_top_left_point(Point_Access point);

/** @brief 取出 Rectangle 右下角的 Point_Type 物件
 * @return 右下角的座標
 */
Point_Access Rectangle_Access_down_right_point(void);

/** @brief 存入 Rectangle 右下角的 Point_Type 物件
 * @param point Point_Type 物件的 Access
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Access_set_down_right_point(Point_Access point);
#endif
