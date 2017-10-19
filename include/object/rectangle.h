#ifndef HEADER_OBJECT_RECTANGLE_TYPE
#define HEADER_OBJECT_RECTANGLE_TYPE

#include "instance/point.h"

typedef struct {
    Point_Type *top_left_point;
    Point_Type *down_right_point;
} Rectangle_Type;

typedef Rectangle_Type *Rectangle_Access;


/** @brief 建立新的 Rectangle_Type 物件
 * @return 物件的 Access
 */
Rectangle_Type *Rectangle_Type_create(void);

/** @brief 釋放 Rectangle_Type 物件
 * @param self Rectangle_Type 物件的 Access
 */
void Rectangle_Type_free(Rectangle_Type * self);

/** @brief 取出 Rectangle 左上角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @return 左上角的座標
 */
Point_Access Rectangle_Type_top_left_point(Rectangle_Type * self);

/** @brief 存入 Rectangle 左上角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @param point Point_Type 物件的 Access
 * 
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Type_set_top_left_point(Rectangle_Type * self,
                                       Point_Access point);

/** @brief 取出 Rectangle 右下角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @return 右下角的座標
 */
Point_Access Rectangle_Type_down_right_point(Rectangle_Type * self);

/** @brief 存入 Rectangle 右下角的 Point_Type 物件
 * @param self Rectangle_Type 物件的 Access
 * @param point Point_Type 物件的 Access
 * 
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void Rectangle_Type_set_down_right_point(Rectangle_Type * self,
                                         Point_Access point);
#endif
