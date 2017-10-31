#ifndef HEADER_TWO_POINT_OBJECT
#define HEADER_TWO_POINT_OBJECT


#include <math.h>

#include "point.h"


/** @brief Namespace Two_Point_
 *
 * 當使用 EXPORT 的函數時，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如 create 必須寫成 Two_Point_create，如果外部程式要簡化呼叫，
 * 可以在程式中自行定義新的 macro，例如下面範例。
 *
 * \#define PKG(name) Two_Point_#\#name<br>
 * Two_Point *self = PKG(create)();
 */
#define EXPORT(name) Two_Point_##name


/** @brief Virtual Point System
 *
 * 虛擬結構，用來實作其他可以用兩個點表示的系統。
 */
//typedef struct Two_Point Two_Point;
/** @brief Virtual Point System
 *
 * 虛擬結構，用來實作其他可以用兩個點表示的系統。
 */
typedef struct Two_Point {
    Point_Type *start;
    Point_Type *end;
} Two_Point;


/** @brief 建立新的 Two_Point 物件
 * @return 物件的 Access
 */
Two_Point *EXPORT(create) (void);


/** @brief 釋放 Two_Point 物件
 * @param self Two_Point 物件的 Access
 */
void EXPORT(free) (Two_Point * self);


/** @brief 取出 Two_Point 的 start Point
 * @param self Two_Point 物件的 Access
 * @return start Point
 *
 * 本函數會直接傳出點座標的 Access，請不要直接釋放點座標
 */
Point_Access EXPORT(get_start) (Two_Point * self);


/** @brief 存入 Point 到 start
 * @param self Two_Point 物件的 Access
 * @param point Point 物件的 Access
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_start_by_point) (Two_Point * self, Point_Access point);


/** @brief 存入 Point 到 start
 * @param self Two_Point 物件的 Access
 * @param x X 座標位置
 * @param y Y 座標位置
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
#define Two_Point_set_start(self, ...) \
{\
    Point_Type item = { __VA_ARGS__ };\
    Point_Type_set_by_point(self->start, &item);\
}


/** @brief 取出 Two_Point 的 end Point
 * @param self Two_Point 物件的 Access
 * @return end Point
 *
 * 本函數會直接傳出點座標的 Access，請不要直接釋放點座標
 */
Point_Access EXPORT(get_end) (Two_Point * self);


/** @brief 存入 Point 到 end
 * @param self Two_Point 物件的 Access
 * @param point Point 物件的 Access
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_end_by_point) (Two_Point * self, Point_Access point);


/** @brief 存入 Point 到 end
 * @param self Two_Point 物件的 Access
 * @param x X 座標位置
 * @param y Y 座標位置
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
#define Two_Point_set_end(self, ...) \
{\
    Point_Type item = { __VA_ARGS__ };\
    Point_Type_set_by_point(self->end, &item);\
}


/** @brief 求出兩點距離
 * @param self Two_Point 物件的 Access
 * @return 長度
 */
int16_t EXPORT(diagonal_length) (Two_Point * self);

#undef EXPORT
#endif
