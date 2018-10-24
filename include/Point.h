#ifndef HEADER_OBJECT_NEW_POINT_TYPE
#define HEADER_OBJECT_NEW_POINT_TYPE

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include "helper/debug.h"

typedef struct Point {
    int32_t x;
    /**< x 座標 */
    int32_t y;
    /**< y 座標 */
} *Point;

/** @brief 建立新的 Point 物件
 * @return 新建立的物件
 */
Point Point_create(void);

/** @brief 釋放 Point 物件
 * @param self Point 物件
 */
void Point_free(Point self);

/** @brief 取出 Point x 欄位的數值
 * @param self Point 物件
 * @return 欄位 x 的數值
 */
int32_t Point_x(Point self);

/** @brief 取出 Point y 欄位的數值
 * @param self Point 物件
 * @return 欄位 y 的數值
 */
int32_t Point_y(Point self);

/** @brief 重設點座標
 * @param self 原始點座標
 * @param other 想重設的數值
 */
void Point_set_by_point(Point self, Point other);

/** @brief 移動點座標
 * @param self 原始點座標
 * @param other 想存入的數值
 */
void Point_move_by_point(Point self, Point other);

/** @brief 重設點座標
 * @param self 點座標
 */
#define Point_set(self, ...) \
{\
    struct Point item = { __VA_ARGS__ };\
    Point_set_by_point(self, &item);\
}

/** @brief 移動點座標
 * @param self 點座標
 */
#define Point_move(self, ...) \
{\
    struct Point item = { __VA_ARGS__ };\
    Point_move_by_point(self, &item);\
}

/** @brief 比較兩點是否相等
 * @param self Point 物件
 * @param other 想比較的 Point 物件
 * @return 是否相等
 */
bool Point_equal(Point self, Point other);

/** @brief 顯示內容
 * @param self Point 物件
 */
void Point_print(Point self);

#endif
