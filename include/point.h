#ifndef HEADER_OBJECT_NEW_POINT_TYPE
#define HEADER_OBJECT_NEW_POINT_TYPE

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>

#include "helper/debug.h"


typedef struct {
    int32_t x;
             /**< x 座標 */
    int32_t y;
             /**< y 座標 */
} Point_Type;

typedef Point_Type *Point_Access;


/** @brief 建立新的 Point_Type 物件
 * @return 物件的 Access
 */
Point_Type *Point_Type_create(void);

/** @brief 釋放 Point_Type 物件
 * @param self Point_Type 物件的 Access
 */
void Point_Type_free(Point_Type * self);

/** @brief 取出 Point_Type x 欄位的數值
 * @param self Point_Type 物件的 Access
 * @return 欄位 x 的數值
 */
int32_t Point_Type_x(Point_Type * self);

/** @brief 取出 Point_Type y 欄位的數值
 * @param self Point_Type 物件的 Access
 * @return 欄位 y 的數值
 */
int32_t Point_Type_y(Point_Type * self);

/** @brief 存入數值到 Point_Type 的 x 欄位
 * @param self Point_Type 物件的 Access
 * @param x 想存入的數值
 */
void Point_Type_set_x(Point_Type * self, int32_t x);

/** @brief 存入數值到 Point_Type 的 y 欄位
 * @param self Point_Type 物件的 Access
 * @param y 想存入的數值
 */
void Point_Type_set_y(Point_Type * self, int32_t y);

/** @brief 增加數值到到 Point_Type 的 x 欄位
 * @param self Point_Type 物件的 Access
 * @param x 想存入的數值
 */
void Point_Type_add_x(Point_Type * self, int32_t x);

/** @brief 增加數值到到 Point_Type 的 y 欄位
 * @param self Point_Type 物件的 Access
 * @param y 想存入的數值
 */
void Point_Type_add_y(Point_Type * self, int32_t y);

/** @brief 比較兩點是否相等
 * @param self Point_Type 物件的 Access
 * @param other Point_Type 物件的 Access
 * @return 是否相等
 */
bool Point_Type_eq(Point_Type * self, Point_Type * other);

#ifdef DEBUG
/** @brief 顯示內容
 * @param self Point_Type 物件的 Access
 */
void Point_Type_print(Point_Type * self);
#endif

#endif
