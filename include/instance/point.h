#ifndef HEADER_OBJECT_NEW_POINT_ACCESS
#define HEADER_OBJECT_NEW_POINT_ACCESS

#include "object/point.h"


/** @brief 替換程式內部 Point_Type 物件
 * @param this 需要使用的新物件
 * @return 回傳原物件的 Access
 *
 * 此函數會將新的 Point_Type 物件的 Access 存在內部使用，然後將舊的
 * 物件 Access 回傳出來。
 */
Point_Access Point_Access_change(Point_Access this);

/** @brief 取出 Point_Type 物件 x 欄位的數值
 * @return 欄位 x 的數值
 */
int32_t Point_Access_x(void);

/** @brief 取出 Point_Type 物件 y 欄位的數值
 * @return 欄位 y 的數值
 */
int32_t Point_Access_y(void);

/** @brief 存入數值到 Point_Type 的 x 欄位
 * @param x 想存入的數值
 */
void Point_Access_set_x(int32_t x);

/** @brief 存入數值到 Point_Type 的 y 欄位
 * @param y 想存入的數值
 */
void Point_Access_set_y(int32_t y);

/** @brief 增加數值到到 Point_Type 的 x 欄位
 * @param x 想存入的數值
 */
void Point_Access_add_x(int32_t x);

/** @brief 增加數值到到 Point_Type 的 y 欄位
 * @param y 想存入的數值
 */
void Point_Access_add_y(int32_t y);

/** @brief 比較兩點是否相等
 * @param other Point_Type 物件的 Access
 * @return 是否相等
 */
bool Point_Access_eq(Point_Access other);


#ifdef DEBUG
/** @brief 顯示 Point_Type 內容
 */
void Point_Access_print(void);
#endif

#endif
