#ifndef HEADER_MAP_SYSTEM_OBJECT
#define HEADER_MAP_SYSTEM_OBJECT

#include "base_type.h"
#include "two_point.h"


/** @brief Namespace Map_Type_
 *
 * 當使用 EXPORT 的函數時，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如 create 必須寫成 Map_Type_create，如果外部程式要簡化呼叫，
 * 可以在程式中自行定義新的 macro，例如下面範例。
 *
 * #define MAP(name) Map_Type_##name<br>
 * Map_Type *self = MAP(create)();
 */
#define EXPORT(name) Map_Type_##name


typedef Two_Point Map_Type;
typedef Map_Type *Map_Access;


/** @brief 建立地圖系統
 * @return 地圖物件
 */
Map_Type *EXPORT(create) (void);

/** @brief 釋放地圖系統
 * @param self 地圖物件
 */
void EXPORT(free) (Map_Type * self);

/** @brief 設定左上角的點座標
 * @param self 地圖物件
 * @param x X 座標位置
 * @param y Y 座標位置
 */
void EXPORT(set_top_left) (Map_Type * self, int32_t x, int32_t y);

/** @brief 取出左上角的點物件
 * @param self 地圖物件
 *
 * 請不要釋放回傳的物件
 */
Point_Access EXPORT(top_left) (Map_Type * self);

/** @brief 設定右下角的點座標
 * @param self 地圖物件
 * @param x X 座標位置
 * @param y Y 座標位置
 */
void EXPORT(set_bottom_right) (Map_Type * self, int32_t x, int32_t y);

/** @brief 取出右下角的點物件
 * @param self 地圖物件
 *
 * 請不要釋放回傳的物件
 */
Point_Access EXPORT(bottom_right) (Map_Type * self);

/** @brief 取得左上角的 X 座標
 * @param self 地圖物件
 */
int32_t EXPORT(top_left_x) (Map_Type * self);

/** @brief 取得左上角的 Y 座標
 * @param self 地圖物件
 */
int32_t EXPORT(top_left_y) (Map_Type * self);

/** @brief 取得右下角的 X 座標
 * @param self 地圖物件
 */
int32_t EXPORT(bottom_right_x) (Map_Type * self);

/** @brief 取得右下角的 Y 座標
 * @param self 地圖物件
 */
int32_t EXPORT(bottom_right_y) (Map_Type * self);

/** @brief 移動左上角的點座標
 * @param self 地圖物件
 * @param x 需要移動的 X 值
 * @param y 需要移動的 Y 值
 */
void EXPORT(move_top_left) (Map_Type * self, int32_t x, int32_t y);

/** @brief 移動右下角的點座標
 * @param self 地圖物件
 * @param x 需要移動的 X 值
 * @param y 需要移動的 Y 值
 */
void EXPORT(move_bottom_right) (Map_Type * self, int32_t x, int32_t y);

#undef EXPORT
#endif
