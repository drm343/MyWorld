#ifndef HEADER_MAP_SYSTEM_OBJECT
#define HEADER_MAP_SYSTEM_OBJECT

#include "namespace.h"

#include "base_type.h"
#include "two_point.h"
#include "graphic.h"


/** @brief Namespace MAP
 */
#define EXPORT(name) MAP(name)


typedef int8_t Level[128][128];


typedef struct Map_Type {
    Two_Point *base;
    Level *current;
} Map_Type;
typedef Map_Type *Map_Access;


// ----------------------------------------
// Init Map_System
// ----------------------------------------
/** @brief 建立地圖系統
 * @return 地圖物件
 */
Map_Type *EXPORT(create) (void);


/** @brief 釋放地圖系統
 * @param self 地圖物件
 */
void EXPORT(free) (Map_Type * self);


// ----------------------------------------
// Move Map
// ----------------------------------------
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
