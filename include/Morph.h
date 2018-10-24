#ifndef HEADER_MORPH_CLASS
#define HEADER_MORPH_CLASS

#include "BaseClass.h"
#include "Point.h"
#include "String.h"


#define MORPH(name) Morph_##name


typedef void *Color;
typedef struct Morph *Morph;


/** @brief 建立新的 Morph 物件
 * @return 新建立的物件
 */
Morph MORPH(create) (void);


/** @brief 釋放 Morph 物件
 * @param self Morph 物件
 */
void MORPH(free) (Morph self);


/** @brief 設定 Morph 的顏色
 * @param self Morph 物件
 * @param color Color 物件
 *
 * Color 實際上是 void *，可以根據需求傳入自己的型別。
 */
void MORPH(set_color) (Morph self, Color color);


/** @brief 取出 Morph 的 Color 物件
 * @param self Morph 物件
 * @return Color 物件
 *
 * Color 實際上是 void *，必須自行轉型。
 */
Color MORPH(color) (Morph self);


/** @brief 設定 Morph 座標
 * @param self Morph 物件
 * @param x x 座標
 * @param y y 座標
 */
void MORPH(set_position) (Morph self, int32_t x, int32_t y);


/** @brief 取出 Morph 的座標
 * @param self Morph 物件
 * @param x x 座標
 * @param y y 座標
 */
Point MORPH(position) (Morph self);


/** @brief 設定 Morph 的長寬
 * @param self Morph 物件
 * @param x 長度
 * @param y 寬度
 */
void MORPH(set_extent) (Morph self, int32_t x, int32_t y);


/** @brief 取出 Morph 的長寬
 * @param self Morph 物件
 */
Point MORPH(extent) (Morph self);


/** @brief 設定 Morph 的上下關係
 * @param self Morph 物件
 * @param other 要設定為 submorph 的 Morph 物件
 */
void MORPH(add) (Morph self, Morph other);

#endif
