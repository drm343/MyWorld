#ifndef HEADER_RECTANGLE_OBJECT
#define HEADER_RECTANGLE_OBJECT

#include "Point.h"


/** @brief namespace for Rectangle_Type
 */
#define RECT(name) Rectangle_Type_##name

#define EXPORT(name) RECT(name)

typedef struct Rectangle *Rectangle;

/** @brief 建立新的 Rectangle 物件
 * @return Rectangle 物件
 */
Rectangle EXPORT(create) (void);

/** @brief 釋放 Rectangle 物件
 * @param self Rectangle 物件
 */
void EXPORT(free) (Rectangle self);

/** @brief 取出 Rectangle 的 position
 * @param self Rectangle 物件
 * @return position
 */
Point EXPORT(position) (Rectangle self);

/** @brief 修改 Rectangle 的 position
 * @param self Rectangle 物件
 * @param point 新的 position，Point 物件
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_position) (Rectangle self, Point point);

/** @brief 重設 position
 * @param self Rectangle 物件
 */
#define RECT_set_position(self, ...) \
{\
    struct Point item = { __VA_ARGS__ };\
    RECT(set_position)(self, &item);\
}

/** @brief 取出 Rectangle 的長寬
 * @param self Rectangle 物件
 * @return 長寬
 */
Point EXPORT(extent) (Rectangle self);

/** @brief 修改 Rectangle 的長寬
 * @param self Rectangle 物件
 * @param point 新的長寬
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_extent) (Rectangle self, Point point);

/** @brief 重設 extent
 * @param self Rectangle 物件
 */
#define RECT_set_extent(self, ...) \
{\
    struct Point item = { __VA_ARGS__ };\
    RECT(set_extent)(self, &item);\
}

/** @brief 求出兩點距離
 * @param self Rectangle 物件
 * @return 長度
 */
int16_t EXPORT(diagonal_length) (Rectangle self);

#undef EXPORT
#endif
