#include <math.h>

#include "Rectangle.h"
#include "base/Class.h"

#define EXPORT(name) RECT(name)

/** @brief Rectangle
 *
 * 用來表示各種長方形。
 */
typedef struct Rectangle {
    Point position;
    /**< position 起始座標 */
    Point extent;
    /**< extent 長度及寬度 */
} *Rectangle;

/** @brief 建立新的 Rectangle 物件
 * @return Rectangle 物件
 */
Rectangle EXPORT(create) (void)
{
    Rectangle self = NEW(Rectangle);
    self->position = Point_create();
    self->extent = Point_create();
    return self;
}

/** @brief 釋放 Rectangle 物件
 * @param self Rectangle 物件
 */
void EXPORT(free) (Rectangle self)
{
    Point_free(self->position);
    Point_free(self->extent);
    free(self);
}

/** @brief 取出 Rectangle 的 position
 * @param self Rectangle 物件
 * @return position
 */
Point EXPORT(position) (Rectangle self) {
    return self->position;
}

/** @brief 修改 Rectangle 的 position
 * @param self Rectangle 物件
 * @param point 新的 position，Point 物件
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_position) (Rectangle self,
                                 Point point) {
    Point_set_by_point(self->position, point);
}

/** @brief 取出 Rectangle 的長寬
 * @param self Rectangle 物件
 * @return 長寬
 */
Point EXPORT(extent) (Rectangle self) {
    return self->extent;
}

/** @brief 修改 Rectangle 的長寬
 * @param self Rectangle 物件
 * @param point 新的長寬
 *
 * 此函數會將 point 內容複製到 self 物件內，使用後可安心釋放傳入的 point。
 */
void EXPORT(set_extent) (Rectangle self,
                                   Point point) {
    Point_set_by_point(self->extent, point);
}


/** @brief 求出兩點距離
 * @param self Rectangle 物件
 * @return 長度
 */
int16_t EXPORT(diagonal_length) (Rectangle self) {
    int16_t diff_x = self->extent->x - self->position->x;
    int16_t diff_y = self->extent->y - self->position->y;

    return (int16_t) sqrt(pow(diff_x, 2) + pow(diff_y, 2));
}

#undef EXPORT
