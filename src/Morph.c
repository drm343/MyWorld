#include "Morph.h"


typedef struct Morph {
    Point position;
    /**< position 起始座標 */
    Point extent;
    /**< extent 長度及寬度 */
    Color color;
    /**< color 顏色 */
    ImmutableString name;
    /**< name Morph 的名稱 */
    struct Morph *owner;
    /**< owner 此 Morph 上層 Morph, NULL 表示為最上層 */
    struct Morph *submorph;
    /**< submorph 此 Morph 的下層 Morph, NULL 表示為最下層 */
} *Morph;


/** @brief 建立新的 Morph 物件
 * @return 新建立的物件
 */
Morph MORPH(create) (void) {
    Morph self = NEW(Morph);
    self->position = Point_create();
    self->extent = Point_create();
    self->color = NULL;
    self->name = NULL;
    self->owner = NULL;
    self->submorph = NULL;
    return self;
}


/** @brief 釋放 Morph 物件
 * @param self Morph 物件
 */
void MORPH(free) (Morph self) {
    if (self != NULL) {
        if (self->submorph != NULL) {
            MORPH(free) (self->submorph);
        }
        Point_free(self->position);
        Point_free(self->extent);
        self->owner->submorph = NULL;
    }
}


/** @brief 設定 Morph 座標
 * @param self Morph 物件
 * @param x x 座標
 * @param y y 座標
 */
void MORPH(set_position) (Morph self, int32_t x, int32_t y) {
    Point_set(self->position,.x = x,.y = y);
}


/** @brief 取出 Morph 的座標
 * @param self Morph 物件
 * @param x x 座標
 * @param y y 座標
 */
Point MORPH(position) (Morph self) {
    return self->position;
}


/** @brief 設定 Morph 的長寬
 * @param self Morph 物件
 * @param x 長度
 * @param y 寬度
 */
void MORPH(set_extent) (Morph self, int32_t x, int32_t y) {
    Point_set(self->extent,.x = x,.y = y);
}


/** @brief 取出 Morph 的長寬
 * @param self Morph 物件
 */
Point MORPH(extent) (Morph self) {
    return self->extent;
}


/** @brief 設定 Morph 的上下關係
 * @param self Morph 物件
 * @param other 要設定為 submorph 的 Morph 物件
 */
void MORPH(add) (Morph self, Morph other) {
    self->submorph = other;
    other->owner = self;
}
