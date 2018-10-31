#include "Morph.h"


typedef struct Custom_Property {
} *Custom_Property;


/** @brief 釋放 Morph 物件
 * @param self Morph 物件
 */
static void MORPH(free) (Morph self) {
    if (self != NULL) {
        MORPH(Property) morph = self->morph;

        if (morph->submorph != NULL) {
            Morph submorph = morph->submorph;
            submorph->free(submorph);
        }

        if (morph->owner != NULL) {
            Morph owner = morph->owner;
            owner->morph->submorph = NULL;
            morph->owner = NULL;
        }

        if (morph->name != NULL) {
            String_free(morph->name);
        }

        Point_free(morph->position);
        Point_free(morph->extent);
        self->auto_free_color(self);
        free(morph);
        RELEASE_CLASS(self->class);
        free(self);
    }
}


/** @brief 設定 Morph 座標
 * @param self Morph 物件
 * @param x x 座標
 * @param y y 座標
 */
static void MORPH(set_position) (Morph self, int32_t x, int32_t y) {
    Point_set(self->morph->position,.x = x,.y = y);
}


/** @brief 取出 Morph 的座標
 * @param self Morph 物件
 * @param x x 座標
 * @param y y 座標
 */
static Point MORPH(position) (Morph self) {
    return self->morph->position;
}

/** @brief 設定 Morph 的外部名稱
 * @param self Morph 物件
 * @param name 外部名稱
 */
static void MORPH(set_name) (Morph self, const char *name) {
    if (self->morph->name != NULL) {
        String_free(self->morph->name);
    }
    self->morph->name = String_create(name);
}

/** @brief 取出 Morph 的長寬
 * @param self Morph 物件
 * @return 外部名稱
 */
static char *MORPH(name) (Morph self) {
    return self->morph->name->str;
}

/** @brief 設定 Morph 的長寬
 * @param self Morph 物件
 * @param x 長度
 * @param y 寬度
 */
static void MORPH(set_extent) (Morph self, int32_t x, int32_t y) {
    Point_set(self->morph->extent,.x = x,.y = y);
}


/** @brief 取出 Morph 的長寬
 * @param self Morph 物件
 */
static Point MORPH(extent) (Morph self) {
    return self->morph->extent;
}


/** @brief 設定 Morph 的顏色
 * @param self Morph 物件
 * @param color 要設定的顏色
 */
static void MORPH(set_color) (Morph self, void *color) {
    self->morph->color = color;
}


/** @brief 取出 Morph 的顏色
 * @param self Morph 物件
 * @return Morph 的顏色
 */
static void *MORPH(color) (Morph self) {
    return self->morph->color;
}


/** @brief 設定 Morph 的上下關係
 * @param self Morph 物件
 * @param other 要設定為 submorph 的 Morph 物件
 */
static void MORPH(add) (Morph self, Morph other) {
    self->morph->submorph = other;
    other->morph->owner = self;
}


/** @brief 取出下一個 Morph 物件
 * @param self Morph 物件
 * @return submorph 的 Morph 物件
 */
static Morph MORPH(submorph) (Morph self) {
    return self->morph->submorph;
}


/** @brief 建立新的 Morph 物件
 * @param init_color 初始化顏色用
 * @param free_color 自動釋放顏色用
 * @return 新建立的物件
 */
Morph MORPH(create) (ColorCallback init_color, ColorCallback free_color) {
    Morph self = NEW(Morph);
    self->class = NULL;

    self->morph = NEW(Morph_Property);
    self->morph->position = Point_create();
    self->morph->extent = Point_create();
    self->morph->name = String_create("");
    self->morph->owner = NULL;
    self->morph->submorph = NULL;
    self->property = NULL;

    self->free = MORPH(free);
    self->set_position = MORPH(set_position);
    self->position = MORPH(position);
    self->set_extent = MORPH(set_extent);
    self->extent = MORPH(extent);
    self->set_name = MORPH(set_name);
    self->name = MORPH(name);
    self->set_color = MORPH(set_color);
    self->color = MORPH(color);
    self->add = MORPH(add);
    self->submorph = MORPH(submorph);
    self->draw = NULL;
    self->auto_init_color = init_color;
    self->auto_free_color = free_color;

    init_color(self);
    return self;
}


/** @brief 確認兩個 Morph 的 Custom_Property 是否相同
 * @param self Morph 物件
 * @param other Morph 物件
 * @return 確認結果
 */
bool MORPH(is_the_same_id) (Morph self, Morph other) {
    return CHECK_CLASS(self->class, other->class);
}
