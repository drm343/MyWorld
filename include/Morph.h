#ifndef HEADER_MORPH_CLASS
#define HEADER_MORPH_CLASS

#include "BaseClass.h"
#include "Point.h"
#include "String.h"


#define MORPH(name) Morph_##name
#define CLASS struct Morph *

typedef struct Morph *Morph;

typedef void (*ColorCallback) (Morph self);


typedef struct MORPH (Property) {
    Point position;
    /**< position 起始座標 */
    Point extent;
    /**< extent 長度及寬度 */
    void *color;
    /**< color 顏色 */
    ImmutableString name;
    /**< name Morph 的名稱 */
    Morph owner;
    /**< owner 此 Morph 上層 Morph, NULL 表示為最上層 */
    Morph submorph;
    /**< submorph 此 Morph 的下層 Morph, NULL 表示為最下層 */
} *MORPH(Property);

typedef struct Morph {
    Class class;
    /**< class 用來判斷 Morph 的 Custom_Property 是否相同 */
     MORPH(Property) morph;
    Custom_Property property;

    /** @brief 釋放 Morph 物件
     * @param self Morph 物件
     */
    void (*free) (CLASS self);

    /** @brief 設定 Morph 座標
     * @param self Morph 物件
     * @param x x 座標
     * @param y y 座標
     */
    void (*set_position) (CLASS self, int32_t x, int32_t y);

    /** @brief 取出 Morph 的座標
     * @param self Morph 物件
     * @param x x 座標
     * @param y y 座標
     */
     Point(*position) (CLASS self);

    /** @brief 設定 Morph 的長寬
     * @param self Morph 物件
     * @param x 長度
     * @param y 寬度
     */
    void (*set_extent) (CLASS self, int32_t x, int32_t y);

    /** @brief 取出 Morph 的長寬
     * @param self Morph 物件
     */
     Point(*extent) (CLASS self);

    /** @brief 設定 Morph 的外部名稱
     * @param self Morph 物件
     * @param name 外部名稱
     */
    void (*set_name) (CLASS self, const char *name);

    /** @brief 取出 Morph 的長寬
     * @param self Morph 物件
     * @return 外部名稱
     */
    char *(*name) (CLASS self);

    /** @brief 自動建立 color 避免 color 為 NULL
     * @param self Morph 物件
     */
    void (*auto_init_color) (CLASS self);

    /** @brief 自動釋放 color
     * @param self Morph 物件
     */
    void (*auto_free_color) (CLASS self);

    /** @brief 設定 Morph 的顏色
     * @param self Morph 物件
     * @param color Color 物件
     */
    void (*set_color) (CLASS self, void *color);

    /** @brief 取出 Morph 的 Color 物件
     * @param self Morph 物件
     * @return Color 物件
     *
     * Color 必須自行轉型。
     */
    void *(*color) (CLASS self);

    /** @brief 設定 Morph 的上下關係
     * @param self Morph 物件
     * @param other 要設定為 submorph 的 Morph 物件
     */
    void (*add) (CLASS self, CLASS other);

    /** @brief 取出下一個 Morph 物件
     * @param self Morph 物件
     * @return submorph 的 Morph 物件
     *
     * 未來可能會修改
     */
     CLASS(*submorph) (CLASS self);

    /** @brief 在螢幕上畫出 Morph 物件
     * @param self Morph 物件
     */
    void (*draw) (CLASS self);
} *Morph;


/** @brief 建立新的 Morph 物件
 * @param init_color 初始化顏色用
 * @param free_color 自動釋放顏色用
 * @return 新建立的物件
 */
Morph MORPH(create) (ColorCallback init_color, ColorCallback free_color);


/** @brief 確認兩個 Morph 的 Custom_Property 是否相同
 * @param self Morph 物件
 * @param other Morph 物件
 * @return 確認結果
 */
bool MORPH(is_the_same_id) (Morph self, Morph other);

#undef CLASS
#endif
