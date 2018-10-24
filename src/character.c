#include "character.h"

// -----------------------------------
//Init and free
// -----------------------------------
#define EXPORT(name) CHARA(name)

/** @brief 初始化角色
 * @param self 要初始化的角色
 *
 * 請確保傳入的物件不是 NULL
 */
void EXPORT(init) (Character_Access self) {
    STATUS(init) (self->status);

    self->Real_Position = Point_create();
    self->Graph_Position = Point_create();
}

/** @brief 釋放角色
 * @param self 要釋放的角色
 *
 * 並不會直接釋放 self 本身的 Access，這部份通常透過 Status_Pool 管理。
 */
void EXPORT(free) (Character_Access self) {
    Point_free(self->Real_Position);
    Point_free(self->Graph_Position);
    STATUS(free) (self->status);
}

/** @brief 複製角色資料
 * @param self 資料儲存的地方，通常為一個新的空角色
 * @param from 要被複製的角色
 *
 * 請確保傳進去的物件不是 NULL。
 *
 * 這個函數不會釋放資料，只會複製資料，如果資料本身是 Access，則直接複製 Access，
 * 因此請確保角色資料的 Access 不會在複製後被釋放。
 */
void EXPORT(copy) (Character_Access self, Character_Access from) {
    STATUS(copy) (self->status, from->status);
    self->Mark = from->Mark;
}

#ifdef DEBUG
/** @brief 顯示角色部份訊息，Debug 用
 * @param self 要顯示的角色
 */
void EXPORT(print_status) (Character_Access self) {
    STATUS(print_status) (self->status);
    DEBUG_PRINT("Mark: %s\n", self->Mark->mark->str);
}
#endif

//-----------------------------------------
//GET or SET BASE VALUE
// -----------------------------------------
/** @brief 設定角色圖形物件
 * @param self 要設定的角色
 * @param style 角色圖形
 */
void EXPORT(set_style) (Character_Access self, Style_Access style) {
    self->Mark = style;
}

//-----------------------------------------
//POSITION
// -----------------------------------------
/** @brief 設定角色隨機座標
 * @param self 目標角色
 * @param max_x 最大 x 隨機值最大
 * @param max_y 最大 y 隨機值最大
 */
void
EXPORT(set_random_position) (Character_Access self,
                             int64_t max_x, int64_t max_y) {
    Point_set(self->Real_Position, .x = rand() % max_x, .y = rand() % max_y);
}

/** @brief 取得角色真實座標
 * @param self 目標角色
 * @return 回傳座標
 */
Point EXPORT(get_position) (Character_Access self) {
    return self->Real_Position;
}

#undef EXPORT
