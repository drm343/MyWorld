#include "character.h"


#define EXPORT(name) Character_Base_##name


  /** @brief 初始化角色基本資料
   * @param self 要初始化的角色物件
   *
   * 請確保傳進去的物件不是 NULL
   *
   * @warning 目前初始化會自動給予點座標物件，但還沒有釋放角色的時機，因此不會釋放點座標，會造成 leak，這部份需要處理。
  */
void EXPORT(init) (Character_Base * self) {
    self->Real_Position = Point_Type_create();
    self->Graph_Position = Point_Type_create();

    Point_Access_change(self->Real_Position);
    Point_Access_set_x(0);
    Point_Access_set_y(0);

    Point_Access_change(self->Graph_Position);
    Point_Access_set_x(-1);
    Point_Access_set_y(-1);

    self->crossable = NO;
    self->attackable = YES;
    self->is_alive = true;
    self->status = IN_USE;
}


  /** @brief 釋放角色基本資料
   * @param self 要釋放的角色物件
   *
   * 請確保傳進去的物件不是 NULL。
  */
void EXPORT(free) (Character_Base * self) {
    Point_Type_free(self->Real_Position);
    Point_Type_free(self->Graph_Position);
    self->status = UNUSE;
}


  /** @brief 複製角色基本資料
   * @param self 目標
   * @param origin 要被複製的角色物件
   *
   * 請確保傳進去的物件不是 NULL。
   *
   * 由於 name、Mark 皆為不會變動的指標，因此可以放心複製。
  */
void EXPORT(copy) (Character_Base * self, Character_Base * from) {
    self->name = from->name;
    self->Mark = from->Mark;

    self->crossable = from->crossable;
    self->attackable = from->attackable;
    self->is_alive = ALIVE;
    self->status = IN_USE;
}


/** @brief 確認角色是否存活
 * @param self 要顯示的角色
 * @return 角色存活狀態
 */
Is_Alive EXPORT(is_alive) (Character_Base * self) {
    return self->is_alive;
}

#undef EXPORT
