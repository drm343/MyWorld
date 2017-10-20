#include "character.h"


#define EXPORT(name) Character_Base_##name


  /** @brief 初始化角色基本資料
   * @param self 要初始化的角色物件
   *
   * 請確保傳進去的物件不是 NULL
  */
void EXPORT(init)(Character_Base_Type * self)
{
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
void EXPORT(free)(Character_Base_Type * access)
{
    Point_Type_free(access->Real_Position);
    Point_Type_free(access->Graph_Position);
    access->status = UNUSE;
}


  /** @brief 複製角色基本資料
   * @param self 目標
   * @param origin 要被複製的角色物件
   *
   * 請確保傳進去的物件不是 NULL。
   *
   * 由於 name、Mark 皆為不會變動的指標，因此可以放心複製。
  */
void EXPORT(copy)(Character_Base_Access access,
                           Character_Base_Access from)
{
    access->name = from->name;
    access->Mark = from->Mark;

    access->crossable = from->crossable;
    access->attackable = from->attackable;
    access->is_alive = true;
    access->status = IN_USE;
}
#undef EXPORT