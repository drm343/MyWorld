#include "character.h"


static void character_copy(Character_Base_Access access, Character_Base_Access from) {
  access->name = from->name;
  access->Mark = from->Mark;

  access->crossable = from->crossable;
  access->attackable = from->attackable;
  access->is_alive = true;
  access->status = IN_USE;

  access->copy = from->copy;
}


void Character_Base_init(Character_Base_Type *access) {
  access->Real_Position = Point_Type_create();
  access->Graph_Position = Point_Type_create();

  Point_Access_change(access->Real_Position);
  Point_Access_set_x(0);
  Point_Access_set_y(0);

  Point_Access_change(access->Graph_Position);
  Point_Access_set_x(-1);
  Point_Access_set_y(-1);

  access->crossable = NO;
  access->attackable = YES;
  access->is_alive = true;
  access->status = IN_USE;

  access->copy = character_copy;
}


void Character_Base_free(Character_Base_Type *access) {
  Point_Type_free(access->Real_Position);
  Point_Type_free(access->Graph_Position);
  access->status = UNUSE;
}