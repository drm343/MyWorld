#include "character.h"


static void release(Character_Base_Access access) {
  access->status = UNUSE;
}


static void character_copy(Character_Base_Access access, Character_Base_Access from) {
  access->name = from->name;
  access->Mark = from->Mark;

  access->crossable = from->crossable;
  access->attackable = from->attackable;
  access->is_alive = true;
  access->status = IN_USE;

  access->release = from->release;
  access->copy = from->copy;
}


void character_base_init(Character_Base_Access access) {
  access->Real_Position.x  = 0;
  access->Real_Position.y  = 0;
  access->Graph_Position.x = 0;
  access->Graph_Position.y = 0;
  access->crossable = false;
  access->attackable = true;
  access->is_alive = true;
  access->status = IN_USE;

  access->release = release;
  access->copy = character_copy;
}
