#ifndef HEADER_CHARACTER
#define HEADER_CHARACTER

#include <stdbool.h>

#include "base_type.h"
#include "string_pool.h"
#include "graphic.h"


typedef struct Character_Base {
  String name;
  Style_Access Mark;
  Point_Type Real_Position;
  Point_Type Graph_Position;
  bool crossable;
  bool attackable;
  bool is_alive;
  Use_Type status;

  void (*release)(struct Character_Base *);
  void (*copy)(struct Character_Base *, struct Character_Base *);
} Character_Base_Type;
typedef Character_Base_Type * Character_Base_Access;


void character_base_init(Character_Base_Access);
#endif
