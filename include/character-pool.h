#ifndef HEADER_CHARACTER_POOL_NEW
#define HEADER_CHARACTER_POOL_NEW

#include "character-skill.h"
#include "setup_config.h"


GENERIC_POOL(Status_Pool, Status_Access);
typedef Status_Pool * Status_Pool_Access;

GENERIC_POOL(Character_Base_Pool, Character_Base_Access);
typedef Character_Base_Pool * Character_Base_Pool_Access;


typedef struct {
  Status_Pool_Access status;
  Character_Base_Pool_Access base;
} Character_Pool_Type;
typedef Character_Pool_Type * Character_Pool_Access;


typedef struct {
  Character_Pool_Access (*start)(uint8_t);
  void (*stop)(Character_Pool_Access);
  Status_Access (*malloc)(Character_Pool_Access);
  bool (*copy)(Character_Pool_Access, Character_Pool_Access, String);
  bool (*find)(Character_Pool_Access, Status_Access *, String);
  bool (*find_position)(Character_Pool_Access, Point_Access);
} Character_Pool_API;

extern Character_Pool_API character_pool;

#endif
