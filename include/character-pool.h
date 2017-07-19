#ifndef HEADER_CHARACTER_POOL
#define HEADER_CHARACTER_POOL
#include "character-skill.h"
#include "helper_function.h"


typedef enum {
  NOT_FOUND = 0,
  FOUND
} Found_Result;

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
  Found_Result (*find)(Character_Pool_Access, Status_Access *, String);
  Found_Result (*find_position)(Character_Pool_Access, Status_Access *, Point_Access);

  void (*reset_graph_position)(Character_Pool_Access, int64_t, int64_t);
} Character_Pool_API;

extern Character_Pool_API character_pool;

#endif
