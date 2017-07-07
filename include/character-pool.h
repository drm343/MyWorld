#ifndef HEADER_CHARACTER_POOL
#define HEADER_CHARACTER_POOL

#include <stdbool.h>

#include "setup_config.h"

#include "string_pool.h"
#include "character-ability.h"

typedef struct Character_Pool {
  Status_Access pool;
  uint8_t max_size;
  uint8_t current_size;

  void (*stop)(struct Character_Pool *);
  Status_Access (*malloc)(struct Character_Pool *);
  bool (*copy)(struct Character_Pool *, struct Character_Pool *, String);
  bool (*find)(struct Character_Pool *, Status_Access *, String);
  bool (*find_position)(struct Character_Pool *, Point_Access);
} Character_Pool_Type;
typedef Character_Pool_Type * Character_Pool_Access;

Character_Pool_Access Character_Pool_start_heap(uint8_t);
//void Character_Pool_start_stack(Character_Pool_Access, int);
#endif
