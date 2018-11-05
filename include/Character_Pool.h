#ifndef HEADER_CHARACTER_LIST
#define HEADER_CHARACTER_LIST

#include <stdint.h>
#include "List.h"
#include "character.h"

#define CHARA_LIST_POOL(name) Character_List_Pool_##name

typedef List Character_List_Pool;

Character_List_Pool CHARA_LIST_POOL(create) (void);
#endif
