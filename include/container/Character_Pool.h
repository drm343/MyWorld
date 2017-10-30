#ifndef HEADER_CONTAINER_POOL_Character_Pool
#define HEADER_CONTAINER_POOL_Character_Pool

#include <stdint.h>
#include <stdbool.h>

#include "character.h"

/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。

 */

typedef struct _Character_Item {
    Character *content;
    bool is_used;
} Character_Item;

typedef struct {
    Character_Item *pool;
    Character *item;
    uint8_t max_size;
} Character_Pool;

Character_Pool *Character_Pool_start(uint8_t max_size);
void Character_Pool_stop(Character_Pool * access);
Character *Character_Pool_malloc(Character_Pool * access);
#endif
