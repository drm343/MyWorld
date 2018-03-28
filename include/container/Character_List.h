#ifndef HEADER_CONTAINER_LIST_Character_List
#define HEADER_CONTAINER_LIST_Character_List

#include <stdint.h>
#include "character.h"

/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

typedef struct _Character_Node {
    struct _Character_Node *next;
    struct _Character_Node *previous;
    Character *content;
} Character_Node;

typedef struct {
    Character_Node *list;
    uint8_t max_size;
    uint8_t used;
    uint8_t instance_counter;
    Character_Node *first;
    Character_Node *last;
} Character_List;

Character_List* Character_List_start(uint8_t max_size);
void Character_List_stop(Character_List *access);
void Character_List_gc(Character_List *access);
uint8_t Character_List_insert(Character_List *access, Character *item);
void Character_List_remove(Character_List *access, Character *item);
Character* Character_List_get_by_index(Character_List *access, uint8_t index);
void Character_List_copy_all(Character_List *from, Character_List *to);
#endif