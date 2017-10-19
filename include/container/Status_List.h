#ifndef HEADER_CONTAINER_LIST_Status_List
#define HEADER_CONTAINER_LIST_Status_List

#include <stdint.h>
#include "character-status.h"

/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

typedef struct _Status_Node {
    struct _Status_Node *next;
    struct _Status_Node *previous;
    Status *content;
} Status_Node;

typedef struct {
    Status_Node *list;
    uint8_t max_size;
    uint8_t used;
    uint8_t instance_counter;
    Status_Node *first;
    Status_Node *last;
} Status_List;

Status_List *Status_List_start(uint8_t max_size);
void Status_List_stop(Status_List * access);
void Status_List_gc(Status_List * access);
uint8_t Status_List_insert(Status_List * access, Status * item);
void Status_List_remove(Status_List * access, Status * item);
Status *Status_List_get_by_index(Status_List * access, uint8_t index);
void Status_List_copy_all(Status_List * from, Status_List * to);
#endif
