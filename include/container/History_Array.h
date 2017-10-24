#ifndef HEADER_CONTAINER_CYCLE_ARRAY_History_Array_WITH_BASE_TYPE
#define HEADER_CONTAINER_CYCLE_ARRAY_History_Array_WITH_BASE_TYPE

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

typedef struct {
    uint32_t *array;
    uint8_t max_size;
    uint8_t buffer;
    uint8_t start;
} History_Array;

History_Array *History_Array_start(uint8_t max_size);
void History_Array_stop(History_Array * self);
uint32_t *History_Array_insert(History_Array * self, uint32_t item);
uint8_t History_Array_index(History_Array * self);
bool History_Array_next(History_Array * self, uint8_t * next);
uint8_t History_Array_last(History_Array * self);
bool History_Array_previous(History_Array * self, uint8_t * previous);
uint32_t History_Array_get_item(History_Array * self, uint8_t next);
#endif
