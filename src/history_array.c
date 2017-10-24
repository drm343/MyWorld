#include "container/History_Array.h"
/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

History_Array *History_Array_start(uint8_t max_size)
{
    History_Array *self = calloc(1, sizeof(History_Array));
    uint32_t *array = calloc(max_size + 1, sizeof(uint32_t));

    self->array = array;
    self->max_size = max_size + 1;
    self->buffer = 0;
    self->start = 0;
    return self;
}

void History_Array_stop(History_Array * self)
{
    free(self->array);
    free(self);
}

uint32_t *History_Array_insert(History_Array * self, uint32_t item)
{
    self->array[self->buffer] = item;
    self->buffer = self->buffer + 1;

    if (self->buffer >= self->max_size) {
        self->buffer = self->buffer - self->max_size;
    }

    if (self->buffer == self->start) {
        self->start = self->start + 1;
        return &(self->array[self->buffer]);
    }
    return NULL;
}

uint8_t History_Array_index(History_Array * self)
{
    return self->start;
}

bool History_Array_next(History_Array * self, uint8_t * next)
{
    uint8_t counter = *next;
    counter = counter + 1;

    if (counter >= self->max_size) {
        counter = 0;
    }

    if (counter == self->buffer) {
        return false;
    } else {
        *next = counter;
        return true;
    }
}

uint8_t History_Array_last(History_Array * self)
{
    uint8_t result =
        (self->buffer == 0) ? self->max_size - 1 : self->buffer - 1;
    return result;
}

bool History_Array_previous(History_Array * self, uint8_t * previous)
{
    uint8_t counter =
        (*previous == 0) ? self->max_size - 1 : *previous - 1;
    uint8_t end_value =
        (self->start == 0) ? self->max_size - 1 : self->start - 1;

    if (counter == end_value) {
        return false;
    } else {
        *previous = counter;
        return true;
    }
}

uint32_t History_Array_get_item(History_Array * self, uint8_t next)
{
    return self->array[next];
}
