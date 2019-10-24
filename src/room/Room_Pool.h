#ifndef HEADER_Room_Pool
#define HEADER_Room_Pool

#include <stdint.h>
#include <stdbool.h>

#include "room.h"


typedef struct Room_Item {
    Room *content;
    bool is_used;
} Room_Item;

typedef struct {
    Room_Item *pool;
    Room *item;
    uint8_t max_size;
} Room_Pool;

Room_Pool *Room_Pool_start(uint8_t max_size);
void Room_Pool_stop(Room_Pool * access);
Room *Room_Pool_malloc(Room_Pool * access);
#endif
