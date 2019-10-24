#ifndef HEADER_Pool
#define HEADER_Pool

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>


typedef struct Item {
    void *content;
    bool is_used;
} Item;


typedef struct {
    Item *pool;
    uint8_t max_size;

    void *(*create_content)(void);
    void (*free_content)(void *);
} Pool;


Pool *Pool_start(uint8_t max_size, void *(*create_content)(void), void (*free_content)(void *));
void Pool_stop(Pool * access);
Item *Pool_malloc(Pool * access, void(*init)(void *));
void *Item_content(Item * access);
void Item_free(Item * access);
#endif
