#include <stdio.h>
#include <stdlib.h>

#include "pool.h"


Pool *Pool_start(uint8_t max_size, void *(*create_content)(void), void (*free_content)(void *))
{
    Pool *result = calloc(1, sizeof(Pool));
    Item *pool = calloc(max_size, sizeof(Item));

    Item *item = NULL;

    result->pool = pool;
    result->max_size = max_size;
    result->create_content = create_content;
    result->free_content = free_content;

    for (uint8_t index = 0; index < max_size; index++) {
        item = &(result->pool[index]);
        item->is_used = false;
        item->content = NULL;
    }
    return result;
}

void Pool_stop(Pool * access)
{
    Item *item = NULL;
    uint8_t max_size = access->max_size;

    for (uint8_t index = 0; index < max_size; index++) {
        item = &(access->pool[index]);

        access->free_content(item->content);
    }

    free(access->pool);
    free(access);
}


Item *Pool_malloc(Pool * access, void(*init)(void *))
{
    Item *item = NULL;
    uint8_t max_size = access->max_size;

    for (uint8_t index = 0; index < max_size; index++) {
        item = &(access->pool[index]);

        if (item->is_used == false) {
            item->is_used = true;
            if (item->content == NULL) {
                item->content = access->create_content();
            }
            if (init != NULL) {
                init(item->content);
            }
            break;
        }
    }
    return item;
}


void *Item_content(Item * access)
{
    return access->content;
}


void Item_free(Item * access)
{
    access->is_used = false;
}
