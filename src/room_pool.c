#include "container/Room_Pool.h"
/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

Room_Pool *Room_Pool_start(uint8_t max_size)
{
    Room_Pool *result = calloc(1, sizeof(Room_Pool));
    Room *instance = calloc(max_size, sizeof(Room));
    Room_Item *pool = calloc(max_size, sizeof(Room_Item));

    Room *content = NULL;
    Room_Item *item = NULL;

    result->pool = pool;
    result->item = instance;
    result->max_size = max_size;

    for (uint8_t index = 0; index < max_size; index++) {
        content = &(result->item[index]);

        item = &(result->pool[index]);
        item->is_used = UNUSE;
        item->content = content;
    }
    return result;
}

void Room_Pool_stop(Room_Pool * access)
{
    free(access->pool);
    free(access->item);
    free(access);
}

Room *Room_Pool_malloc(Room_Pool * access)
{
    Room *content = NULL;
    Room_Item *item = NULL;
    uint8_t max_size = access->max_size;

    for (uint8_t index = 0; index < max_size; index++) {
        item = &(access->pool[index]);

        if (item->is_used == UNUSE) {
            item->is_used = IN_USE;
            content = item->content;
            break;
        }
    }
    return content;
}
