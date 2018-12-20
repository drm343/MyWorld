#include "Room_Pool.h"

/** file
 * 
 * 此結構與程式由 gpp 自動產生。
 * 請修改 BASE_POOL.gpp。
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
        item->is_used = false;
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

        if (item->is_used == false) {
            item->is_used = true;
            content = item->content;
            break;
        }
    }
    return content;
}
