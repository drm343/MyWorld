#include "container/Character_Pool.h"
/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

Character_Pool *Character_Pool_start(uint8_t max_size)
{
    Character_Pool *result = calloc(1, sizeof(Character_Pool));
    Character *instance = calloc(max_size, sizeof(Character));
    Character_Item *pool = calloc(max_size, sizeof(Character_Item));

    Character *content = NULL;
    Character_Item *item = NULL;

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

void Character_Pool_stop(Character_Pool * access)
{
    free(access->pool);
    free(access->item);
    free(access);
}

Character *Character_Pool_malloc(Character_Pool * access)
{
    Character *content = NULL;
    Character_Item *item = NULL;
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
