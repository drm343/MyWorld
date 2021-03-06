#include "character_factory.h"

/** @brief 啟動角色建立工廠
 * @param max_size 最大實例數
 * @return 角色建立工廠
 */
Character_Factory *Character_Factory_start(uint8_t max_size)
{
    Character_Factory *self = calloc(1, sizeof(Character_Factory));
    self->character = Character_Pool_start(max_size);
    self->status = Status_Pool_start(max_size);
    return self;
}

/** @brief 關閉角色建立工廠
 * @param self 角色建立工廠
 */
void Character_Factory_stop(Character_Factory * self)
{
    Status_Pool_stop(self->status);
    Character_Pool_stop(self->character);
    free(self);
}

/** @brief 建立新的空白角色卡
 * @param self 角色建立工廠
 * @return 空白角色，可以做後續設定
 */
Character_Access Character_Factory_malloc(Character_Factory * self)
{
    Character_Access item = Character_Pool_malloc(self->character);
    item->status = Status_Pool_malloc(self->status);

    Status_init(item->status);
    return item;
}
