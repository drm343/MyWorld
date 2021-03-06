#ifndef HEADER_CHARACTER_FACTORY
#define HEADER_CHARACTER_FACTORY

#include "Character_Pool.h"
#include "Status_Pool.h"

/** @brief 用來建立角色的預備池
 */
typedef struct {
    Character_Pool *character;
    Status_Pool *status;
} Character_Factory;

/** @brief 啟動角色建立工廠
 * @param max_size 最大實例數
 * @return 角色建立工廠
 */
Character_Factory *Character_Factory_start(uint8_t max_size);

/** @brief 關閉角色建立工廠
 * @param self 角色建立工廠
 */
void Character_Factory_stop(Character_Factory * self);

/** @brief 建立新的空白角色卡
 * @param self 角色建立工廠
 * @return 空白角色，可以做後續設定
 */
Character_Access Character_Factory_malloc(Character_Factory * self);
#endif
