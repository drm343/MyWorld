#include "status.h"

// -----------------------------------
//Init and free
// -----------------------------------

/** @brief 初始化角色狀態
 * @param self 要初始化的角色狀態
 */
void Status_init(Status_Access self)
{
    self->is_alive = true;
    self->status = IN_USE;

    self->faction = FACTION_NEUTRAL;
}

/** @brief 釋放角色狀態
 * @param self 要釋放的角色狀態
 *
 * 並不會直接釋放 self 本身的 Access，這部份通常透過狀態池管理。
 */
void Status_free(Status_Access self)
{
    self->status = UNUSE;
    self->faction = FACTION_NEUTRAL;
}

/** @brief 複製角色狀態
 * @param self 空白的角色狀態
 * @param from 要被複製的角色狀態
 *
 * 請確保傳進去的物件不是 NULL。
 *
 * 這個函數不會釋放資料，只會複製資料，如果資料本身是 Access，則直接複製 Access，
 * 因此請確保角色資料的 Access 不會在複製後被釋放。
 */
void Status_copy(Status_Access self, Status_Access from)
{
    self->name = String_copy(from->name);

    self->is_alive = ALIVE;
    self->status = IN_USE;

    self->race = String_copy(from->race);
    self->faction = from->faction;
    self->damage = from->damage;
}

//-----------------------------------------
//Debug
// -----------------------------------------

#ifdef DEBUG
/** @brief 顯示角色部份訊息，Debug 用
 * @param self 要顯示的角色
 */
void Status_print_status(Status_Access self)
{
    DEBUG_PRINT("%s : %sn", self->name->str, self->race->str);
    DEBUG_PRINT("Damage: %dn", self->damage);
}
#endif

//-----------------------------------------
//GET or SET BASE VALUE
// -----------------------------------------

/** @brief 設定角色名稱
 * @param self 要設定的角色狀態
 * @param name 角色名稱
 */
void Status_set_name(Status_Access self, const char *name)
{
    if (self->name == NULL) {
        self->name = String_create(name);
    } else {
        String_free(self->name);
        self->name = String_create(name);
    }
}

/** @brief 設定角色種族
 * @param self 要設定的角色狀態
 * @param race 角色種族
 */
void Status_set_race(Status_Access self, const char *race)
{
    if (self->race == NULL) {
        self->race = String_create(race);
    } else {
        String_free(self->race);
        self->race = String_create(race);
    }
}

/** @brief 確認角色是否存活
 * @param self 要顯示的角色狀態
 * @return 角色存活狀態
 */
Is_Alive Status_is_alive(Status_Access self)
{
    return self->is_alive;
}

//-----------------------------------------
//Action
// -----------------------------------------

/** @brief 攻擊其他角色，修改對應狀態
 * @param from 進行攻擊的角色狀態
 * @param to 被攻擊的角色狀態
 *
 * 目前只要受到 3 點以上的傷害就會死亡
 */
Is_Alive Status_attack(Status_Access from, Status_Access to)
{
    to->damage += 1;
    switch (from->faction) {
        case FACTION_PLAYER:
            Status_set_enemy(to);
            break;
        case FACTION_ALLY:
            Status_set_enemy(to);
            break;
        case FACTION_ENEMY:
            Status_set_ally(to);
            break;
        case FACTION_NEUTRAL:
            Status_set_enemy(to);
            break;
        default:
            break;
    }

    if (to->damage >= 3) {
        to->is_alive = DEAD;
    }
    return to->is_alive;
}

//-----------------------------------------
//RELATION
// -----------------------------------------

/** @brief 角色成為盟友
 * @param self 轉換陣營的角色狀態
 */
void Status_set_ally(Status_Access self)
{
    if (self->faction != FACTION_PLAYER) {
        self->faction = FACTION_ALLY;
    }
}

/** @brief 角色成為敵人
 * @param self 轉換陣營的角色狀態
 */
void Status_set_enemy(Status_Access self)
{
    if (self->faction != FACTION_PLAYER) {
        self->faction = FACTION_ENEMY;
    }
}

/** @brief 角色成為中立方
 * @param self 轉換陣營的角色狀態
 */
void Status_set_neutral(Status_Access self)
{
    if (self->faction != FACTION_PLAYER) {
        self->faction = FACTION_NEUTRAL;
    }
}

/** @brief 隨機給予角色一個陣營
 * @param self 目標角色狀態
 */
void Status_set_random_relation(Status_Access self)
{
    int result = (rand() % 100) + 1;

    if (result <= 33) {
        Status_set_ally(self);
    } else if (result >= 67) {
        Status_set_enemy(self);
    } else {
        Status_set_neutral(self);
    }
}

/** @brief 取得角色陣營
 * @param self 目標角色狀態
 * @return 回傳角色陣營
 */
Faction_Type Status_get_relation_origin(Status_Access self)
{
    return self->faction;
}

/** @brief 取得角色陣營
 * @param self 目標角色狀態
 * @return 回傳角色陣營文字
 */
char *Status_get_relation_string(Status_Access self)
{
    char *result;

    switch (self->faction) {
        case FACTION_ALLY:
            result = "ally";
            break;
        case FACTION_ENEMY:
            result = "enemy";
            break;
        case FACTION_NEUTRAL:
            result = "neutral";
            break;
        default:
            result = "player";
            break;
    }
    return result;
}
