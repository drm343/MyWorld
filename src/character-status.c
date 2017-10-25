#include "character-status.h"


// -----------------------------------
// Init and free
// -----------------------------------
#define SUPER(name) C_BASE(name)
#define EXPORT(name) STATUS(name)


/** @brief 初始化角色
 * @param self 要初始化的角色
 *
 * 請確保傳入的物件不是 NULL
 */
void EXPORT(init) (Status_Access self) {
    SUPER(init) (&(self->base));
    self->faction = FACTION_NEUTRAL;
}


/** @brief 釋放角色
 * @param self 要釋放的角色
 *
 * 並不會直接釋放 self 本身的 Access，這部份通常透過 Status_Pool 管理。
 */
void EXPORT(free) (Status_Access self) {
    SUPER(free) (&(self->base));
    self->faction = FACTION_NEUTRAL;
}


/** @brief 複製角色資料
 * @param self 資料儲存的地方，通常為一個新的空角色
 * @param from 要被複製的角色
 *
 * 請確保傳進去的物件不是 NULL。
 *
 * 這個函數不會釋放資料，只會複製資料，如果資料本身是 Access，則直接複製 Access，
 * 因此請確保角色資料的 Access 不會在複製後被釋放。
 */
void EXPORT(copy) (Status_Access self, Status_Access from) {
    SUPER(copy) (&(self->base), &(from->base));

    self->race = from->race;
    self->faction = from->faction;
    self->damage = from->damage;
}


#ifdef DEBUG
/** @brief 顯示角色部份訊息，Debug 用
 * @param self 要顯示的角色
 */
void print_status(Status_Access self)
{
    DEBUG_PRINT("%s : %s\n", self->name, self->race);
    DEBUG_PRINT("Mark: %s\n", self->Mark->mark);
    DEBUG_PRINT("Damage: %d\n", self->damage);
}
#endif


// -----------------------------------------
// GET or SET BASE VALUE
// -----------------------------------------
/** @brief 設定角色名稱
 * @param self 要設定的角色
 * @param name 角色名稱
 */
void EXPORT(set_name) (Status_Access self, const char *name) {
    self->name = name;
}


/** @brief 設定角色種族
 * @param self 要設定的角色
 * @param race 角色種族
 */
void EXPORT(set_race) (Status_Access self, const char *race) {
    self->race = race;
}


/** @brief 設定角色圖形物件
 * @param self 要設定的角色
 * @param style 角色圖形
 */
void EXPORT(set_style) (Status_Access self, Style_Access style) {
    self->Mark = style;
}


/** @brief 設定角色圖形物件顯示的字
 * @param self 要設定的角色
 * @param mark 顯示文字
 *
 * @warning 此函數在後續設計可能被移到其他地方或是被移除
 */
void EXPORT(set_mark) (Status_Access self, const char *mark) {
    self->Mark->mark = mark;
}


// -----------------------------------------
// Action
// -----------------------------------------
/** @brief 攻擊其他角色
 * @param from 進行攻擊的角色
 * @param to 被攻擊的角色
 *
 * 目前只要受到 3 點以上的傷害就會死亡
 */
Is_Alive EXPORT(attack) (Status_Access from, Status_Access to) {
    to->damage += 1;
    switch (from->faction) {
        case FACTION_PLAYER:
            EXPORT(set_enemy) (to);
            break;
        case FACTION_ALLY:
            EXPORT(set_enemy) (to);
            break;
        case FACTION_ENEMY:
            EXPORT(set_ally) (to);
            break;
        case FACTION_NEUTRAL:
            EXPORT(set_enemy) (to);
            break;
        default:
            break;
    }

    if (to->damage >= 3) {
        to->is_alive = DEAD;
        to->crossable = true;
        to->attackable = false;
    }
    return to->is_alive;
}


// -----------------------------------------
// RELATION
// -----------------------------------------
/** @brief 角色成為盟友
 * @param self 轉換陣營的角色
 */
void EXPORT(set_ally) (Status_Access self) {
    if (self->faction != FACTION_PLAYER) {
        self->faction = RELATION_ALLY;
    }
}


/** @brief 角色成為敵人
 * @param self 轉換陣營的角色
 */
void EXPORT(set_enemy) (Status_Access self) {
    if (self->faction != FACTION_PLAYER) {
        self->faction = RELATION_ENEMY;
    }
}


/** @brief 角色成為中立方
 * @param self 轉換陣營的角色
 */
void EXPORT(set_neutral) (Status_Access self) {
    if (self->faction != FACTION_PLAYER) {
        self->faction = RELATION_NEUTRAL;
    }
}


/** @brief 隨機給予角色一個陣營
 * @param self 目標角色
 */
void EXPORT(set_random_relation) (Status_Access self) {
    int result = (rand() % 100) + 1;

    if (result <= 33) {
        EXPORT(set_ally) (self);
    } else if (result >= 67) {
        EXPORT(set_enemy) (self);
    } else {
        EXPORT(set_neutral) (self);
    }
}


/** @brief 取得角色陣營
 * @param self 目標角色
 * @return 回傳角色陣營
 */
Relation_Type EXPORT(get_relation_origin) (Status_Access self) {
    return self->faction;
}


/** @brief 取得角色陣營
 * @param self 目標角色
 * @return 回傳角色陣營文字
 */
char *EXPORT(get_relation_string) (Status_Access self) {
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


// -----------------------------------------
// POSITION
// -----------------------------------------
/** @brief 設定角色隨機座標
 * @param self 目標角色
 * @param max_x 最大 x 隨機值最大
 * @param max_y 最大 y 隨機值最大
 */
void EXPORT(set_random_position) (Status_Access self,
                                  int64_t max_x, int64_t max_y) {
    Point_Access_change(self->Real_Position);
    Point_Access_set_x(rand() % max_x);
    Point_Access_set_y(rand() % max_y);
}


/** @brief 取得角色真實座標
 * @param self 目標角色
 * @return 回傳座標
 */
Point_Access EXPORT(get_position) (Status_Access self) {
    return self->Real_Position;
}

#undef EXPORT
#undef SUPER
