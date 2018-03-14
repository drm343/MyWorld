#ifndef HEADER_STATUS
#define HEADER_STATUS

#include <stdlib.h>

#include "helper/debug.h"
#include "namespace.h"

#include "base/type.h"
#include "message_base.h"

/** @brief Namespace STATUS
 */
#define EXPORT(name) STATUS(name)

/** @brief 角色生存狀態
 *
 * @note 未來可能需要加上 namespace。
 */
typedef enum {
    DEAD,
    ALIVE
} Is_Alive;

/** @brief 角色陣營
 *
 * 只有玩家會被標注為 FACTION_PLAYER
 */
typedef enum {
    FACTION_PLAYER,
    FACTION_ALLY,
    FACTION_ENEMY,
    FACTION_NEUTRAL
} Faction_Type;

/** @brief 與玩家角色的關係
 *
 * 非玩家角色透過這個 type 來設定(預計)
 */
typedef enum {
    RELATION_ALLY = FACTION_ALLY,
    RELATION_ENEMY = FACTION_ENEMY,
    RELATION_NEUTRAL = FACTION_NEUTRAL
} Relation_Type;

/** @brief 角色狀態結構
 */
typedef struct Status {
    const char *name;           /**< 角色名稱 */
    Is_Alive is_alive;          /**< 角色是否還存活，可以依靠此資訊取代 crossable */
    Use_Type status;            /**< 是否使用中，可以依靠 Character_Pool 來分辨，後續會移除 */

    const char *race;           /**< 角色種族 */
    Faction_Type faction;       /**< 角色跟玩家的關係 */
    Natural damage;             /**< 目前受到的傷害 */
} Status;

/** @brief 角色狀態物件
 *
 * 參數的傳入、傳出，都是透過物件進行，建議直接使用 Status_Access 取代 Status *。
 */
typedef Status *Status_Access;

//-----------------------------------
//Init and free
// -----------------------------------

/** @brief 初始化角色狀態
 * @param self 要初始化的角色狀態
 */
void EXPORT(init) (Status_Access self);

/** @brief 釋放角色狀態
 * @param self 要釋放的角色狀態
 *
 * 並不會直接釋放 self 本身的 Access，這部份通常透過狀態池管理。
 */
void EXPORT(free) (Status_Access self);

/** @brief 複製角色狀態
 * @param self 空白的角色狀態
 * @param from 要被複製的角色狀態
 *
 * 請確保傳進去的物件不是 NULL。
 *
 * 這個函數不會釋放資料，只會複製資料，如果資料本身是 Access，則直接複製 Access，
 * 因此請確保角色資料的 Access 不會在複製後被釋放。
 */
void EXPORT(copy) (Status_Access self, Status_Access from);

//-----------------------------------------
//Debug
// -----------------------------------------

#ifdef DEBUG
/** @brief 顯示角色部份狀態，Debug 用
 * @param self 要顯示的角色狀態
 */
void EXPORT(print_status) (Status_Access self);
#endif

//-----------------------------------------
//GET or SET BASE VALUE
// -----------------------------------------

/** @brief 設定角色名稱
 * @param self 要設定的角色狀態
 * @param name 角色名稱
 */
void EXPORT(set_name) (Status_Access self, const char *name);

/** @brief 設定角色種族
 * @param self 要設定的角色狀態
 * @param race 角色種族
 */
void EXPORT(set_race) (Status_Access self, const char *race);

/** @brief 確認角色是否存活
 * @param self 要顯示的角色狀態
 * @return 角色存活狀態
 */
Is_Alive EXPORT(is_alive) (Status_Access self);

//-----------------------------------------
//Action
// -----------------------------------------

/** @brief 攻擊其他角色，修改對應狀態
 * @param from 進行攻擊的角色狀態
 * @param to 被攻擊的角色狀態
 *
 * 目前只要受到 3 點以上的傷害就會死亡
 */
Is_Alive EXPORT(attack) (Status_Access from, Status_Access to);

//-----------------------------------------
//RELATION
// -----------------------------------------

/** @brief 角色成為盟友
 * @param self 轉換陣營的角色狀態
 */
void EXPORT(set_ally) (Status_Access self);

/** @brief 角色成為敵人
 * @param self 轉換陣營的角色狀態
 */
void EXPORT(set_enemy) (Status_Access self);

/** @brief 角色成為中立方
 * @param self 轉換陣營的角色狀態
 */
void EXPORT(set_neutral) (Status_Access self);

/** @brief 隨機給予角色一個陣營
 * @param self 目標角色狀態
 */
void EXPORT(set_random_relation) (Status_Access self);

/** @brief 取得角色陣營
 * @param self 目標角色狀態
 * @param return_variable 回傳到哪個變數
 *
 * 根據回傳變數的型別決定回傳哪種類型的陣營值
 */
#define Status_get_relation(self, return_variable) \
return_variable = _Generic((return_variable), \
Relation_Type: Status_get_relation_origin, \
char *: Status_get_relation_string, \
default: Status_get_relation_origin)(self)

/** @brief 取得角色陣營
 * @param self 目標角色狀態
 * @return 回傳角色陣營
 */
Relation_Type EXPORT(get_relation_origin) (Status_Access self);

/** @brief 取得角色陣營
 * @param self 目標角色狀態
 * @return 回傳角色陣營文字
 */
char *EXPORT(get_relation_string) (Status_Access self);

#undef EXPORT
#endif
