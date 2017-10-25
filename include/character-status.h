#ifndef HEADER_CHARACTER_STATUS
#define HEADER_CHARACTER_STATUS

#include "namespace.h"

#include "character.h"
#include "message_base.h"


/** @brief Namespace STATUS
 */
#define EXPORT(name) STATUS(name)


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


/** @brief 角色結構
     *
     * @warning 不要透過匿名結構同時使用有同名欄位的兩個結構，請確保只會單一繼承結構，如同 Objective-C。
 */
typedef struct Status {
    /** 繼承基本角色資料
     *
     * 使用了 C11 的匿名結構，在使用結構而未給予名稱的狀況下，新結構內部會把舊結構成員當
     * 成自己的。
     *
     * 透過 union 包覆兩個相同結構後，因為兩者結構相同，結構成員的 Access 一定會相同，
     * 然後就能透過 self->base 的方式使用原結構的函數。
     *
     * 如果不想用 union 處理 Access 問題，則必須自己手動轉型，或是開 plan9-extensions 來確保有正確轉型成舊結構的型別，但如果不能確保匿名結構一定會在結構開頭，或是用了兩個以上的匿名結構，還是建議採用 union 的做法。
     */
    union {
        Character_Base;
        Character_Base base;
    };

    const char *race; /**< 角色種族 */
    Faction_Type faction; /**< 角色跟玩家的關係 */
    Natural damage; /**< 目前受到的傷害 */
} Status;


/** @brief 角色物件
 *
 * 參數的傳入、傳出，都是透過物件進行，建議直接使用 Status_Access 取代 Status *。
 */
typedef Status *Status_Access;


/** @brief 初始化角色
 * @param self 要初始化的角色
 */
void EXPORT(init) (Status_Access self);


/** @brief 釋放角色
 * @param self 要釋放的角色
 *
 * 並不會直接釋放 self 本身的 Access，這部份通常透過 Status_Pool 管理。
 */
void EXPORT(free) (Status_Access self);


/** @brief 複製角色資料
 * @param self 資料儲存的地方，通常為一個新的空角色
 * @param from 要被複製的角色
 *
 * 請確保傳進去的物件不是 NULL。
 *
 * 這個函數不會釋放資料，只會複製資料，如果資料本身是 Access，則直接複製 Access，
 * 因此請確保角色資料的 Access 不會在複製後被釋放。
 */
void EXPORT(copy) (Status_Access self, Status_Access from);


#ifdef DEBUG
/** @brief 顯示角色部份訊息，Debug 用
 * @param self 要顯示的角色
 */
void EXPORT(print_status) (Status_Access self);
#endif


/** @brief 設定角色名稱
 * @param self 要設定的角色
 * @param name 角色名稱
 */
void EXPORT(set_name) (Status_Access self, const char *name);


/** @brief 設定角色種族
 * @param self 要設定的角色
 * @param race 角色種族
 */
void EXPORT(set_race) (Status_Access self, const char *race);


/** @brief 設定角色圖形
 * @param self 要設定的角色
 * @param style 角色圖形
 */
void EXPORT(set_style) (Status_Access self, Style_Access style);


/** @brief 設定角色圖形物件顯示的字
 * @param self 要設定的角色
 * @param mark 顯示文字
 *
 * @warning 此函數在後續設計可能被移到其他地方或是被移除
 */
void EXPORT(set_mark) (Status_Access self, const char *mark);


/** @brief 攻擊其他角色
 * @param from 進行攻擊的角色
 * @param to 被攻擊的角色
 *
 * 目前只要受到 3 點以上的傷害就會死亡
 */
Is_Alive EXPORT(attack) (Status_Access from, Status_Access to);


/** @brief 角色成為盟友
 * @param self 轉換陣營的角色
 */
void EXPORT(set_ally) (Status_Access self);


/** @brief 角色成為敵人
 * @param self 轉換陣營的角色
 */
void EXPORT(set_enemy) (Status_Access self);


/** @brief 角色成為中立方
 * @param self 轉換陣營的角色
 */
void EXPORT(set_neutral) (Status_Access self);


/** @brief 取得角色陣營
 * @param self 目標角色
 * @param return_variable 回傳到哪個變數
 *
 * 根據變數 type 決定回傳哪種類型的陣營值
 */
#define Status_get_relation(self, return_variable) \
return_variable = _Generic((return_variable), \
Relation_Type: Status_get_relation_origin, \
char *: Status_get_relation_string, \
default: Status_get_relation_origin)(self)


/** @brief 取得角色陣營
 * @param self 目標角色
 * @return 回傳角色陣營
 */
Relation_Type EXPORT(get_relation_origin) (Status_Access self);


/** @brief 取得角色陣營
 * @param self 目標角色
 * @return 回傳角色陣營文字
 */
char *EXPORT(get_relation_string) (Status_Access self);


/** @brief 隨機給予角色一個陣營
 * @param self 目標角色
 */
void EXPORT(set_random_relation) (Status_Access self);


/** @brief 設定角色隨機座標
 * @param self 目標角色
 * @param max_x 最大 x 隨機值最大
 * @param max_y 最大 y 隨機值最大
 */
void EXPORT(set_random_position) (Status_Access self, int64_t max_x,
                                  int64_t max_y);


/** @brief 取得角色真實座標
 * @param self 目標角色
 * @return 回傳座標
 */
Point_Access EXPORT(get_position) (Status_Access self);

#undef EXPORT
#endif
