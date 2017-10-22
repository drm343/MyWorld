#ifndef HEADER_CHARACTER_STATUS
#define HEADER_CHARACTER_STATUS

#include "character.h"
#include "message_base.h"

typedef enum {
    DEAD,
    ALIVE
} Is_Alive;


typedef enum {
    FACTION_PLAYER,
    FACTION_ALLY,
    FACTION_ENEMY,
    FACTION_NEUTRAL
} Faction_Type;


typedef enum {
    RELATION_ALLY = FACTION_ALLY,
    RELATION_ENEMY,
    RELATION_NEUTRAL
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


/** @brief 角色專用 api 變數結構
 *
 * @warning 之後會去掉變數結構改成純函數。
 */
typedef struct {
    void (*init) (Status_Access);
    void (*free) (Status_Access);

    void (*copy) (Status_Access, Status_Access);

#ifdef DEBUG
    void (*print_status) (Status_Access);
#endif

    void (*set_name) (Status_Access, const char *);
    void (*set_race) (Status_Access, const char *);
    void (*set_style) (Status_Access, Style_Access);
    void (*set_mark) (Status_Access, const char *);

     Is_Alive(*is_alive) (Status_Access);
     Relation_Type(*get_relation) (Status_Access);
    char *(*get_relation_string) (Status_Access);
    void (*set_relation_ally) (Status_Access);
    void (*set_relation_enemy) (Status_Access);
    void (*set_relation_neutral) (Status_Access);

     Is_Alive(*attack) (Status_Access, Status_Access);
    void (*set_random_position) (Status_Access, int64_t, int64_t);
    void (*set_random_relation) (Status_Access);

     Point_Access(*get_position) (Status_Access);
} Character_API;

extern Character_API character;

#endif
