#include "game_status.h"

#include "List-Faction_Group.h"
#include "factory/character_factory.h"


// ----------------------------------------------
//Internal Object Struct
// ----------------------------------------------
/** @brief 遊戲狀態結構
 *
 * 建議直接使用結構名稱 Game_Status，而不用 struct Game_Status。
 */
typedef struct Game_Status {
    Character_Access player; /**< 玩家本身 */
    Character_Factory *prepare; /**< 種族池 */
    Faction_List race;       /**< 實體化種族 */
    Character_Factory *used_pool;       /**< 實體化角色用的記憶體空間 */
    Faction_Group group;       /**< 實體化角色 */
    Faction_List faction_player;       /**< 玩家自身 */
    Faction_List ally;       /**< 盟友 */
    Faction_List enemy;      /**< 敵人 */
    Faction_List neutral;    /**< 中立 */
    Faction_List current;    /**< 使用在 iterator 中 */
} Game_Status;


//----------------------------------------------
//External API
// ----------------------------------------------
Message_Type Point_over_there(Point self, Point other)
{
    Message_Type result = DO_NOTHING;

    int32_t diff_x = Point_x(self) - Point_x(other);
    int32_t diff_y = Point_y(self) - Point_y(other);
    int32_t abs_x = abs(diff_x);
    int32_t abs_y = abs(diff_y);

    if (abs_x >= abs_y) {
        if (diff_x >= 0) {
            result = LEFT;
        } else {
            result = RIGHT;
        }
    } else {
        if (diff_y >= 0) {
            result = TOP;
        } else {
            result = DOWN;
        }
    }
    return result;
}


Message_Type Point_near_by(Point self, Point other)
{
    Message_Type result = DO_NOTHING;

    int32_t diff_x = Point_x(self) - Point_x(other);
    int32_t diff_y = Point_y(self) - Point_y(other);
    int32_t abs_x = abs(diff_x);
    int32_t abs_y = abs(diff_y);

    if (abs_x >= abs_y) {
        if (diff_x >= 2) {
            result = LEFT;
        } else if (diff_x <= -2) {
            result = RIGHT;
        } else {
            goto RANDOM_POSITION;
        }
    } else {
        if (diff_y >= 2) {
            result = TOP;
        } else if (diff_y <= -2) {
            result = DOWN;
        } else {
            goto RANDOM_POSITION;
        }
    }
    goto DONE;

  RANDOM_POSITION:
    switch (rand() % 4) {
        case 1:
            result = TOP;
            break;
        case 2:
            result = DOWN;
            break;
        case 3:
            result = LEFT;
            break;
        default:
            result = RIGHT;
            break;
    }

  DONE:
    return result;
}


static void reset_graph_position(Faction_List list, Rectangle rectangle)
{
    Point start_point = RECT(position) (rectangle);
    Point end_point = RECT(extent) (rectangle);

    int64_t x = Point_x(start_point);
    int64_t y = Point_y(start_point);

    int64_t max_x = Point_x(end_point);
    int64_t max_y = Point_y(end_point);

    Character_Access npc = NULL;
    int64_t counter_x = 0;
    int64_t counter_y = 0;

    for (npc = list->reset_iterator(list); npc != NULL;
         npc = list->next(list, npc)) {
        counter_x = npc->Real_Position->x - x;
        counter_y = npc->Real_Position->y - y;

        if (((counter_x >= 0) || (counter_y >= 0)) &&
            ((counter_x < max_x) && (counter_y < max_y - 1))) {
            Point_set(npc->Graph_Position,.x = counter_x,.y = counter_y);
        } else {
            Point_set(npc->Graph_Position,.x = -1,.y = -1);
        }
    }
}


static void reset_group_graph_position(Faction_Group group,
                                       Rectangle rectangle)
{
    for (Faction_List list = group->reset_iterator(group); list != NULL;
         list = group->next(group, list)) {
        reset_graph_position(list, rectangle);
    }
}

//----------------------------------------------
//Internal API
// ----------------------------------------------
/** @brief 設定種族池最大值
 * @param access 要使用的 Game_Status
 * @param max_size 想設定的最大值
 *
 * @warning 只能設定一次
*/
static void set_prepare(Game_Status_Access access, uint8_t max_size)
{
    Character_Factory *prepare = access->prepare;

    if (prepare == NULL) {
        access->prepare = Character_Factory_start(max_size);
    }
    Faction_List race = access->race;

    if (race == NULL) {
        access->race = FACTION(create) ();
    }
}

/** @brief 設定實體化的遊戲狀態最大值
 * @param access 要使用的 Game_Status
 * @param max_size 想設定的最大值
 *
 * @warning 之後可能會被移出
 *
 * @warning 只能設定一次
*/
static void init_group(Game_Status * access, uint8_t max_size)
{
    Character_Factory *used_pool = access->used_pool;

    if (used_pool == NULL) {
        access->used_pool = Character_Factory_start(max_size);
    }
    Faction_Group group = access->group;

    if (group == NULL) {
        access->group = F_GROUP(create) ();
    }
}

static Found_Result pool_find(Faction_List list,
                              Character_Access * npc, const char *race)
{
    ImmutableString find_race = String_create(race);
    Found_Result result = NOT_FOUND;

    for (*npc = list->reset_iterator(list); *npc != NULL;
         *npc = list->next(list, *npc)) {
        ImmutableString npc_race = (*npc)->status->race;
        if (String_equal(npc_race, find_race) == YES) {
            result = FOUND;
            goto DONE;
        }
    }
    *npc = NULL;

  DONE:
    String_free(find_race);
    return result;
}

/** @brief 分配一個半完成初始化的角色實體
 * @param self 要使用的遊戲狀態
 * @param race 角色種族
 * @param name 角色名稱
 * @param map 使用的地圖
 * @return 半實體化的角色 Access
 *
 * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
 * 設定。
 */
static Character_Access use_npc(Game_Status * self, const char *race,
                                const char *name, Map_Access map)
{
    Character_Access origin_npc = NULL;
    Character_Access npc = NULL;

    Found_Result result = pool_find(self->race, &origin_npc, race);

    if (result == FOUND) {
        npc = Character_Factory_malloc(self->used_pool);
        STATUS(copy) (npc->status, origin_npc->status);

        STATUS(set_name) (npc->status, name);

        npc->Mark = origin_npc->Mark;

        npc->Real_Position = Point_create();
        npc->Graph_Position = Point_create();

        Point bottom_right = MAP(bottom_right) (map);
        CHARA(set_random_position) (npc, Point_x(bottom_right),
                                    Point_y(bottom_right));

        Point_set(npc->Graph_Position,.x = -1,.y = -1);
    }
    return npc;
}


static Message_Type player_reaction(Character_Access self,
                                    Faction_List enemy_group)
{
    bool is_alive = self->status->is_alive;

    Message_Type result = DO_NOTHING;
    SDL_Event event;

    if (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                result = QUIT;
                break;
            case SDL_KEYDOWN:
                if (is_alive == YES) {
                    goto CHECK_KEYDOWN;
                } else {
                    result = QUIT;
                }
                break;
            default:
                break;
        }
    }
    goto DONE;

  CHECK_KEYDOWN:
    switch (event.key.keysym.sym) {
        case SDLK_UP:
            result = TOP;
            break;
        case SDLK_DOWN:
            result = DOWN;
            break;
        case SDLK_LEFT:
            result = LEFT;
            break;
        case SDLK_RIGHT:
            result = RIGHT;
            break;
        case SDLK_q:
            result = QUIT;
            break;
        default:
            break;
    }
  DONE:
    return result;
}


/** @brief 設定玩家本身的陣營
 * @param access 要使用的 Game_Status
 */
static void init_player(Game_Status * access)
{
    Faction_List faction_player = access->faction_player;

    if (faction_player == NULL) {
        access->faction_player = FACTION(create) ();
        FACTION(set_name) (access->faction_player, "player");
        FACTION(set_action) (access->faction_player, player_reaction);
    }
    F_GROUP(link) (access->group, access->faction_player);
}


static Message_Type npc_reaction(Character_Access self,
                                 Faction_List enemy_group)
{
    Character_Access target = FACTION(get_random_target) (enemy_group);

    if (target == NULL) {
        return DO_NOTHING;
    }
    Point target_position = CHARA(get_position) (target);
    Point self_position = CHARA(get_position) (self);
    return Point_over_there(self_position, target_position);
}


/** @brief 設定友方角色總量的上限值
 * @param access 要使用的 Game_Status
 */
static void init_ally(Game_Status * access)
{
    Faction_List ally = access->ally;

    if (ally == NULL) {
        access->ally = FACTION(create) ();
        FACTION(set_name) (access->ally, "ally");
        FACTION(set_action) (access->ally, npc_reaction);
    }
    F_GROUP(link) (access->group, access->ally);
}


static Message_Type enemy_reaction(Character_Access self,
                                   Faction_List enemy_group)
{
    Faction_List target_list = F_GROUP(get_random_target) (enemy_group);
    return npc_reaction(self, target_list);
}


/** @brief 設定敵方角色總量的上限值
 * @param access 要使用的 Game_Status
 */
static void init_enemy(Game_Status * access)
{
    Faction_List enemy = access->enemy;

    if (enemy == NULL) {
        access->enemy = FACTION(create) ();
        FACTION(set_name) (access->enemy, "enemy");
        FACTION(set_action) (access->ally, enemy_reaction);
    }
    F_GROUP(link) (access->group, access->enemy);
}


static Message_Type neutral_npc_reaction(Character_Access self,
                                         Faction_List follow_faction)
{
    Character_Access player =
        follow_faction->reset_iterator(follow_faction);
    Point target_position = CHARA(get_position) (player);
    Point self_position = CHARA(get_position) (self);
    return Point_near_by(self_position, target_position);
}


/** @brief 設定中立角色總量的上限值
 * @param access 要使用的 Game_Status
 */
static void init_neutral(Game_Status * access)
{
    Faction_List neutral = access->neutral;

    if (neutral == NULL) {
        access->neutral = FACTION(create) ();
        FACTION(set_name) (access->neutral, "neutral");
        FACTION(set_action) (access->neutral, neutral_npc_reaction);
    }
    F_GROUP(link) (access->group, access->neutral);
}

/** @brief 設定 NPC 為敵方角色
 * @param access 要使用的 Game_Status
 * @param npc 要設定的角色
 */
static void add_enemy(Game_Status * access, Character_Access npc)
{
    Faction_List enemy = access->enemy;
    enemy->insert(enemy, npc);
}

/** @brief 設定 NPC 為友方角色
 * @param access 要使用的 Game_Status
 * @param npc 要設定的角色
 */
static void add_ally(Game_Status * access, Character_Access npc)
{
    Faction_List ally = access->ally;
    ally->insert(ally, npc);
}

/** @brief 設定 NPC 為中立角色
 * @param access 要使用的 Game_Status
 * @param npc 要設定的角色
 */
static void add_neutral(Game_Status * access, Character_Access npc)
{
    Faction_List neutral = access->neutral;
    neutral->insert(neutral, npc);
}

//----------------------------------------------
//Internal Finding API
// ----------------------------------------------
/*
 * static bool pool_copy(Base_Pool_Access from, Base_Pool_Access to, char
 * *name) { uint8_t count = 0; uint8_t used = from->status->max_size -
 * from->status->current_size; Status_Access from_status = NULL;
 * Status_Access to_status = NULL;
 *
 * for (count; count < used; count++) { from_status =
 * &(from->status->pool[count]);
 *
 * if ([from_status->name isEqualToString: name] == YES) { to_status =
 * [Game_Status malloc: to]; character.copy(to_status, from_status);
 * character.set_name(to_status, name); return true; } } return false; }
 */

static bool pool_all_copy(Status_Pool * from, Status_Pool * to)
{
    uint8_t used = from->max_size;
    Status_Access from_status = NULL;
    Status_Access to_status = NULL;

    for (uint8_t count = 0; count < used; count++) {
        Status_Item *from_item = &(from->pool[count]);

        if (from_item->is_used == IN_USE) {
            Status *from_status = from_item->content;
            Status *to_status = Status_Pool_malloc(to);
            STATUS(copy) (to_status, from_status);
        }
    }
    return false;
}

static Found_Result pool_find_by_position(Faction_List list,
                                          Character_Access * npc,
                                          Point point)
{
    for (*npc = list->reset_iterator(list); *npc != NULL;
         *npc = list->next(list, *npc)) {
        Point npc_position = CHARA(get_position) (*npc);
        if (Point_equal(point, (*npc)->Real_Position)) {
            if ((*npc)->Mark->crossable == NO) {
                return FOUND;
            }
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}

static Found_Result pool_find_group_by_position(Faction_Group group,
                                                Character_Access * npc,
                                                Point point)
{
    for (Faction_List list = group->reset_iterator(group); list != NULL;
         list = group->next(group, list)) {
        if (pool_find_by_position(list, npc, point) == FOUND) {
            return FOUND;
        }
    }
    return NOT_FOUND;
}

/** @brief 產生遊戲狀態
 * @param max_config_size 種族最大值
 * @param max_instance_size 實體化角色最大值
 * @return 遊戲狀態 Access
*/
Game_Status_Access
GAME(create) (uint8_t max_config_size, uint8_t max_instance_size) {
    Game_Status_Access self = calloc(1, sizeof(Game_Status));
    set_prepare(self, max_config_size);
    init_group(self, max_instance_size);
    init_player(self);
    init_ally(self);
    init_enemy(self);
    init_neutral(self);

    Faction_Group target_group = F_GROUP(create_without_free_list) ();
    F_GROUP(link) (target_group, self->enemy);
    FACTION(add_target) (self->ally, target_group);

    target_group = F_GROUP(create_without_free_list) ();
    F_GROUP(link) (target_group, self->faction_player);
    F_GROUP(link) (target_group, self->ally);
    F_GROUP(link) (target_group, self->neutral);
    FACTION(add_target) (self->enemy, target_group);

    target_group = F_GROUP(create_without_free_list) ();
    F_GROUP(link) (target_group, self->faction_player);
    FACTION(add_target) (self->neutral, target_group);
    return self;
}

/** @brief 結束遊戲狀態
 * @param self 要使用的遊戲狀態
 *
 * Status_Pool 擁有角色 Access 的管理權，因此必須比 Status_List 晚釋放。
*/
void GAME(free) (Game_Status * self) {
    /*
       self->ally->free(self->ally);
       self->neutral->free(self->neutral);
       self->enemy->free(self->enemy);
     */

    self->group->free(self->group);
    Character_Factory_stop(self->used_pool);

    self->race->free(self->race);
    Character_Factory_stop(self->prepare);
    free(self);
}

/** @brief 分析角色設定檔，產生遊戲狀態
 * @param self 要使用的遊戲狀態
 * @param file_path 設定檔位置
 * @param style_pool 用來儲存圖形的圖形池
 * @return 設定結果
*/
Execute_Result
GAME(parse_npc_config) (Game_Status * self,
                        const char *file_path,
                        Style_Pool_Access style_pool) {
    Execute_Result result = EXECUTE_FAILED;
    config_t cfg;
    config_setting_t *setting;
    const char *str;

    config_init(&cfg);

    /* Read the file. If there is an error, report it and exit. */
    if (!config_read_file(&cfg, file_path)) {
#ifdef DEBUG
        DEBUG_PRINT
            ("%s:%d - %s\n", config_error_file(&cfg),
             config_error_line(&cfg), config_error_text(&cfg));
#endif

        goto DONE;
    }
    /* find npc list */
    setting = config_lookup(&cfg, "npc");

    if (setting != NULL) {
        int total_counter = config_setting_length(setting);
        int counter;

        /* setup npc status with list */
        for (counter = 0; counter < total_counter; counter++) {
            config_setting_t *npc_setting =
                config_setting_get_elem(setting, counter);

            const char *name;
            config_setting_lookup_string(npc_setting, "name", &name);

            const char *race;
            config_setting_lookup_string(npc_setting, "race", &race);

            Style_Access style = STYLE_P(find) (style_pool, race);

            Character_Access npc = Character_Factory_malloc(self->prepare);
            STATUS(set_name) (npc->status, name);
            STATUS(set_race) (npc->status, race);
            CHARA(set_style) (npc, style);
            self->race->insert(self->race, npc);
        }
    } else {
        goto DONE;
    }

    result = EXECUTE_SUCCESS;
  DONE:
    config_destroy(&cfg);
    return result;;
}

/** @brief 尋找該位置是否有任何角色
 * @param self 要使用的遊戲狀態
 * @param npc 回傳 NPC 的 Access
 * @param point 尋找的位置
 * @return 搜尋結果
 *
 * npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的
 * Address，該 npc 變數即可在函數外部使用。
*/
Found_Result
GAME(find_character) (Game_Status * self,
                      Character_Access * npc, Point point) {
    Found_Result result =
        pool_find_group_by_position(self->group, npc, point);
    return result;
}

/** @brief 重新計算所有角色在畫面上的顯示位置
 * @param self 要使用的遊戲狀態
 * @param rectangle 當前螢幕對應實際位置所產生的的方形
 *
 * 根據方形的兩個點重新計算角色位置。
 */
void
GAME(calculate_graph_position) (Game_Status * self, Rectangle rectangle) {
    reset_group_graph_position(self->group, rectangle);
}

/** @brief 實體化友方角色
 * @param self 要使用的遊戲狀態
 * @param race 角色種族
 * @param name 角色名稱
 * @param map 使用的地圖
 * @return 實體化的角色
 *
 * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
 * 設定。
 */
Character_Access
GAME(use_ally) (Game_Status * self, const char *race,
                const char *name, Map_Access map) {
    Character_Access npc = use_npc(self, race, name, map);

    if (npc != NULL) {
        STATUS(set_ally) (npc->status);
        add_ally(self, npc);
    }
    return npc;
}

/** @brief 實體化敵方角色
 * @param self 要使用的遊戲狀態
 * @param race 角色種族
 * @param name 角色名稱
 * @param map 使用的地圖
 * @return 實體化的角色
 *
 * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
 * 設定。
 */
Character_Access
GAME(use_enemy) (Game_Status * self, const char *race,
                 const char *name, Map_Access map) {
    Character_Access npc = use_npc(self, race, name, map);

    if (npc != NULL) {
        STATUS(set_enemy) (npc->status);
        add_enemy(self, npc);
    }
    return npc;
}

/** @brief 實體化中立角色
 * @param self 要使用的遊戲狀態
 * @param race 角色種族
 * @param name 角色名稱
 * @param map 使用的地圖
 * @return 實體化的角色
 *
 * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
 * 設定。
 */
Character_Access
GAME(use_neutral) (Game_Status * self,
                   const char *race, const char *name, Map_Access map) {
    Character_Access npc = use_npc(self, race, name, map);

    if (npc != NULL) {
        STATUS(set_neutral) (npc->status);
        add_neutral(self, npc);
    }
    return npc;
}

/** @brief 將玩家角色實體化
 * @param self 要使用的遊戲狀態
 * @return 回傳玩家角色 Access 以供後續設定
 */
Character_Access GAME(use_player) (Game_Status * self) {
    Character_Access player = Character_Factory_malloc(self->used_pool);
    STATUS(init) (player->status);
    player->Real_Position = Point_create();
    player->Graph_Position = Point_create();

    player->status->faction = FACTION_PLAYER;

    self->faction_player->insert(self->faction_player, player);
    self->player = player;
    return player;
}

/** @brief 回傳角色行動訊息
 * @param self 要使用的遊戲狀態
 * @param current_character 當前進行行動的角色
 * @return 回傳該角色的行動結果
 *
 *  ALLY
 *  1. 確定屬於 FACTION_ENEMY 的 NPC 數量 count
 *  2. 根據 count 隨機取出一個 NPC
 *  3. 根據雙方位置判斷該如何移動(先做測試用路徑搜尋)
 *
 *  Enemy
 *  1. 從 FACTION_ALLY、FACTION_NEUTRAL、FACTION_PLAYER 中隨機挑出一個 FACTION
 *  2. 確定屬於該 FACTION 的 NPC 數量 count
 *  3. 根據 count 隨機取出一個 NPC
 *  4. 根據雙方位置判斷該如何移動(先做測試用路徑搜尋)
 *
 *  NEUTRAL
 *  隨機移動
*/
Message_Type
GAME(action) (Game_Status * self, Character_Access current_character) {
    Message_Type result = DO_NOTHING;

    switch (current_character->status->faction) {
        case FACTION_ALLY:
            result = npc_reaction(current_character, self->enemy);
            break;
        case FACTION_ENEMY:
            result = enemy_reaction(current_character, self->group);
            break;
        case FACTION_NEUTRAL:
            result =
                neutral_npc_reaction(current_character,
                                     self->faction_player);
            break;
        default:
            result = player_reaction(current_character, NULL);
            break;
    }
    return result;
}

/** @brief 攻擊敵方角色
 * @param self 要使用的遊戲狀態
 * @param current 進行攻擊的角色
 * @param target 被攻擊的敵方角色
 * @return 目標的生存狀況
 *
 * 執行 character.attack 後，NPC 會改變立場，但遊戲狀態內也有記錄角色立場的欄位，
 * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
*/
Is_Alive
GAME(attack_enemy_by) (Game_Status * self,
                       Character_Access current, Character_Access target) {
    Is_Alive result = STATUS(attack) (current->status, target->status);
    Faction_List enemy = self->enemy;
    enemy->remove(enemy, target);

    switch (target->status->faction) {
        case FACTION_ENEMY:
            add_enemy(self, target);
            break;
        case FACTION_ALLY:
            add_ally(self, target);
            break;
        default:
            add_neutral(self, target);
            break;
    }
    return result;
}

/** @brief 攻擊友軍角色
 * @param self 要使用的遊戲狀態
 * @param current 進行攻擊的角色
 * @param target 被攻擊的友軍角色
 * @return 目標的生存狀況
 *
 * 執行 character.attack 後，NPC 會改變立場，但遊戲狀態內也有記錄角色立場的欄位，
 * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
*/
Is_Alive
GAME(attack_ally_by) (Game_Status * self,
                      Character_Access current, Character_Access target) {
    Is_Alive result = STATUS(attack) (current->status, target->status);
    Faction_List ally = self->ally;
    ally->remove(ally, target);

    switch (target->status->faction) {
        case FACTION_ENEMY:
            add_enemy(self, target);
            break;
        case FACTION_ALLY:
            add_ally(self, target);
            break;
        default:
            add_neutral(self, target);
            break;
    }
    return result;
}

/** @brief 攻擊中立角色
 * @param self 要使用的遊戲狀態
 * @param current 進行攻擊的角色
 * @param target 被攻擊的中立角色
 * @return 目標的生存狀況
 *
 * 執行 character.attack 後，NPC 會改變立場，但遊戲狀態內也有記錄角色立場的欄位，
 * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
*/
Is_Alive
GAME(attack_neutral_by) (Game_Status * self,
                         Character_Access current,
                         Character_Access target) {
    Is_Alive result = STATUS(attack) (current->status, target->status);
    Faction_List neutral = self->neutral;
    neutral->remove(neutral, target);

    Faction_Type faction;
    Status_get_relation(target->status, faction);
    switch (faction) {
        case FACTION_ENEMY:
            add_enemy(self, target);
            break;
        case FACTION_ALLY:
            add_ally(self, target);
            break;
        default:
            add_neutral(self, target);
            break;
    }
    return result;
}

/** @brief 攻擊玩家角色
 * @param self 要使用的遊戲狀態
 * @param current 進行攻擊的角色
 * @return 目標的生存狀況
*/
Is_Alive GAME(attack_player_by) (Game_Status * self,
                                 Character_Access current) {
    Character_Access target = self->player;
    Is_Alive result = STATUS(attack) (current->status, target->status);
    return result;
}

/** @brief 重設 iterator 並回傳角色
 * @param self 要使用的遊戲狀態
 * @return 第一個角色
*/
Character_Access GAME(reset_iterator) (Game_Status * self) {
    Character_Access npc = NULL;

    self->current = self->group->reset_iterator(self->group);
  RUN:
    if (self->current == NULL) {
        return NULL;
    } else {
        npc = self->current->reset_iterator(self->current);

        if (npc != NULL) {
            return npc;
        } else {
            self->current = self->group->next(self->group, self->current);
            goto RUN;
        }
    }
}

/** @brief 從 iterator 取出下一個角色
 * @param self 要使用的遊戲狀態
 * @param npc iterator 目前的 npc
 * @return 下一個角色
*/
Character_Access GAME(next) (Game_Status * self, Character_Access npc) {
    npc = self->current->next(self->current, npc);
  NPC_CHECK:
    if (npc == NULL) {
        self->current = self->group->next(self->group, self->current);
        if (self->current != NULL) {
            npc = self->current->reset_iterator(self->current);
            goto NPC_CHECK;
        } else {
            return NULL;
        }
    }
    return npc;
}


/** @brief 建立執行順序
 * @param self 要使用的遊戲狀態
 * @return 角色行動順序
*/
Iterator GAME(action_order) (Game_Status * self) {
    return F_GROUP(to_iterator) (self->group);
}
