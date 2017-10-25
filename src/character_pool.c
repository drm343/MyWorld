#include "character_pool.h"


/** @brief Namespace CP
 */
#define EXPORT(name) CP(name)

// ----------------------------------------------
// Internal Object Struct
// ----------------------------------------------
/** @brief 角色池結構
 *
 * 建議直接使用結構名稱 Character_Pool，而不用 struct Character_Pool。
 *
 * Status_List 只儲存實體化角色的 Access。
 */
typedef struct Character_Pool {
    Status_Pool *prepare;       /**< 種族池 */
    Status_Pool *used_pool;     /**< 實體化角色用的角色池 */
    Status_List *used;          /**< 實體化角色 */
    Status_List *ally;          /**< 盟友 */
    Status_List *enemy;         /**< 敵人 */
    Status_List *neutral;       /**< 中立 */
} Character_Pool;


// ----------------------------------------------
// Internal API
// ----------------------------------------------
  /** @brief 設定種族池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_prepare(Character_Pool * access, uint8_t max_size);

  /** @brief 設定實體化的角色池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_used(Character_Pool * access, uint8_t max_size);

  /** @brief 分配一個半完成初始化的角色實體
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 半實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
static Status_Access use_npc(Character_Pool * access, const char *race,
                             const char *name, Map_Access map);
  /** @brief 設定友方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_ally(Character_Pool * access, uint8_t max_size);

  /** @brief 設定敵方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_enemy(Character_Pool * access, uint8_t max_size);

  /** @brief 設定中立角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_neutral(Character_Pool * access, uint8_t max_size);

  /** @brief 設定 NPC 為敵方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_enemy(Character_Pool * access, Status_Access npc);

  /** @brief 設定 NPC 為友方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_ally(Character_Pool * access, Status_Access npc);

  /** @brief 設定 NPC 為中立角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_neutral(Character_Pool * access, Status_Access npc);


// ----------------------------------------------
// External API
// ----------------------------------------------
Message_Type Point_Type_over_there(Point_Type * self, Point_Type * other)
{
    Message_Type result = DO_NOTHING;

    int32_t diff_x = Point_Type_x(self) - Point_Type_x(other);
    int32_t diff_y = Point_Type_y(self) - Point_Type_y(other);
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

Message_Type Point_Type_near_by(Point_Type * self, Point_Type * other)
{
    Message_Type result = DO_NOTHING;

    int32_t diff_x = Point_Type_x(self) - Point_Type_x(other);
    int32_t diff_y = Point_Type_y(self) - Point_Type_y(other);
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


/*
static bool pool_copy(Base_Pool_Access from, Base_Pool_Access to,
    char *name) {
  uint8_t count = 0;
  uint8_t used = from->status->max_size - from->status->current_size;
  Status_Access from_status = NULL;
  Status_Access to_status = NULL;

  for (count; count < used; count++) {
    from_status = &(from->status->pool[count]);

    if ([from_status->name isEqualToString: name] == YES) {
      to_status = [Character_Pool malloc: to];
      character.copy(to_status, from_status);
      character.set_name(to_status, name);
      return true;
    }
  }
  return false;
}
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


static Found_Result pool_find(Status_Pool * access,
                              Status_Access * npc, const char *race)
{
    race = String_Repo_search(race);

    Status *content = NULL;
    Status_Item *item = NULL;

    uint8_t max_size = access->max_size;

    for (uint8_t index = 0; index < max_size; index++) {
        Status_Item *from_item = &(access->pool[index]);

        if (from_item->is_used == IN_USE) {
            *npc = from_item->content;

            if ((*npc)->race == race) {
                return FOUND;
            }
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}


static Found_Result pool_find_by_position(Status_List *
                                          access, Status_Access * npc,
                                          Point_Access point)
{
    uint8_t used = access->instance_counter;

    for (uint8_t count = 0; count < used; count++) {
        *npc = Status_List_get_by_index(access, count);

        Faction_Type faction;
        Status_get_relation(*npc, faction);
        Point_Access npc_position = STATUS(get_position) (*npc);

        if (Point_Type_eq(point, (*npc)->Real_Position)) {
            if ((*npc)->Mark->crossable == NO) {
                return FOUND;
            }
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}


static void reset_graph_position(Status_List * access,
                                 Rectangle_Access rectangle)
{
    Rectangle_Access_change(rectangle);
    Point_Access start_point = Rectangle_Access_top_left_point();
    Point_Access end_point = Rectangle_Access_down_right_point();

    Point_Access_change(start_point);
    int64_t x = Point_Access_x();
    int64_t y = Point_Access_y();

    Point_Access_change(end_point);
    int64_t max_x = Point_Access_x();
    int64_t max_y = Point_Access_y();

    uint8_t used = access->instance_counter;
    Status_Access npc = NULL;
    int64_t counter_x = 0;
    int64_t counter_y = 0;

    for (uint8_t count = 0; count < used; count++) {
        npc = Status_List_get_by_index(access, count);

        counter_x = npc->Real_Position->x - x;
        counter_y = npc->Real_Position->y - y;

        if (((counter_x >= 0) || (counter_y >= 0)) &&
            ((counter_x < max_x) && (counter_y < max_y - 1))) {
            Point_Access_change(npc->Graph_Position);
            Point_Access_set_x(counter_x);
            Point_Access_set_y(counter_y);
        } else {
            Point_Access_change(npc->Graph_Position);
            Point_Access_set_x(-1);
            Point_Access_set_y(-1);
        }
    }
}


static Message_Type player_reaction(bool is_alive)
{
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


static Message_Type faction_neutral_reaction(Point_Access self,
                                             Point_Access player)
{
    return Point_Type_near_by(self, player);
}


static Message_Type npc_reaction(Status * self, Status_List * enemy_group)
{
    uint8_t used = enemy_group->instance_counter;

    if (used <= 0) {
        return DO_NOTHING;
    }

    uint8_t target_number = rand() % used;
    Status *target = Status_List_get_by_index(enemy_group, target_number);
    Point_Access target_position = STATUS(get_position) (target);
    Point_Access self_position = STATUS(get_position) (self);
    return Point_Type_over_there(self_position, target_position);
}


  /** @brief 產生角色池
   * @param max_config_size 種族最大值
   * @param max_instance_size 實體化角色最大值
   * @return 角色池 Access
  */
Character_Pool *EXPORT(create) (uint8_t max_config_size,
                                uint8_t max_instance_size) {
    Character_Pool *self = calloc(1, sizeof(Character_Pool));
    set_prepare(self, max_config_size);
    set_used(self, max_instance_size);
    set_ally(self, 10);
    set_enemy(self, 10);
    set_neutral(self, 10);
    return self;
}


  /** @brief 結束角色池
   * @param self 要使用的角色池
   *
   * Status_Pool 擁有角色 Access 的管理權，因此必須比 Status_List 晚釋放。
  */
void EXPORT(free) (Character_Pool * self) {
    Status_List_stop(self->used);
    Status_List_stop(self->ally);
    Status_List_stop(self->neutral);
    Status_List_stop(self->enemy);

    Status_Pool_stop(self->used_pool);
    Status_Pool_stop(self->prepare);
    free(self);
}


  /** @brief 分析角色設定檔，產生角色池
   * @param self 要使用的角色池
   * @param file_path 設定檔位置
   * @param style_pool 用來儲存圖形的圖形池
   * @return 設定結果
  */
Execute_Result EXPORT(parse_npc_config) (Character_Pool * self,
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
        const char *value;

        /* setup npc status with list */
        for (counter = 0; counter < total_counter; counter++) {
            config_setting_t *npc_setting =
                config_setting_get_elem(setting, counter);

            Status_Access npc = EXPORT(sign_in) (self);

            config_setting_lookup_string(npc_setting, "name", &value);
            value = String_Repo_sign_in(value);
            STATUS(set_name) (npc, value);

            config_setting_lookup_string(npc_setting, "race", &value);
            value = String_Repo_sign_in(value);
            STATUS(set_race) (npc, value);

            Style_Access style_access = STYLE_P(find) (style_pool, value);
            STATUS(set_style) (npc, style_access);
        }
    } else {
        goto DONE;
    }

    result = EXECUTE_SUCCESS;
  DONE:
    config_destroy(&cfg);
    return result;;
}


  /** @brief 設定種族池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_prepare(Character_Pool * access, uint8_t max_size)
{
    Status_Pool *prepare = access->prepare;

    if (prepare) {
        if (max_size > prepare->max_size) {
            Status_Pool *tmp_pool = Status_Pool_start(max_size);
            pool_all_copy(prepare, tmp_pool);
            Status_Pool_stop(prepare);
            access->prepare = tmp_pool;
        }
    } else {
        access->prepare = Status_Pool_start(max_size);
    }
}


  /** @brief 設定實體化的角色池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_used(Character_Pool * access, uint8_t max_size)
{
    Status_Pool *used_pool = access->used_pool;

    if (used_pool) {
        if (max_size > used_pool->max_size) {
            Status_Pool *tmp_pool = Status_Pool_start(max_size);
            pool_all_copy(used_pool, tmp_pool);
            Status_Pool_stop(used_pool);
            access->used_pool = tmp_pool;
        }
    } else {
        access->used_pool = Status_Pool_start(max_size);
    }

    Status_List *used = access->used;

    if (used) {
        if ((max_size > used->max_size) &&
            (used->instance_counter >= used->max_size)) {
            Status_List *tmp_list = Status_List_start(max_size);
            Status_List_copy_all(used, tmp_list);
            Status_List_stop(used);
            access->used = tmp_list;
        }
    } else {
        access->used = Status_List_start(max_size);
    }
}


  /** @brief 註冊種族資料到角色池
   * @param self 要使用的角色池
   * @return 回傳待設定的種族資料 Access
  */
Status_Access EXPORT(sign_in) (Character_Pool * self) {
    Status_Access race = Status_Pool_malloc(self->prepare);
    STATUS(init) (race);
    return race;
}


  /** @brief 尋找該位置是否有任何角色
   * @param self 要使用的角色池
   * @param npc 回傳 NPC 的 Access
   * @param point 尋找的位置
   * @return 搜尋結果
   *
   * npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的
   * Address，該 npc 變數即可在函數外部使用。
  */
Found_Result EXPORT(find_character) (Character_Pool * self,
                                     Status_Access * npc,
                                     Point_Access point) {
    Found_Result result = pool_find_by_position(self->used, npc, point);
    return result;
}


  /** @brief 重新計算所有角色在畫面上的顯示位置
   * @param self 要使用的角色池
   * @param rectangle 當前螢幕對應實際位置所產生的的方形
   *
   * 根據方形的兩個點重新計算角色位置。
   */
void EXPORT(calculate_graph_position) (Character_Pool * self,
                                       Rectangle_Access rectangle) {
    reset_graph_position(self->used, rectangle);
}


  /** @brief 設定友方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_ally(Character_Pool * access, uint8_t max_size)
{
    Status_List *ally = access->ally;

    if (ally) {
        if (max_size >= ally->max_size) {
            Status_List *tmp_list = Status_List_start(max_size);
            Status_List_copy_all(ally, tmp_list);
            Status_List_stop(ally);
            access->ally = tmp_list;
        }
    } else {
        access->ally = Status_List_start(max_size);
    }
}


  /** @brief 實體化友方角色
   * @param self 要使用的角色池
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色
   *
   * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_ally) (Character_Pool * self, const char *race,
                                const char *name, Map_Access map) {
    Status_Access npc = use_npc(self, race, name, map);

    if (npc != NULL) {
        STATUS(set_ally) (npc);
        add_ally(self, npc);
    }
    return npc;
}


  /** @brief 設定 NPC 為友方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_ally(Character_Pool * access, Status_Access npc)
{
    Status_List *ally = access->ally;
    uint8_t max_size = ally->max_size;

    if (Status_List_insert(ally, npc) < 0) {
        set_ally(access, max_size + 10);
        Status_List_insert(ally, npc);
    }
}


  /** @brief 設定敵方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_enemy(Character_Pool * access, uint8_t max_size)
{
    Status_List *enemy = access->enemy;

    if (enemy) {
        if (max_size >= enemy->max_size) {
            Status_List *tmp_list = Status_List_start(max_size);
            Status_List_copy_all(enemy, tmp_list);
            Status_List_stop(enemy);
            access->enemy = tmp_list;
        }
    } else {
        access->enemy = Status_List_start(max_size);
    }
}


  /** @brief 實體化敵方角色
   * @param self 要使用的角色池
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色
   *
   * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_enemy) (Character_Pool * self, const char *race,
                                 const char *name, Map_Access map) {
    Status_Access npc = use_npc(self, race, name, map);

    if (npc != NULL) {
        STATUS(set_enemy) (npc);
        add_enemy(self, npc);
    }
    return npc;
}


  /** @brief 設定 NPC 為敵方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_enemy(Character_Pool * access, Status_Access npc)
{
    Status_List *enemy = access->enemy;
    uint8_t max_size = enemy->max_size;

    if (Status_List_insert(enemy, npc) < 0) {
        set_enemy(access, max_size + 10);
        Status_List_insert(enemy, npc);
    }
}


  /** @brief 設定中立角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_neutral(Character_Pool * access, uint8_t max_size)
{
    Status_List *neutral = access->neutral;

    if (neutral) {
        if (max_size >= neutral->max_size) {
            Status_List *tmp_list = Status_List_start(max_size);
            Status_List_copy_all(neutral, tmp_list);
            Status_List_stop(neutral);
            access->neutral = tmp_list;
        }
    } else {
        access->neutral = Status_List_start(max_size);
    }
}


  /** @brief 實體化中立角色
   * @param self 要使用的角色池
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色
   *
   * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_neutral) (Character_Pool * self,
                                   const char *race, const char *name,
                                   Map_Access map) {
    Status_Access npc = use_npc(self, race, name, map);

    if (npc != NULL) {
        STATUS(set_neutral) (npc);
        add_neutral(self, npc);
    }
    return npc;
}


  /** @brief 設定 NPC 為中立角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_neutral(Character_Pool * access, Status_Access npc)
{
    Status_List *neutral = access->neutral;
    uint8_t max_size = neutral->max_size;

    if (Status_List_insert(neutral, npc) < 0) {
        set_neutral(access, max_size + 10);
        Status_List_insert(neutral, npc);
    }
}


  /** @brief 分配一個半完成初始化的角色實體
   * @param self 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 半實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
static Status_Access use_npc(Character_Pool * self, const char *race,
                             const char *name, Map_Access map)
{
    Status_Access origin_npc = NULL;
    Status_Access npc = NULL;

    Found_Result result = pool_find(self->prepare, &origin_npc, race);

    if (result == FOUND) {
        npc = Status_Pool_malloc(self->used_pool);
        STATUS(init) (npc);
        STATUS(copy) (npc, origin_npc);

        STATUS(set_name) (npc, name);

        Point_Access_change(MAP(bottom_right) (map));
        STATUS(set_random_position) (npc, Point_Access_x(),
                                     Point_Access_y());

        Status_List_insert(self->used, npc);
    }
    return npc;
}


  /** @brief 將玩家角色實體化
   * @param self 要使用的角色池
   * @return 回傳玩家角色 Access 以供後續設定
   */
Status_Access EXPORT(use_player) (Character_Pool * self) {
    Status_Access player = Status_Pool_malloc(self->used_pool);
    STATUS(init) (player);

    player->faction = FACTION_PLAYER;
    Status_List_insert(self->used, player);
    return player;
}


  /** @brief 回傳實體化角色總和
   * @param self 要使用的角色池
   * @return 回傳總和數值
   */
uint8_t EXPORT(instance_count) (Character_Pool * self) {
    return self->used->instance_counter;
}


  /** @brief 根據編號回傳角色 Access
   * @param self 要使用的角色池
   * @param index 要找出來的角色編號
   * @return 回傳角色 Access
   */
Status_Access EXPORT(get_instance_by_index) (Character_Pool * self,
                                             int index) {
    return Status_List_get_by_index(self->used, index);
}


  /** @brief 回傳角色行動訊息
   * @param self 要使用的角色池
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
Message_Type EXPORT(action) (Character_Pool * self,
                             Status_Access current_character) {
    Message_Type result = DO_NOTHING;
    uint8_t target_group_number;
    bool is_alive = current_character->is_alive;

    uint8_t total_target_number = 1;
    uint8_t neutral_target_number = 0;
    uint8_t ally_target_number = 0;
    uint8_t weigh_value = 0;
    uint8_t neutral_weigh_value = 0;
    uint8_t ally_weigh_value = 0;

    Status_Access target = NULL;
    Point_Access target_position = NULL;
    Point_Access self_position = NULL;

    switch (current_character->faction) {
        case FACTION_ALLY:
            result = npc_reaction(current_character, self->enemy);
            break;
        case FACTION_ENEMY:
            target_group_number = rand() % 100;
            ally_target_number = self->ally->instance_counter;
            neutral_target_number = self->neutral->instance_counter;

            total_target_number = 1 + ally_target_number
                + neutral_target_number;
            weigh_value = 80 / total_target_number;
            neutral_weigh_value =
                100 - neutral_target_number * weigh_value;
            ally_weigh_value =
                100 - (neutral_target_number +
                       ally_target_number) * weigh_value;

            if (target_group_number >= neutral_weigh_value) {
                result = npc_reaction(current_character, self->neutral);
            } else if (target_group_number >= ally_weigh_value) {
                result = npc_reaction(current_character, self->ally);
            } else {
                target = EXPORT(get_instance_by_index) (self, 0);
                target_position = STATUS(get_position) (target);
                self_position = STATUS(get_position) (current_character);
                result =
                    Point_Type_over_there(self_position, target_position);
            }
            break;
        case FACTION_NEUTRAL:
            target = EXPORT(get_instance_by_index) (self, 0);
            target_position = STATUS(get_position) (target);
            self_position = STATUS(get_position) (current_character);
            result =
                faction_neutral_reaction(self_position, target_position);
            break;
        default:
            result = player_reaction(is_alive);
            break;
    }
    return result;
}


  /** @brief 攻擊敵方角色
   * @param self 要使用的角色池
   * @param current 進行攻擊的角色
   * @param target 被攻擊的敵方角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但角色池內也有記錄角色立場的欄位，
   * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_enemy_by) (Character_Pool * self,
                                  Status_Access current,
                                  Status_Access target) {
    Is_Alive result = STATUS(attack) (current, target);
    Status_List *enemy = self->enemy;
    Status_List_remove(enemy, target);

    switch (target->faction) {
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
   * @param self 要使用的角色池
   * @param current 進行攻擊的角色
   * @param target 被攻擊的友軍角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但角色池內也有記錄角色立場的欄位，
   * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_ally_by) (Character_Pool * self,
                                 Status_Access current,
                                 Status_Access target) {
    Is_Alive result = STATUS(attack) (current, target);
    Status_List *ally = self->ally;
    Status_List_remove(ally, target);

    switch (target->faction) {
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
   * @param self 要使用的角色池
   * @param current 進行攻擊的角色
   * @param target 被攻擊的中立角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但角色池內也有記錄角色立場的欄位，
   * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_neutral_by) (Character_Pool * self,
                                    Status_Access current,
                                    Status_Access target) {
    Is_Alive result = STATUS(attack) (current, target);
    Status_List *neutral = self->neutral;
    Status_List_remove(neutral, target);

    switch (target->faction) {
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
   * @param self 要使用的角色池
   * @param current 進行攻擊的角色
   * @return 目標的生存狀況
  */
Is_Alive EXPORT(attack_player_by) (Character_Pool * self,
                                   Status_Access current) {
    Status_Access target = EXPORT(get_instance_by_index) (self, 0);
    Is_Alive result = STATUS(attack) (current, target);
    return result;
}

#undef EXPORT
