#include "character_pool.h"


#define EXPORT(name) Character_Pool_##name


// ----------------------------------------------
// Internal Object Struct
// ----------------------------------------------
GENERIC_POOL(Status_Pool, Status_Access);
typedef Status_Pool *Status_Pool_Access;


GENERIC_POOL(Character_Base_Pool, Character_Base_Access);
typedef Character_Base_Pool *Character_Base_Pool_Access;


typedef struct {
    Status_Pool_Access status;
    Character_Base_Pool_Access base;
} Base_Pool_Type;
typedef Base_Pool_Type *Base_Pool_Access;


/** @brief 角色池基本定義
 *
 * 建議使用本名稱，僅提供 API 供使用者操作本結構。
 *
 * Status_List 只儲存實體化角色的 Access。
 */
typedef struct _Character_Pool {
    Base_Pool_Access prepare; // 種族池
    Base_Pool_Access used; // 實體化角色
    Status_List *ally; // 盟友
    Status_List *enemy; // 敵人
    Status_List *neutral; // 中立
} Character_Pool;


// ----------------------------------------------
// Internal API
// ----------------------------------------------
  /** @brief 設定種族池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_prepare (Character_Pool * access, uint8_t max_size);

  /** @brief 設定實體化的角色池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_used (Character_Pool * access, uint8_t max_size);

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
static Status_Access use_npc (Character_Pool * access, const char *race,
                             const char *name, Map_Access map);
  /** @brief 設定友方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_ally (Character_Pool * access, uint8_t max_size);

  /** @brief 設定敵方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_enemy (Character_Pool * access, uint8_t max_size);

  /** @brief 設定中立角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_neutral (Character_Pool * access, uint8_t max_size);

  /** @brief 設定 NPC 為敵方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_enemy(Character_Pool * access, Status_Access npc);

  /** @brief 設定 NPC 為友方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_ally (Character_Pool * access, Status_Access npc);

  /** @brief 設定 NPC 為中立角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_neutral (Character_Pool * access, Status_Access npc);


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


static Status_Pool_Access status_pool_start(uint8_t size)
{
    Status_Access memory = calloc(size, sizeof(Status));
    Status_Pool_Access pool_access = calloc(1, sizeof(Status_Pool));

    pool_access->pool = memory;
    pool_access->max_size = size;
    pool_access->current_size = size;

    return pool_access;
}

static Character_Base_Pool_Access base_pool_start(uint8_t size)
{
    Character_Base_Access memory =
        calloc(size, sizeof(Character_Base_Type));
    Character_Base_Pool_Access pool_access =
        calloc(1, sizeof(Character_Base_Pool));

    pool_access->pool = memory;
    pool_access->max_size = size;
    pool_access->current_size = size;

    return pool_access;
}

static Base_Pool_Access pool_start(uint8_t size)
{
    Status_Pool_Access status = status_pool_start(size);
    Character_Base_Pool_Access base = base_pool_start(size);

    Base_Pool_Access pool_access =
        calloc(1, sizeof(Base_Pool_Access));

    pool_access->status = status;
    pool_access->base = base;
    return pool_access;
}


static void status_pool_stop(Status_Pool_Access pool_access)
{
    free(pool_access->pool);
    free(pool_access);
}


static void base_pool_stop(Character_Base_Pool_Access pool_access)
{
    free(pool_access->pool);
    free(pool_access);
}


static void pool_stop(Base_Pool_Access pool_access)
{
    status_pool_stop(pool_access->status);
    base_pool_stop(pool_access->base);
    free(pool_access);
}


static Status_Access status_pool_malloc(Status_Pool_Access pool_access)
{
    uint8_t start = pool_access->max_size - pool_access->current_size;
    Status_Access result = &(pool_access->pool[start]);

    pool_access->current_size -= 1;
    return result;
}

static Character_Base_Access base_pool_malloc(Character_Base_Pool_Access
                                              pool_access)
{
    uint8_t start = pool_access->max_size - pool_access->current_size;
    Character_Base_Access result = &(pool_access->pool[start]);

    pool_access->current_size -= 1;
    return result;
}


static Status_Access Base_Pool_Type_malloc(Base_Pool_Type * access)
{
    Status_Access status = status_pool_malloc(access->status);
    Character_Base_Access base = base_pool_malloc(access->base);

    status->base = base;
    character.init(status);
    return status;
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

    if ([from_status->base->name isEqualToString: name] == YES) {
      to_status = [Character_Pool malloc: to];
      character.copy(to_status, from_status);
      character.set_name(to_status, name);
      return true;
    }
  }
  return false;
}
*/


static bool pool_all_copy(Base_Pool_Access from,
                          Base_Pool_Access to)
{
    uint8_t count = 0;
    uint8_t used = from->status->max_size - from->status->current_size;
    Status_Access from_status = NULL;
    Status_Access to_status = NULL;

    for (count; count < used; count++) {
        from_status = &(from->status->pool[count]);

        to_status = Base_Pool_Type_malloc(to);
        character.copy(to_status, from_status);
    }
    return false;
}


static Found_Result pool_find(Base_Pool_Access access,
                              Status_Access * npc, const char *race)
{
    uint8_t count = 0;
    uint8_t used = access->status->max_size - access->status->current_size;
    race = String_Repo_search(race);

    for (count; count < used; count++) {
        *npc = &(access->status->pool[count]);

        if ((*npc)->race == race) {
            return FOUND;
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}


static Found_Result pool_find_by_position(Base_Pool_Access
                                          access, Status_Access * npc,
                                          Point_Access point)
{
    uint8_t count = 0;
    uint8_t used = access->status->max_size - access->status->current_size;

    for (count; count < used; count++) {
        *npc = &(access->status->pool[count]);

        Faction_Type faction = character.get_relation(*npc);
        Point_Access npc_position = character.get_position(*npc);

        if (Point_Type_eq(point, (*npc)->base->Real_Position)) {
            if ((*npc)->base->crossable == NO) {
                return FOUND;
            }
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}


static void reset_graph_position(Base_Pool_Access access,
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

    uint8_t count = 0;
    uint8_t used = access->status->max_size - access->status->current_size;
    Status_Access npc = NULL;
    int64_t counter_x = 0;
    int64_t counter_y = 0;

    for (count; count < used; count++) {
        npc = &(access->status->pool[count]);

        counter_x = npc->base->Real_Position->x - x;
        counter_y = npc->base->Real_Position->y - y;

        if (((counter_x >= 0) || (counter_y >= 0)) &&
            ((counter_x < max_x) && (counter_y < max_y - 1))) {
            Point_Access_change(npc->base->Graph_Position);
            Point_Access_set_x(counter_x);
            Point_Access_set_y(counter_y);
        } else {
            Point_Access_change(npc->base->Graph_Position);
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
    Point_Access target_position = character.get_position(target);
    Point_Access self_position = character.get_position(self);
    return Point_Type_over_there(self_position, target_position);
}


  /** @brief 產生 Character_Pool 物件
   * @param max_config_size 種族池最大值
   * @param max_instance_size 實體角色池最大值
   * @return 新的 Character_Pool 物件
  */
Character_Pool *EXPORT(create) (uint8_t max_config_size,
                              uint8_t max_instance_size) {
    Character_Pool *object = calloc(1, sizeof(Character_Pool));
    set_prepare (object, max_config_size);
    set_used (object, max_instance_size);
    set_ally (object, 10);
    set_enemy (object, 10);
    set_neutral (object, 10);
    return object;
}


  /** @brief 結束 Character_Pool 物件
   * @param access 要使用的 Character_Pool
  */
void EXPORT(free) (Character_Pool * access) {
    pool_stop(access->prepare);
    pool_stop(access->used);
    Status_List_stop(access->ally);
    Status_List_stop(access->neutral);
    Status_List_stop(access->enemy);
    free(access);
}


  /** @brief 分析角色設定，產生種族池
   * @param access 要使用的 Character_Pool
   * @param file_path 設定檔位置
   * @param style_pool 實體化圖形介面用的圖形池
   * @return 設定結果
  */
Execute_Result EXPORT(parse_npc_config) (Character_Pool * access,
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

            Status_Access npc = EXPORT(sign_in) (access);
            Style_Access style_access =
                Style_Pool_Interface.malloc(style_pool);

            character.set_style(npc, style_access);

            config_setting_lookup_string(npc_setting, "name", &value);
            value = String_Repo_sign_in(value);
            character.set_name(npc, value);

            config_setting_lookup_string(npc_setting, "mark", &value);
            value = String_Repo_sign_in(value);
            character.set_mark(npc, value);

            config_setting_lookup_string(npc_setting, "race", &value);
            value = String_Repo_sign_in(value);
            character.set_race(npc, value);
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
static void set_prepare (Character_Pool * access, uint8_t max_size) {
    Base_Pool_Type *prepare = access->prepare;

    if (prepare) {
        if (max_size > prepare->status->max_size) {
            Base_Pool_Access tmp_pool = pool_start(max_size);
            pool_all_copy(prepare, tmp_pool);
            pool_stop(prepare);
            access->prepare = tmp_pool;
        }
    } else {
        access->prepare = pool_start(max_size);
    }
}


  /** @brief 設定實體化的角色池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
static void set_used (Character_Pool * access, uint8_t max_size) {
    Base_Pool_Type *used = access->used;

    if (used) {
        if (max_size > used->status->max_size) {
            Base_Pool_Access tmp_pool = pool_start(max_size);
            pool_all_copy(used, tmp_pool);
            pool_stop(used);
            access->used = tmp_pool;
        }
    } else {
        access->used = pool_start(max_size);
    }
}


  /** @brief 註冊種族資料到角色池
   * @param access 要使用的 Character_Pool
   * @return 待設定的種族資料
  */
Status_Access EXPORT(sign_in) (Character_Pool * access) {
    return Base_Pool_Type_malloc(access->prepare);
}


  /** @brief 尋找該位置是否有任何角色
   * @param access 要使用的 Character_Pool
   * @param npc 回傳 NPC 的 Access
   * @param point 尋找的位置
   * @return 搜尋結果
   *
   * npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的
   * Address，該 npc 變數即可在函數外部使用。
  */
Found_Result EXPORT(find_character) (Character_Pool * access,
                                   Status_Access * npc,
                                   Point_Access point) {
    Found_Result result = pool_find_by_position(access->used, npc, point);
    return result;
}


  /** @brief 重新計算所有角色在畫面上的顯示位置
   * @param access 要使用的 Character_Pool
   * @param rectangle 當前圖形所在的方形
   *
   * 根據方形的兩個點重新計算角色位置。
   */
void EXPORT(calculate_graph_position) (Character_Pool * access,
                                     Rectangle_Access rectangle) {
    reset_graph_position(access->used, rectangle);
}


  /** @brief 設定友方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_ally (Character_Pool * access, uint8_t max_size) {
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
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_ally) (Character_Pool * access, const char *race,
                              const char *name, Map_Access map) {
    Status_Access npc = use_npc (access, race, name, map);

    if (npc != NULL) {
        character.set_relation_ally(npc);
        add_ally(access, npc);
    }
    return npc;
}


  /** @brief 設定 NPC 為友方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_ally (Character_Pool * access, Status_Access npc) {
    Status_List *ally = access->ally;
    uint8_t max_size = ally->max_size;

    if (Status_List_insert(ally, npc) < 0) {
        set_ally (access, max_size + 10);
        Status_List_insert(ally, npc);
    }
}


  /** @brief 設定敵方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_enemy (Character_Pool * access, uint8_t max_size) {
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
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_enemy) (Character_Pool * access, const char *race,
                               const char *name, Map_Access map) {
    Status_Access npc = use_npc (access, race, name, map);

    if (npc != NULL) {
        character.set_relation_enemy(npc);
        add_enemy (access, npc);
    }
    return npc;
}

  /** @brief 設定 NPC 為敵方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_enemy (Character_Pool * access, Status_Access npc) {
    Status_List *enemy = access->enemy;
    uint8_t max_size = enemy->max_size;

    if (Status_List_insert(enemy, npc) < 0) {
        set_enemy (access, max_size + 10);
        Status_List_insert(enemy, npc);
    }
}


  /** @brief 設定中立角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   */
static void set_neutral (Character_Pool * access, uint8_t max_size) {
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
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_neutral) (Character_Pool * access,
                                 const char *race, const char *name,
                                 Map_Access map) {
    Status_Access npc = use_npc (access, race, name, map);

    if (npc != NULL) {
        character.set_relation_neutral(npc);
        add_neutral (access, npc);
    }
    return npc;
}

  /** @brief 設定 NPC 為中立角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   */
static void add_neutral (Character_Pool * access, Status_Access npc) {
    Status_List *neutral = access->neutral;
    uint8_t max_size = neutral->max_size;

    if (Status_List_insert(neutral, npc) < 0) {
        set_neutral (access, max_size + 10);
        Status_List_insert(neutral, npc);
    }
}


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
static Status_Access use_npc (Character_Pool * access, const char *race,
                             const char *name, Map_Access map) {
    Status_Access origin_npc = NULL;
    Status_Access npc = NULL;

    Found_Result result = pool_find(access->prepare, &origin_npc, race);

    if (result == FOUND) {
        npc = Base_Pool_Type_malloc(access->used);
        character.copy(npc, origin_npc);

        character.set_name(npc, name);

        character.set_random_position(npc,[map get_end_x],[map get_end_y]);
    }
    return npc;
}


  /** @brief 將玩家角色實體化
   * @param access 要使用的 Character_Pool
   * @return 回傳玩家角色已供後續設定
   */
Status_Access EXPORT(use_player) (Character_Pool * access) {
#ifdef DEBUG
    DEBUG_PRINT("access is null? %s\n", BOOL_STRING(access == NULL));
    DEBUG_PRINT("access->used is null? %s\n",
                BOOL_STRING(access->used == NULL));
#endif

    Status_Access player = Base_Pool_Type_malloc(access->used);

#ifdef DEBUG
    DEBUG_PRINT("player init failed? %s\n", BOOL_STRING(player == NULL));
#endif

    player->faction = FACTION_PLAYER;
    return player;
}


  /** @brief 回傳角色實體總和
   * @param access 要使用的 Character_Pool
   * @return 回傳總和數值
   */
uint8_t EXPORT(instance_count) (Character_Pool * access) {
    return access->used->status->max_size -
        access->used->status->current_size;
}


  /** @brief 回傳角色實體
   * @param access 要使用的 Character_Pool
   * @param index 要找出來的角色編號
   * @return 回傳角色
   */
Status_Access EXPORT(get_instance_by_index) (Character_Pool * access,
                                           int index) {
    return &(access->used->status->pool[index]);
}


  /** @brief 回傳角色行動訊息
   * @param access 要使用的 Character_Pool
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
Message_Type EXPORT(action) (Character_Pool * access,
                           Status_Access current_character) {
    Message_Type result = DO_NOTHING;
    uint8_t target_group_number;
    bool is_alive = current_character->base->is_alive;

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
            result = npc_reaction(current_character, access->enemy);
            break;
        case FACTION_ENEMY:
            target_group_number = rand() % 100;
            ally_target_number = access->ally->instance_counter;
            neutral_target_number = access->neutral->instance_counter;

            total_target_number = 1 + ally_target_number
                + neutral_target_number;
            weigh_value = 80 / total_target_number;
            neutral_weigh_value =
                100 - neutral_target_number * weigh_value;
            ally_weigh_value =
                100 - (neutral_target_number +
                       ally_target_number) * weigh_value;

            if (target_group_number >= neutral_weigh_value) {
                result = npc_reaction(current_character, access->neutral);
            } else if (target_group_number >= ally_weigh_value) {
                result = npc_reaction(current_character, access->ally);
            } else {
                target = EXPORT(get_instance_by_index) (access, 0);
                target_position = character.get_position(target);
                self_position = character.get_position(current_character);
                result =
                    Point_Type_over_there(self_position, target_position);
            }
            break;
        case FACTION_NEUTRAL:
            target = EXPORT(get_instance_by_index) (access, 0);
            target_position = character.get_position(target);
            self_position = character.get_position(current_character);
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
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @param target 被攻擊的敵方角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_enemy_by) (Character_Pool * access,
                                Status_Access current,
                                Status_Access target) {
    Is_Alive result = character.attack(current, target);
    Status_List *enemy = access->enemy;
    Status_List_remove(enemy, target);

    switch (target->faction) {
        case FACTION_ENEMY:
            add_enemy (access, target);
            break;
        case FACTION_ALLY:
            add_ally (access, target);
            break;
        default:
            add_neutral (access, target);
            break;
    }
    return result;
}


  /** @brief 攻擊友軍角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @param target 被攻擊的友軍角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_ally_by) (Character_Pool * access,
                               Status_Access current,
                               Status_Access target) {
    Is_Alive result = character.attack(current, target);
    Status_List *ally = access->ally;
    Status_List_remove(ally, target);

    switch (target->faction) {
        case FACTION_ENEMY:
            add_enemy (access, target);
            break;
        case FACTION_ALLY:
            add_ally (access, target);
            break;
        default:
            add_neutral (access, target);
            break;
    }
    return result;
}


  /** @brief 攻擊中立角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @param target 被攻擊的中立角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_neutral_by) (Character_Pool * access,
                                  Status_Access current,
                                  Status_Access target) {
    Is_Alive result = character.attack(current, target);
    Status_List *neutral = access->neutral;
    Status_List_remove(neutral, target);

    switch (target->faction) {
        case FACTION_ENEMY:
            add_enemy (access, target);
            break;
        case FACTION_ALLY:
            add_ally (access, target);
            break;
        default:
            add_neutral (access, target);
            break;
    }
    return result;
}


  /** @brief 攻擊玩家角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @return 目標的生存狀況
  */
Is_Alive EXPORT(attack_player_by) (Character_Pool * access,
                                 Status_Access current) {
    Status_Access target = EXPORT(get_instance_by_index) (access, 0);
    Is_Alive result = character.attack(current, target);
    return result;
}

#undef EXPORT
