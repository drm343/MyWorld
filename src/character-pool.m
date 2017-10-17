#include "character-pool.h"

#include <time.h>


@implementation Point_Type (Process_C_Message)
- (Message_Type) over_there: (Point_Type *) other {
  Message_Type result = DO_NOTHING;

  int32_t diff_x = [self x] - [other x];
  int32_t diff_y = [self y] - [other y];
  int32_t abs_x = abs(diff_x);
  int32_t abs_y = abs(diff_y);

  if (abs_x >= abs_y) {
    if (diff_x >= 0) {
      result = LEFT;
    }
    else {
      result = RIGHT;
    }
  }
  else {
    if (diff_y >= 0) {
      result = TOP;
    }
    else {
      result = DOWN;
    }
  }
  return result;
}

- (Message_Type) near_by: (Point_Type *) other {
  Message_Type result = DO_NOTHING;

  int32_t diff_x = [self x] - [other x];
  int32_t diff_y = [self y] - [other y];
  int32_t abs_x = abs(diff_x);
  int32_t abs_y = abs(diff_y);

  if (abs_x >= abs_y) {
    if (diff_x >= 2) {
      result = LEFT;
    }
    else if (diff_x <= -2) {
      result = RIGHT;
    }
    else {
      goto RANDOM_POSITION;
    }
  }
  else {
    if (diff_y >= 2) {
      result = TOP;
    }
    else if (diff_y <= -2) {
      result = DOWN;
    }
    else {
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
@end


static Status_Pool_Access status_pool_start(uint8_t size) {
  Status_Access memory = calloc(size, sizeof(Status));
  Status_Pool_Access pool_access = calloc(1, sizeof(Status_Pool));

  pool_access->pool = memory;
  pool_access->max_size = size;
  pool_access->current_size = size;

  return pool_access;
}

static Character_Base_Pool_Access base_pool_start(uint8_t size) {
  Character_Base_Access memory = calloc(size, sizeof(Character_Base_Type));
  Character_Base_Pool_Access pool_access = calloc(1, sizeof(Character_Base_Pool));

  pool_access->pool = memory;
  pool_access->max_size = size;
  pool_access->current_size = size;

  return pool_access;
}

static Character_Pool_Type_Access pool_start(uint8_t size) {
  Status_Pool_Access status = status_pool_start(size);
  Character_Base_Pool_Access base = base_pool_start(size);

  Character_Pool_Type_Access pool_access = calloc(1, sizeof(Character_Pool_Type_Access));

  pool_access->status = status;
  pool_access->base = base;
  return pool_access;
}


static void status_pool_stop(Status_Pool_Access pool_access) {
  free(pool_access->pool);
  free(pool_access);
}


static void base_pool_stop(Character_Base_Pool_Access pool_access) {
  free(pool_access->pool);
  free(pool_access);
}


static void pool_stop(Character_Pool_Type_Access pool_access) {
  status_pool_stop(pool_access->status);
  base_pool_stop(pool_access->base);
  free(pool_access);
}


static Status_Access status_pool_malloc(Status_Pool_Access pool_access) {
  uint8_t start = pool_access->max_size - pool_access->current_size;
  Status_Access result = &(pool_access->pool[start]);

  pool_access->current_size -= 1;
  return result;
}

static Character_Base_Access base_pool_malloc(Character_Base_Pool_Access pool_access) {
  uint8_t start = pool_access->max_size - pool_access->current_size;
  Character_Base_Access result = &(pool_access->pool[start]);

  pool_access->current_size -= 1;
  return result;
}


static bool pool_copy(Character_Pool_Type_Access from, Character_Pool_Type_Access to,
    NSString *name) {
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


static bool pool_all_copy(Character_Pool_Type_Access from, Character_Pool_Type_Access to) {
  uint8_t count = 0;
  uint8_t used = from->status->max_size - from->status->current_size;
  Status_Access from_status = NULL;
  Status_Access to_status = NULL;

  for (count; count < used; count++) {
    from_status = &(from->status->pool[count]);

    to_status = [Character_Pool malloc: to];
    character.copy(to_status, from_status);
  }
  return false;
}


static Found_Result pool_find(Character_Pool_Type_Access access, Status_Access *npc,
    NSString *race) {
  uint8_t count = 0;
  uint8_t used = access->status->max_size - access->status->current_size;

  for (count; count < used; count++) {
    *npc = &(access->status->pool[count]);

    if ([(*npc)->race isEqualToString: race] == YES) {
      return FOUND;
    }
  }
  *npc = NULL;
  return NOT_FOUND;
}


static Found_Result pool_find_by_position(Character_Pool_Type_Access access,
    Status_Access *npc,
    Point_Access point) {
  uint8_t count = 0;
  uint8_t used = access->status->max_size - access->status->current_size;

  for (count; count < used; count++) {
    *npc = &(access->status->pool[count]);

    Faction_Type faction = character.get_relation(*npc);
    Point_Access npc_position = character.get_position(*npc);

    if ([point eq: (*npc)->base->Real_Position]) {
      if ((*npc)->base->crossable == NO) {
        return FOUND;
      }
    }
  }
  *npc = NULL;
  return NOT_FOUND;
}


static void reset_graph_position(Character_Pool_Type_Access access,
    Rectangle_Access rectangle) {
  Point_Access start_point = [rectangle top_left_point];
  Point_Access end_point = [rectangle down_right_point];

  int64_t x = [start_point x];
  int64_t y = [start_point y];

  int64_t max_x = [end_point x];
  int64_t max_y = [end_point y];

  uint8_t count = 0;
  uint8_t used = access->status->max_size - access->status->current_size;
  Status_Access npc = NULL;
  int64_t counter_x = 0;
  int64_t counter_y = 0;

  for (count; count < used; count++) {
    npc = &(access->status->pool[count]);

    counter_x = npc->base->Real_Position.x - x;
    counter_y = npc->base->Real_Position.y - y;

    if (((counter_x >= 0) || (counter_y >= 0)) &&
        ((counter_x < max_x) && (counter_y < max_y - 1))) {
      npc->base->Graph_Position.x = counter_x;
      npc->base->Graph_Position.y = counter_y;
    }
    else {
      npc->base->Graph_Position.x = -1;
      npc->base->Graph_Position.y = -1;
    }
  }
}


static Message_Type player_reaction(bool is_alive) {
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
        }
        else {
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


static Message_Type faction_neutral_reaction(Point_Access self, Point_Access player) {
  Message_Type result = [self near_by: player];

  return result;
}


static Message_Type npc_reaction(Status *self, Status_List *enemy_group) {
  uint8_t used = enemy_group->instance_counter;

  if (used <= 0) {
    return DO_NOTHING;
  }

  uint8_t target_number = rand() % used;
  Status *target = Status_List_get_by_index(enemy_group, target_number);
  Point_Access target_position = character.get_position(target);
  Point_Access self_position = character.get_position(self);
  return [self_position over_there: target_position];
}


@implementation Character_Pool

+ (id) create: (uint8_t) max_config_size with_instance_size: (uint8_t) max_instance_size {
  Character_Pool *object = [[Character_Pool alloc] init];
  [object setPrepare: max_config_size];
  [object setUsed: max_instance_size];
  [object set_ally: 10];
  [object set_enemy: 10];
  [object set_neutral: 10];
  return [object autorelease];
}


+ (Status_Access) malloc: (Character_Pool_Type_Access) pool_access {
  Status_Access status = status_pool_malloc(pool_access->status);
  Character_Base_Access base = base_pool_malloc(pool_access->base);

  status->base = base;
  character.init(status);
  return status;
}


- (Execute_Result) parse_npc_config: (const char *) file_path with_style: (Style_Pool_Access) style_pool {
  Execute_Result result = EXECUTE_FAILED;
  config_t cfg;
  config_setting_t *setting;
  const char *str;

  config_init(&cfg);

  /* Read the file. If there is an error, report it and exit. */
  if(! config_read_file(&cfg, file_path)) {
    fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
	    config_error_line(&cfg), config_error_text(&cfg));

    goto DONE;
  }

  /* find npc list */
  setting = config_lookup(&cfg, "npc");

  if (setting != NULL) {
    int total_counter = config_setting_length(setting);
    int counter;
    const char *c_value;
    NSString *value;

    /* setup npc status with list */
    for (counter = 0; counter < total_counter; counter++) {
      config_setting_t *npc_setting = config_setting_get_elem(setting, counter);

      Status_Access npc = [self sign_in];
      Style_Access style_access = Style_Pool_Interface.malloc(style_pool);

      character.set_style(npc, style_access);

      config_setting_lookup_string(npc_setting, "name", &c_value);
      value = [NSString  stringWithUTF8String: c_value];
      character.set_name(npc, value);

      config_setting_lookup_string(npc_setting, "mark", &c_value);
      value = [NSString  stringWithUTF8String: c_value];
      character.set_mark(npc, value);

      config_setting_lookup_string(npc_setting, "race", &c_value);
      value = [NSString  stringWithUTF8String: c_value];
      character.set_race(npc, value);
    }
  }
  else {
    goto DONE;
  }

  result = EXECUTE_SUCCESS;
DONE:
  config_destroy(&cfg);
  return result;;
}


- (id) setPrepare: (uint8_t) max_size {
  if (prepare) {
    if (max_size > prepare->status->max_size) {
      Character_Pool_Type_Access tmp_pool = pool_start(max_size);
      pool_all_copy(prepare, tmp_pool);
      pool_stop(prepare);
      prepare = tmp_pool;
    }
  }
  else {
    prepare = pool_start(max_size);
  }
  return self;
}


- (Character_Pool_Type *) prepare {
  return prepare;
}


- (id) setUsed: (uint8_t) max_size {
  if (used) {
    if (max_size > used->status->max_size) {
      Character_Pool_Type_Access tmp_pool = pool_start(max_size);
      pool_all_copy(used, tmp_pool);
      pool_stop(used);
      used = tmp_pool;
    }
  }
  else {
    used = pool_start(max_size);
  }
  return self;
}


- (Character_Pool_Type *) used {
  return used;
}


- (void) dealloc {
  pool_stop(prepare);
  pool_stop(used);
  [super dealloc];
}


- (Status_Access) sign_in {
  return [Character_Pool malloc: prepare];
}


  /** @brief 尋找該位置是否有任何角色
   * @param npc 回傳 NPC 的 Access
   * @param point 尋找的位置
   * @return 搜尋結果
   *
   * npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的
   * Address，該 npc 變數即可在函數外部使用。
  */
- (Found_Result) find_character: (Status_Access *) npc
  with_position: (Point_Access) point {
    Found_Result result = pool_find_by_position(used, npc, point);
    return result;
}


- (id) calculate_graph_position: (Rectangle_Access) rectangle {
  reset_graph_position(used, rectangle);
  return self;
}


- (id) set_ally: (uint8_t) max_size {
  if (ally) {
    if (max_size >= ally->max_size) {
      Status_List *tmp_list = Status_List_start(max_size);
      Status_List_copy_all(ally, tmp_list);
      Status_List_stop(ally);
      ally = tmp_list;
    }
  }
  else {
    ally = Status_List_start(max_size);
  }
  return self;
}


- (id) add_ally: (Status_Access) npc {
  uint8_t max_size = ally->max_size;

  if (Status_List_insert(ally, npc) < 0) {
    [self set_ally: max_size + 10];
    Status_List_insert(ally, npc);
  }
  return self;
}


- (id) set_enemy: (uint8_t) max_size {
  if (enemy) {
    if (max_size >= enemy->max_size) {
      Status_List *tmp_list = Status_List_start(max_size);
      Status_List_copy_all(enemy, tmp_list);
      Status_List_stop(enemy);
      enemy = tmp_list;
    }
  }
  else {
    enemy = Status_List_start(max_size);
  }
  return self;
}


- (id) add_enemy: (Status_Access) npc {
  uint8_t max_size = enemy->max_size;

  if (Status_List_insert(enemy, npc) < 0) {
    [self set_enemy: max_size + 10];
    Status_List_insert(enemy, npc);
  }
  return self;
}


- (id) set_neutral: (uint8_t) max_size {
  if (neutral) {
    if (max_size >= neutral->max_size) {
      Status_List *tmp_list = Status_List_start(max_size);
      Status_List_copy_all(neutral, tmp_list);
      Status_List_stop(neutral);
      neutral = tmp_list;
    }
  }
  else {
    neutral = Status_List_start(max_size);
  }
  return self;
}


- (id) add_neutral: (Status_Access) npc {
  uint8_t max_size = neutral->max_size;

  if (Status_List_insert(neutral, npc) < 0) {
    [self set_neutral: max_size + 10];
    Status_List_insert(neutral, npc);
  }
  return self;
}


- (Status_Access) use_ally: (NSString *) race
                  with_name: (NSString *) name
                  and_map: (Map_Access) map {
  Status_Access npc = [self use_npc: race with_name: name and_map: map];

  if (npc != NULL) {
    character.set_relation_ally(npc);
    [self add_ally: npc];
  }
  return npc;
}


- (Status_Access) use_enemy: (NSString *) race
                  with_name: (NSString *) name
                  and_map: (Map_Access) map {
  Status_Access npc = [self use_npc: race with_name: name and_map: map];

  if (npc != NULL) {
    character.set_relation_enemy(npc);
    [self add_enemy: npc];
  }
  return npc;
}


- (Status_Access) use_neutral: (NSString *) race
                  with_name: (NSString *) name
                  and_map: (Map_Access) map {
  Status_Access npc = [self use_npc: race with_name: name and_map: map];

  if (npc != NULL) {
    character.set_relation_neutral(npc);
    [self add_neutral: npc];
  }
  return npc;
}


- (Status_Access) use_npc: (NSString *) race
                  with_name: (NSString *) name
                  and_map: (Map_Access) map {
  Status_Access origin_npc = NULL;
  Status_Access npc = NULL;

  Found_Result result = pool_find(prepare, &origin_npc, race);

  if (result == FOUND) {
    npc = [Character_Pool malloc: used];
    character.copy(npc, origin_npc);

    character.set_name(npc, name);

    character.set_random_position(npc, [map get_end_x], [map get_end_y]);
  }
  return npc;
}


- (Status_Access) use_player {
  Status_Access player = [Character_Pool malloc: used];
  player->faction = FACTION_PLAYER;
  return player;
}


- (uint8_t) instance_count {
  return used->status->max_size - used->status->current_size;
}


- (Status_Access) get_instance_by_index: (int) index {
  return &(used->status->pool[index]);
}


  /** @brief 回傳角色行動訊息
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
- (Message_Type) action: (Status_Access) current_character {
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
      result = npc_reaction(current_character, enemy);
      break;
    case FACTION_ENEMY:
      target_group_number = rand() % 100;
      ally_target_number = ally->instance_counter;
      neutral_target_number = neutral->instance_counter;

      total_target_number = 1 + ally_target_number
        + neutral_target_number;
      weigh_value = 80 / total_target_number;
      neutral_weigh_value = 100 - neutral_target_number * weigh_value;
      ally_weigh_value = 100 - (neutral_target_number + ally_target_number) * weigh_value;

      if (target_group_number >= neutral_weigh_value) {
        result = npc_reaction(current_character, neutral);
      }
      else if (target_group_number >= ally_weigh_value) {
        result = npc_reaction(current_character, ally);
      }
      else {
        target = [self get_instance_by_index: 0];
        target_position = character.get_position(target);
        self_position = character.get_position(current_character);
        result = [self_position over_there: target_position];
      }
      break;
    case FACTION_NEUTRAL:
      target = [self get_instance_by_index: 0];
      target_position = character.get_position(target);
      self_position = character.get_position(current_character);
      result = faction_neutral_reaction(self_position, target_position);
      break;
    default:
      result = player_reaction(is_alive);
      break;
  }
  return result;
}


  /** @brief 攻擊敵方角色
   * @param current 進行攻擊的角色
   * @param target 被攻擊的敵方角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
- (Is_Alive) attack_enemy_by: (Status_Access) current with_target: (Status_Access) target {
  Is_Alive result = character.attack(current, target);
  Status_List_remove(enemy, target);

  switch (target->faction) {
    case FACTION_ENEMY:
      [self add_enemy: target];
      break;
    case FACTION_ALLY:
      [self add_ally: target];
      break;
    default:
      [self add_neutral: target];
      break;
  }
  return result;
}


  /** @brief 攻擊友軍角色
   * @param current 進行攻擊的角色
   * @param target 被攻擊的友軍角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
- (Is_Alive) attack_ally_by: (Status_Access) current with_target: (Status_Access) target {
  Is_Alive result = character.attack(current, target);
  Status_List_remove(ally, target);

  switch (target->faction) {
    case FACTION_ENEMY:
      [self add_enemy: target];
      break;
    case FACTION_ALLY:
      [self add_ally: target];
      break;
    default:
      [self add_neutral: target];
      break;
  }
  return result;
}


  /** @brief 攻擊中立角色
   * @param current 進行攻擊的角色
   * @param target 被攻擊的中立角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
- (Is_Alive) attack_neutral_by: (Status_Access) current with_target: (Status_Access) target {
  Is_Alive result = character.attack(current, target);
  Status_List_remove(neutral, target);

  switch (target->faction) {
    case FACTION_ENEMY:
      [self add_enemy: target];
      break;
    case FACTION_ALLY:
      [self add_ally: target];
      break;
    default:
      [self add_neutral: target];
      break;
  }
  return result;
}


  /** @brief 攻擊玩家角色
   * @param current 進行攻擊的角色
   * @return 目標的生存狀況
  */
- (Is_Alive) attack_player_by: (Status_Access) current {
  Status_Access target = [self get_instance_by_index: 0];
  Is_Alive result = character.attack(current, target);
  return result;
}
@end
