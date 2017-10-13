#include "character-pool.h"


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


static Status_Access pool_malloc(Character_Pool_Type_Access pool_access) {
  Status_Access status = status_pool_malloc(pool_access->status);
  Character_Base_Access base = base_pool_malloc(pool_access->base);

  status->base = base;
  character.init(status);
  return status;
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


static Found_Result pool_find_by_position(Character_Pool_Type_Access access, Status_Access *npc,
    Point_Access point) {
  uint8_t count = 0;
  uint8_t used = access->status->max_size - access->status->current_size;

  for (count; count < used; count++) {
    *npc = &(access->status->pool[count]);

    if ([point eq: (*npc)->base->Real_Position]
        && ((*npc)->faction != FACTION_PLAYER)) {

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


@implementation Character_Pool

+ (id) create: (uint8_t) max_config_size with_instance_size: (uint8_t) max_instance_size {
  Character_Pool *object = [[Character_Pool alloc] init];
  [object setPrepare: max_config_size];
  [object setUsed: max_instance_size];
  return [object autorelease];
}


+ (Status_Access) malloc: (Character_Pool_Type_Access) pool_access {
  Status_Access status = status_pool_malloc(pool_access->status);
  Character_Base_Access base = base_pool_malloc(pool_access->base);

  status->base = base;
  character.init(status);
  return status;
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

- (Found_Result) find_character: (Status_Access *) npc
  with_position: (Point_Access) point {
    return pool_find_by_position(used, npc, point);
}

- (id) calculate_graph_position: (Rectangle_Access) rectangle {
  reset_graph_position(used, rectangle);
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
    character.set_random_relation(npc);
  }
  return npc;
}


- (Status_Access) use_player {
  return [Character_Pool malloc: used];
}


- (uint8_t) instance_count {
  return used->status->max_size - used->status->current_size;
}


- (Status_Access) get_instance_by_index: (int) index {
  return &(used->status->pool[index]);
}

@end