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

static Character_Pool_Access pool_start(uint8_t size) {
  Status_Pool_Access status = status_pool_start(size);
  Character_Base_Pool_Access base = base_pool_start(size);

  Character_Pool_Access pool_access = calloc(1, sizeof(Character_Pool_Type));

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

static void pool_stop(Character_Pool_Access pool_access) {
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

static Status_Access pool_malloc(Character_Pool_Access pool_access) {
  Status_Access status = status_pool_malloc(pool_access->status);
  Character_Base_Access base = base_pool_malloc(pool_access->base);

  status->base = base;
  character.init(status);
  return status;
}


static bool pool_copy(Character_Pool_Access from, Character_Pool_Access to,
    String name) {
  uint8_t count = 0;
  uint8_t used = from->status->max_size - from->status->current_size;
  Status_Access from_status = NULL;
  Status_Access to_status = NULL;

  for (count; count < used; count++) {
    from_status = &(from->status->pool[count]);

    if(STRCMP(from_status->base->name, name)) {
      to_status = character_pool.malloc(to);
      character.copy(to_status, from_status);
      character.set_name(to_status, name);
      return true;
    }
  }
  return false;
}


static Found_Result pool_find(Character_Pool_Access access, Status_Access *npc,
    String race) {
  uint8_t count = 0;
  uint8_t used = access->status->max_size - access->status->current_size;

  for (count; count < used; count++) {
    *npc = &(access->status->pool[count]);

    if(STRCMP((*npc)->race, race)) {
      return FOUND;
    }
  }
  *npc = NULL;
  return NOT_FOUND;
}


static Found_Result pool_find_by_position(Character_Pool_Access access, Status_Access *npc,
    Point_Access point) {
  uint8_t count = 0;
  uint8_t used = access->status->max_size - access->status->current_size;

  for (count; count < used; count++) {
    *npc = &(access->status->pool[count]);

    if (Point.eq(point, &((*npc)->base->Real_Position))
        && ((*npc)->faction != FACTION_PLAYER)) {

      if ((*npc)->base->crossable == NO) {
        return FOUND;
      }
    }
  }
  *npc = NULL;
  return NOT_FOUND;
}


static void reset_graph_position(Character_Pool_Access access,
    int64_t x,
    int64_t y,
    int64_t max_x,
    int64_t max_y) {
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


Character_Pool_API character_pool = {
  .start = pool_start,
  .stop = pool_stop,
  .malloc = pool_malloc,
  .copy = pool_copy,
  .find = pool_find,
  .find_position = pool_find_by_position,
  .reset_graph_position = reset_graph_position
};
