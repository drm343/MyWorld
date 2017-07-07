#include "character-pool.h"

//static void CHARACTER(gc) (Status_Access pool_access) {
//  uint8_t count = 0;
//  uint8_t used = pool_access->max_size - pool_access->current_size;
//  Status_Access result = NULL;
//
//  for (count; count < used; count++) {
//    result = ADDRESS(pool_access->pool[count]);
//
//    result->access = NULL;
//  }
//}

static void pool_stop(Character_Pool_Access pool_access) {
  free(pool_access->pool);
  free(pool_access);
}

static Status_Access pool_malloc(Character_Pool_Access pool_access) {
  uint8_t start = pool_access->max_size - pool_access->current_size;
  Status_Access result = ADDRESS(pool_access->pool[start]);

  pool_access->current_size -= 1;
  character_init(result);
  return result;
}

static bool pool_copy(Character_Pool_Access from, Character_Pool_Access to,
    String name) {
  uint8_t count = 0;
  uint8_t used = from->max_size - from->current_size;
  Status_Access from_status = NULL;
  Status_Access to_status = NULL;

  for (count; count < used; count++) {
    from_status = ADDRESS(from->pool[count]);

    if(STRCMP(from_status->name, name)) {
      to_status = from->malloc(from);

      to_status->name = from_status->name;
      to_status->Mark = from_status->Mark;
      to_status->Faction = from_status->Faction;
      to_status->Damage = 0;
      to_status->Ability = from_status->Ability;
      to_status->crossable = from_status->crossable;
      return true;
    }
  }
  return false;
}

static bool pool_find(Character_Pool_Access access, Status_Access *npc,
    String race) {
  uint8_t count = 0;
  uint8_t used = access->max_size - access->current_size;

  for (count; count < used; count++) {
    *npc = ADDRESS(access->pool[count]);

    if(STRCMP((*npc)->race, race)) {
      return true;
    }
  }
  npc = NULL;
  return false;
}


static bool pool_find_by_position(Character_Pool_Access access,
    Point_Access point) {
  Status_Access npc = NULL;
  uint8_t count = 0;
  uint8_t used = access->max_size - access->current_size;

  for (count; count < used; count++) {
    npc = ADDRESS(access->pool[count]);

    if(Point.eq(point, &(npc->Real_Position))) {
      return true;
    }
  }
  return false;
}


Character_Pool_Access Character_Pool_start_heap(uint8_t size) {
  Status_Access status_memory = calloc(size, sizeof(Status_Type));
  Character_Pool_Access pool_access = calloc(1, sizeof(Character_Pool_Type));

  pool_access->pool = status_memory;
  pool_access->max_size = size;
  pool_access->current_size = size;

  pool_access->stop = pool_stop;
  pool_access->malloc = pool_malloc;
  pool_access->copy = pool_copy;
  pool_access->find = pool_find;
  pool_access->find_position = pool_find_by_position;
  return pool_access;
}
