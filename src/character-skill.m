#include "character-skill.h"

// ------------------------------------
// Skill API
// ------------------------------------
static void set_skill_name(Skill_Access access, char *name) {
  access->name = name;
}

static void set_skill_efficacy(Skill_Access access, uint8_t efficacy) {
  access->efficacy = efficacy;
}

static void remove_skill(Skill_Access access) {
  access->status = UNUSE;
}


// ------------------------------------
// Pool API
// ------------------------------------
static Skill_Pool_Access pool_start(uint8_t size) {
  Skill_Access memory = calloc(size, sizeof(Skill));
  Skill_Pool_Access pool_access = calloc(1, sizeof(Skill_Pool));

  pool_access->pool = memory;
  pool_access->max_size = size;
  pool_access->current_size = size;

  return pool_access;
}

static void pool_stop(Skill_Pool_Access pool_access) {
  free(pool_access->pool);
  free(pool_access);
}

static Skill_Access pool_malloc(Skill_Pool_Access pool_access, Status_Access owner) {
  uint8_t start = pool_access->max_size - pool_access->current_size;
  Skill_Access result = &(pool_access->pool[start]);
  result->owner = owner;
  result->status = IN_USE;

  pool_access->current_size -= 1;
  return result;
}

static bool pool_find(Skill_Pool_Access access, Status_Access owner,
    char *name, Skill_Access * result) {
  uint8_t count = 0;
  uint8_t used = access->max_size - access->current_size;
  *result = NULL;

  for (count; count < used; count++) {
    *result = &(access->pool[count]);

    if (((*result)->name == name)
      && ((*result)->owner == owner)) {
      return true;
    }
  }
  return false;
}

// ------------------------------------
// Expose API
// ------------------------------------
Skill_API skill = {
  .set_name = set_skill_name,
  .set_efficacy = set_skill_efficacy,
  .remove = remove_skill
};


Skill_Pool_API skill_pool = {
  .start = pool_start,
  .stop = pool_stop,
  .malloc = pool_malloc,
  .find = pool_find
};
