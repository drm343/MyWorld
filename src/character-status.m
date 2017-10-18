#include "character-status.h"


static void print_status(Status_Access access) {
  Character_Base_Access base = access->base;

  printf("--------- status -------\n");
  printf("%s : %s\n", base->name, access->race);
  printf("Mark: %s\n", base->Mark->mark);
  printf("Damage: %d\n", access->damage);
}


// -----------------------------------
// Init and free
// -----------------------------------
static void character_init(Status_Access access) {
  Character_Base_init(access->base);
  access->faction = FACTION_NEUTRAL;
}


static void character_free(Status_Access access) {
  Character_Base_free(access->base);
  access->faction = FACTION_NEUTRAL;
}


static void character_copy(Status_Access access, Status_Access from) {
  from->base->copy(access->base, from->base);
  access->race = from->race;

  access->faction = from->faction;
  access->damage = from->damage;
}


static Is_Alive is_alive(Status_Access access) {
  if (access->base->is_alive == true) {
    return ALIVE;
  }
  else {
    return DEAD;
  }
}


static Is_Alive attack_character(Status_Access from, Status_Access to) {
  to->damage += 1;
  switch (from->faction) {
    case FACTION_PLAYER:
      character.set_relation_enemy(to);
      break;
    case FACTION_ALLY:
      character.set_relation_enemy(to);
      break;
    case FACTION_ENEMY:
      character.set_relation_ally(to);
      break;
    case FACTION_NEUTRAL:
      character.set_relation_enemy(to);
      break;
    default:
      break;
  }

  if (to->damage >= 3) {
    to->base->is_alive = false;
    to->base->crossable = true;
    to->base->attackable = false;
    return DEAD;
  }
  else {
    return ALIVE;
  }
}


// -----------------------------------------
// GET or SET BASE VALUE
// -----------------------------------------
static void set_base_name(Status_Access access, const char *name) {
  access->base->name = name;
}

static void set_race(Status_Access access, const char *race) {
  access->race = race;
}

static void set_base_style(Status_Access access, Style_Access style) {
  access->base->Mark = style;
}

static void set_base_mark(Status_Access access, const char *mark) {
  access->base->Mark->mark = mark;
}


static void set_random_position(Status_Access access,
    int64_t max_x, int64_t max_y) {
      Point_Access_change(access->base->Real_Position);
      Point_Access_set_x(rand() % max_x);
      Point_Access_set_y(rand() % max_y);
}


static void set_random_relation(Status_Access access) {
  int result = (rand() % 100) + 1;

  if (result <= 33) {
    character.set_relation_ally(access);
  }
  else if (result >= 67) {
    character.set_relation_enemy(access);
  }
  else {
    character.set_relation_neutral(access);
  }
}


// -----------------------------------------
// RELATION
// -----------------------------------------
static Relation_Type get_relation(Status_Access access) {
  return access->faction;
}

static char *get_relation_string(Status_Access access) {
  char *result;

  switch (access->faction) {
  case FACTION_ALLY:
    result = "ally";
    break;
  case FACTION_ENEMY:
    result = "enemy";
    break;
  case FACTION_NEUTRAL:
    result = "neutral";
    break;
  default:
    result = "player";
    break;
  }
  return result;
}

static void set_ally(Status_Access access) {
  if (access->faction != FACTION_PLAYER) {
    access->faction = RELATION_ALLY;
  }
}

static void set_enemy(Status_Access access) {
  if (access->faction != FACTION_PLAYER) {
    access->faction = RELATION_ENEMY;
  }
}

static void set_neutral(Status_Access access) {
  if (access->faction != FACTION_PLAYER) {
    access->faction = RELATION_NEUTRAL;
  }
}


// -----------------------------------------
// POSITION
// -----------------------------------------
Point_Access get_position(Status_Access access) {
  return access->base->Real_Position;
}


// -----------------------------------------
// EXPOSE API
// -----------------------------------------
Character_API character = {
  .init = character_init,
  .free = character_free,
  .copy = character_copy,
  .print_status = print_status,

  .set_name = set_base_name,
  .set_race = set_race,
  .set_style = set_base_style,
  .set_mark = set_base_mark,
  .set_random_position = set_random_position,
  .set_random_relation = set_random_relation,

  .is_alive = is_alive,

  .get_relation = get_relation,
  .get_relation_string = get_relation_string,
  .set_relation_ally = set_ally,
  .set_relation_enemy = set_enemy,
  .set_relation_neutral = set_neutral,

  .attack = attack_character,

  .get_position = get_position
};
