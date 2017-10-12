#include "character-status.h"


static void print_status(Status_Access access) {
  Character_Base_Access base = access->base;

  printf("--------- status -------\n");
  printf("%s : %s\n", base->name, access->race);
  printf("Mark: %s\n", base->Mark->mark);
  printf("Damage: %d\n", access->damage);
}


static void character_init(Status_Access access) {
  character_base_init(access->base);
}


static void character_copy(Status_Access access, Status_Access from) {
  from->base->copy(access->base, from->base);
  access->race = from->race;

  access->faction = from->faction;
  access->damage = from->damage;
}


static Is_Alive attack_character(Status_Access from, Status_Access to) {
  to->damage += 1;

  if (to->damage >= 3) {
    character.set_relation_enemy(to);
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
// SET BASE VALUE
// -----------------------------------------
static void set_base_name(Status_Access access, String name) {
  access->base->name = name;
}

static void set_race(Status_Access access, String race) {
  access->race = race;
}

static void set_base_style(Status_Access access, Style_Access style) {
  access->base->Mark = style;
}

static void set_base_mark(Status_Access access, String mark) {
  access->base->Mark->mark = mark;
}


static void set_random_position(Status_Access access,
    int64_t max_x, int64_t max_y) {
  access->base->Real_Position.x = rand() % max_x;
  access->base->Real_Position.y = rand() % max_y;
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

static void set_ally(Status_Access access) {
  access->faction = RELATION_ALLY;
}

static void set_enemy(Status_Access access) {
  access->faction = RELATION_ENEMY;
}

static void set_neutral(Status_Access access) {
  access->faction = RELATION_NEUTRAL;
}


// -----------------------------------------
// EXPOSE API
// -----------------------------------------
Character_API character = {
  .init = character_init,
  .copy = character_copy,
  .print_status = print_status,
  .set_name = set_base_name,
  .set_race = set_race,
  .set_style = set_base_style,
  .set_mark = set_base_mark,
  .set_random_position = set_random_position,
  .set_random_relation = set_random_relation,

  .get_relation = get_relation,
  .set_relation_ally = set_ally,
  .set_relation_enemy = set_enemy,
  .set_relation_neutral = set_neutral,

  .attack = attack_character
};
