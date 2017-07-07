#include "character-ability.h"

#define POWER(name) CHARACTER_POWER_##name
#define TOUGH(name) CHARACTER_TOUGH_##name
#define SPEED(name) CHARACTER_SPEED_##name
#define WISE(name) CHARACTER_WISE_##name
#define RELATION(name) CHARACTER_RELATION_##name

static bool Is_Correct_Rank(Ability_Access access) {
  Rank_Range total =
    ACCESS_FIELD(access, Power) +
    ACCESS_FIELD(access, Tough) +
    ACCESS_FIELD(access, Speed) +
    ACCESS_FIELD(access, Wise);

  if (total == ACCESS_FIELD(access, Rank)) {
    return true;
  }
  else {
    return false;
  }
}

static void Set_Rank(Ability_Access access, Rank_Range Rank) {
  SET_ACCESS_FIELD(access, Rank, Rank);
}

// -----------------------------------------
// POWER
// -----------------------------------------
static Power_Type Get_Power(Ability_Access access) {
  return ACCESS_FIELD(access, Power);
}

static void POWER(Set_Weak)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Power, POWER_TYPE(Weak));
}

static void POWER(Set_Normal)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Power, POWER_TYPE(Normal));
}

static void POWER(Set_Powerful)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Power, POWER_TYPE(Powerful));
}

static void POWER(Set_Vary_Powerful)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Power, POWER_TYPE(Vary_Powerful));
}

static POWER_TYPE_INTERFACE Power_Type_Interface = {
  .Get_Power = Get_Power,
  .Set_Weak = POWER(Set_Weak),
  .Set_Normal = POWER(Set_Normal),
  .Set_Powerful = POWER(Set_Powerful),
  .Set_Vary_Powerful = POWER(Set_Vary_Powerful)
};

// -----------------------------------------
// TOUGH
// -----------------------------------------
static Tough_Type Get_Tough(Ability_Access access) {
  return ACCESS_FIELD(access, Tough);
}

static void TOUGH(Set_Sickly)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Tough, TOUGH_TYPE(Sickly));
}

static void TOUGH(Set_Normal)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Tough, TOUGH_TYPE(Normal));
}

static void TOUGH(Set_Tough)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Tough, TOUGH_TYPE(Tough));
}

static void TOUGH(Set_Vary_Tough)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Tough, TOUGH_TYPE(Vary_Tough));
}

static TOUGH_TYPE_INTERFACE Tough_Type_Interface = {
  .Get_Tough = Get_Tough,
  .Set_Sickly = TOUGH(Set_Sickly),
  .Set_Normal = TOUGH(Set_Normal),
  .Set_Tough = TOUGH(Set_Tough),
  .Set_Vary_Tough = TOUGH(Set_Vary_Tough)
};

// -----------------------------------------
// SPEED
// -----------------------------------------
static Speed_Type Get_Speed(Ability_Access access) {
  return ACCESS_FIELD(access, Speed);
}

static void SPEED(Set_Slow)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Speed, SPEED_TYPE(Slow));
}

static void SPEED(Set_Normal)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Speed, SPEED_TYPE(Normal));
}

static void SPEED(Set_Fast)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Speed, SPEED_TYPE(Fast));
}

static void SPEED(Set_Vary_Fast)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Speed, SPEED_TYPE(Vary_Fast));
}

static SPEED_TYPE_INTERFACE Speed_Type_Interface = {
  .Get_Speed = Get_Speed,
  .Set_Slow = SPEED(Set_Slow),
  .Set_Normal = SPEED(Set_Normal),
  .Set_Fast = SPEED(Set_Fast),
  .Set_Vary_Fast = SPEED(Set_Vary_Fast)
};

// -----------------------------------------
// WISE
// -----------------------------------------
static Wise_Type Get_Wise(Ability_Access access) {
  return ACCESS_FIELD(access, Wise);
}

static void WISE(Set_Foolish)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Wise, WISE_TYPE(Foolish));
}

static void WISE(Set_Normal)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Wise, WISE_TYPE(Normal));
}

static void WISE(Set_Wise)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Wise, WISE_TYPE(Wise));
}

static void WISE(Set_Vary_Wise)(Ability_Access access) {
  SET_ACCESS_FIELD(access, Wise, WISE_TYPE(Vary_Wise));
}

static WISE_TYPE_INTERFACE Wise_Type_Interface = {
  .Get_Wise = Get_Wise,
  .Set_Foolish = WISE(Set_Foolish),
  .Set_Normal = WISE(Set_Normal),
  .Set_Wise = WISE(Set_Wise),
  .Set_Vary_Wise = WISE(Set_Vary_Wise)
};

// -----------------------------------------
// RELATION
// -----------------------------------------
static Relation_Type Get_Relation(Status_Access access) {
  return access->Faction;
}

static void RELATION(Set_Ally)(Status_Access access) {
  access->Faction = RELATION_ALLY;
}

static void RELATION(Set_Enemy)(Status_Access access) {
  access->Faction = RELATION_ENEMY;
}

static void RELATION(Set_Neutral)(Status_Access access) {
  access->Faction = RELATION_NEUTRAL;
}

// --------------------------
// Character
// --------------------------
static void set_style(Status_Access access, Style_Access style) {
  access->Mark = style;
}

static void print_status(Status_Access access) {
  Ability_Type status = access->Ability;

  printf("--------- status -------\n");
  printf("%s : %s\n", access->name, access->race);
  printf("Mark: %s %s\n", access->Mark->name, access->Mark->mark);
  printf("Rank : %d\n", status.Rank);
  printf("Power: %d\n", status.Power);
  printf("Tough: %d\n", status.Tough);
  printf("Speed: %d\n", status.Speed);
  printf("Wise : %d\n", status.Wise);
}

static void release(Status_Access access) {
  access->status = UNUSE;
}

static void character_copy(Status_Access access, Status_Access from) {
  access->name = from->name;
  access->race = from->race;
  access->Mark = from->Mark;

  access->Faction = from->Faction;
  access->Damage = 0;
  access->Ability.Power = from->Ability.Power;
  access->Ability.Tough = from->Ability.Tough;
  access->Ability.Speed = from->Ability.Speed;
  access->Ability.Wise = from->Ability.Wise;
  access->Ability.Rank = 0;
  access->crossable = from->crossable;
  access->status = IN_USE;

  access->set_style = from->set_style;
  access->print_status = from->print_status;
  access->release = from->release;
  access->copy = from->copy;
}

void character_init(Status_Access access) {
  access->Real_Position.x  = 0;
  access->Real_Position.y  = 0;
  access->Graph_Position.x = 0;
  access->Graph_Position.y = 0;
  access->Faction = FACTION_PLAYER;
  access->Damage = 0;
  access->Ability.Power = POWER_TYPE(Normal);
  access->Ability.Tough = TOUGH_TYPE(Normal);
  access->Ability.Speed = SPEED_TYPE(Normal);
  access->Ability.Wise = WISE_TYPE(Normal);
  access->Ability.Rank = 0;
  access->crossable = false;
  access->status = IN_USE;

  access->set_style = set_style;
  access->print_status = print_status;
  access->release = release;
  access->copy = character_copy;
}


// -----------------------------------------
// EXPORT
// -----------------------------------------
ABILITY_INTERFACE Ability = {
  .Is_Correct_Rank = Is_Correct_Rank,
  .Set_Rank = Set_Rank,

  .power = ADDRESS(Power_Type_Interface),
  .tough = ADDRESS(Tough_Type_Interface),
  .speed = ADDRESS(Speed_Type_Interface),
  .wise  = ADDRESS(Wise_Type_Interface)
};

CHARACTER_INTERFACE Character = {
  .Get_Relation = Get_Relation,
  .Set_Relation_Ally = RELATION(Set_Ally),
  .Set_Relation_Enemy = RELATION(Set_Enemy),
  .Set_Relation_Neutral = RELATION(Set_Neutral)
};

#undef POWER
