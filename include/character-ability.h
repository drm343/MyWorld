#ifndef HEADER_CHARACTER_ABILITY
#define HEADER_CHARACTER_ABILITY

#include <stdint.h>
#include <stdbool.h>

#include "ada_style.h"

#include "base_type.h"
#include "setup_config.h"
#include "string_pool.h"
#include "graphic.h"

#define POWER_TYPE(val) ABILITY_POWER_##val
#define TOUGH_TYPE(val) ABILITY_TOUGH_##val
#define SPEED_TYPE(val) ABILITY_SPEED_##val
#define WISE_TYPE(val) ABILITY_WISE_##val

typedef enum {
  BELONG(Weak, ABILITY_POWER) = -1,
  BELONG(Normal, ABILITY_POWER),
  BELONG(Powerful, ABILITY_POWER),
  BELONG(Vary_Powerful, ABILITY_POWER)
} Power_Type;

typedef enum {
  BELONG(Sickly, ABILITY_TOUGH) = -1,
  BELONG(Normal, ABILITY_TOUGH),
  BELONG(Tough, ABILITY_TOUGH),
  BELONG(Vary_Tough, ABILITY_TOUGH)
} Tough_Type;

typedef enum {
  BELONG(Slow, ABILITY_SPEED) = -1,
  BELONG(Normal, ABILITY_SPEED),
  BELONG(Fast, ABILITY_SPEED),
  BELONG(Vary_Fast, ABILITY_SPEED)
} Speed_Type;

typedef enum {
  BELONG(Foolish, ABILITY_WISE) = -1,
  BELONG(Normal, ABILITY_WISE),
  BELONG(Wise, ABILITY_WISE),
  BELONG(Vary_Wise, ABILITY_WISE)
} Wise_Type;

typedef uint8_t Rank_Range;

RECORD_TYPE(Ability_Type,
	    Power_Type Power;
	    Tough_Type Tough;
	    Speed_Type Speed;
	    Wise_Type  Wise;
	    Rank_Range Rank;
)
ACCESS_TYPE(Ability_Type, Ability_Access)


typedef enum {
  FACTION_PLAYER,
  FACTION_ALLY,
  FACTION_ENEMY,
  FACTION_NEUTRAL
} Faction_Type;


typedef enum {
  RELATION_ALLY = FACTION_ALLY,
  RELATION_ENEMY,
  RELATION_NEUTRAL
} Relation_Type;


typedef enum {
  UNUSE,
  IN_USE
} Use_Type;


typedef struct Status {
  String name;
  String race;
  Style_Access Mark;
  Point_Type Real_Position;
  Point_Type Graph_Position;
  Faction_Type Faction;
  Natural Damage;
  Ability_Type Ability;
  bool crossable;
  Use_Type status;

	void (*set_style)(struct Status *, Style_Access);
	void (*print_status)(struct Status *);
  void (*release)(struct Status *);
  void (*copy)(struct Status *, struct Status *);
} Status_Type;
typedef Status_Type * Status_Access;

void character_init(Status_Access);

// -----------------------------------------
// EXPORT
// -----------------------------------------
typedef struct {
  Power_Type (*Get_Power)(Ability_Access);
  void (*Set_Weak)(Ability_Access);
  void (*Set_Normal)(Ability_Access);
  void (*Set_Powerful)(Ability_Access);
  void (*Set_Vary_Powerful)(Ability_Access);
} POWER_TYPE_INTERFACE;
typedef POWER_TYPE_INTERFACE * POWER_ACCESS_INTERFACE;

typedef struct {
  Tough_Type (*Get_Tough)(Ability_Access);
  void (*Set_Sickly)(Ability_Access);
  void (*Set_Normal)(Ability_Access);
  void (*Set_Tough)(Ability_Access);
  void (*Set_Vary_Tough)(Ability_Access);
} TOUGH_TYPE_INTERFACE;
typedef TOUGH_TYPE_INTERFACE * TOUGH_ACCESS_INTERFACE;

typedef struct {
  Speed_Type (*Get_Speed)(Ability_Access);
  void (*Set_Slow)(Ability_Access);
  void (*Set_Normal)(Ability_Access);
  void (*Set_Fast)(Ability_Access);
  void (*Set_Vary_Fast)(Ability_Access);
} SPEED_TYPE_INTERFACE;
typedef SPEED_TYPE_INTERFACE * SPEED_ACCESS_INTERFACE;

typedef struct {
  Wise_Type (*Get_Wise)(Ability_Access);
  void (*Set_Foolish)(Ability_Access);
  void (*Set_Normal)(Ability_Access);
  void (*Set_Wise)(Ability_Access);
  void (*Set_Vary_Wise)(Ability_Access);
} WISE_TYPE_INTERFACE;
typedef WISE_TYPE_INTERFACE * WISE_ACCESS_INTERFACE;

typedef struct {
  bool (*Is_Correct_Rank)(Ability_Access);
  void (*Set_Rank)(Ability_Access, Rank_Range);
  POWER_ACCESS_INTERFACE power;
  TOUGH_ACCESS_INTERFACE tough;
  SPEED_ACCESS_INTERFACE speed;
  WISE_ACCESS_INTERFACE wise;
} ABILITY_INTERFACE;


typedef struct {
  Relation_Type (*Get_Relation)(Status_Access);
  void (*Set_Relation_Ally)(Status_Access);
  void (*Set_Relation_Enemy)(Status_Access);
  void (*Set_Relation_Neutral)(Status_Access);
} CHARACTER_INTERFACE;

extern ABILITY_INTERFACE Ability;
extern CHARACTER_INTERFACE Character;

#endif
