#ifndef HEADER_CHARACTER_ABILITY_NEW
#define HEADER_CHARACTER_ABILITY_NEW

#include "character.h"


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


typedef struct Character {
  Character_Base_Access base;

  String race;
  Faction_Type faction;
  Natural damage;

	void (*print_status)(struct Character *);
} Status;
typedef Status * Status_Access;


typedef struct {
  void (*init)(Status_Access);
  void (*copy)(Status_Access, Status_Access);
  void (*set_name)(Status_Access, String);
	void (*set_race)(Status_Access, String);
	void (*set_style)(Status_Access, Style_Access);
	void (*set_mark)(Status_Access, String);

  Relation_Type (*get_relation)(Status_Access);
  void (*set_relation_ally)(Status_Access);
  void (*set_relation_enemy)(Status_Access);
  void (*set_relation_neutral)(Status_Access);
} Character_API;

extern Character_API character;

#endif
