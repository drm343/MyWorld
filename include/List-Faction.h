#ifndef HEADER_LIST_FACTION_CLASS
#define HEADER_LIST_FACTION_CLASS

#include <stdint.h>
#include "List.h"
#include "character.h"

#define FACTION(name) Faction_List_##name

typedef List Faction_List;
typedef Message_Type(*Action) (Character_Access charater, List targets);

Faction_List FACTION(create) (void);
Faction_List FACTION(create_with_free) (void);
Character_Access FACTION(get_random_target) (Faction_List self);

void FACTION(set_name) (Faction_List self, char *name);
char *FACTION(name) (Faction_List self);

void FACTION(set_action) (Faction_List self, Action callback);
Action FACTION(action) (Faction_List self);


void FACTION(add_target) (Faction_List self, List target);
#endif
