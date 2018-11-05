#ifndef HEADER_LIST_FACTION_GROUP_CLASS
#define HEADER_LIST_FACTION_GROUP_CLASS

#include "List-Faction.h"

#define F_GROUP(name) Faction_Group_##name

typedef List Faction_Group;

Faction_Group F_GROUP(create) (void);
void F_GROUP(link) (Faction_Group self, List item);
#endif
