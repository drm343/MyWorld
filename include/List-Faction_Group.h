#ifndef HEADER_LIST_FACTION_GROUP_CLASS
#define HEADER_LIST_FACTION_GROUP_CLASS

#include "List-Faction.h"
#include "List-Iterator.h"

#define F_GROUP(name) Faction_Group_##name

typedef List Faction_Group;

Faction_Group F_GROUP(create) (void);
Faction_Group F_GROUP(create_without_free_list) (void);

void F_GROUP(link) (Faction_Group self, List item);

Faction_List F_GROUP(get_random_target) (Faction_Group self);

Iterator F_GROUP(to_iterator) (Faction_Group self);
#endif
