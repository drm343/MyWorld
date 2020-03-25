#ifndef HEADER_LIST_FACTION_GROUP_CLASS
#define HEADER_LIST_FACTION_GROUP_CLASS

#include "List-Faction.h"
#include "List-Iterator.h"


typedef List Faction_Group;

Faction_Group faction_group_create (void);
Faction_Group faction_group_create_without_free_list (void);

void faction_group_link (Faction_Group self, List item);

Faction_List faction_group_get_random_target (Faction_Group self);

Iterator faction_group_to_iterator (Faction_Group self);
#endif
