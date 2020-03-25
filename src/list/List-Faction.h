#ifndef HEADER_LIST_FACTION_CLASS
#define HEADER_LIST_FACTION_CLASS

#include <stdint.h>
#include "List.h"
#include "character.h"


typedef List Faction_List;
typedef Message_Type(*Action) (Character_Access charater, List targets);

Faction_List faction_list_create (void);
Faction_List faction_list_create_with_free (void);
Character_Access faction_list_get_random_target (Faction_List self);

void faction_list_set_name (Faction_List self, char *name);
char *faction_list_name (Faction_List self);

void faction_list_set_action (Faction_List self, Action callback);
Action faction_list_action (Faction_List self);


void faction_list_add_target (Faction_List self, List target);
#endif
