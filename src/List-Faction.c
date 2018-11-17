#include "List-Faction.h"

#include "String.h"


typedef struct Custom_Property {
    ImmutableString name;
    List target;

    void (*super_free) (List self);
     Message_Type(*action) (Character_Access charater, List targets);
} *Custom_Property;


static void FACTION(free_content) (void *content) {
    Character_Access self = content;
    CHARA(free) (self);
}


static void FACTION(free) (List self) {
    Custom_Property property = self->property;
    void (*super_free) (List self) = property->super_free;

    if (property->name != NULL) {
        String_free(property->name);
    }

    if (property->target != NULL) {
        List target = property->target;
        target->free(target);
    }
    free(property);
    super_free(self);
}


Faction_List FACTION(create) (void) {
    Faction_List self = LIST(create) (NULL, NULL, NULL);
    Custom_Property property = NEW(Custom_Property);
    property->name = NULL;
    property->super_free = self->free;
    property->target = NULL;
    self->property = property;
    self->free = FACTION(free);
    return self;
}


Faction_List FACTION(create_with_free) (void) {
    Faction_List self = LIST(create) (NULL, FACTION(free_content), NULL);
    Custom_Property property = NEW(Custom_Property);
    property->name = NULL;
    property->super_free = self->free;
    property->target = NULL;
    self->property = property;
    self->free = FACTION(free);
    return self;
}


Character_Access FACTION(get_random_target) (Faction_List self) {
    List_Property list = self->list;

    uint8_t used = list->counter;
    if (used <= 0) {
        return NULL;
    }
    uint8_t target_number = rand() % used + 1;

    if (list->current != NULL) {
        if (list->current->index == target_number) {
            return list->current->content;
        } else if (list->current->index < target_number) {
            Node current = list->current;

            for (uint8_t counter =
                 target_number - list->current->index; counter > 0;
                 counter--) {
                current = current->next;
            }
            return current->content;
        } else {
            Node current = list->current;

            for (uint8_t counter =
                 list->current->index - target_number; counter > 0;
                 counter--) {
                current = current->previous;
            }
            return current->content;
        }
    } else {
        Node current = list->first;

        for (uint8_t counter = target_number - current->index;
             counter > 0; counter--) {
            current = current->next;
        }
        return current->content;
    }
}


void FACTION(set_name) (Faction_List self, char *name) {
    Custom_Property property = self->property;
    if (property->name != NULL) {
        free(property->name);
    }
    property->name = String_create(name);
}


char *FACTION(name) (Faction_List self) {
    return self->property->name->str;
}


void FACTION(set_action) (Faction_List self, Action callback) {
    self->property->action = callback;
}


Action FACTION(action) (Faction_List self) {
    return self->property->action;
}


void FACTION(add_target) (Faction_List self, List target) {
    self->property->target = target;
}
