#include "List-Faction_Group.h"


typedef struct Custom_Property {
} *Custom_Property;


static void F_GROUP(free_list) (void *content) {
    List self = content;
    self->free(self);
}


Faction_Group F_GROUP(create) (void) {
    Faction_Group self = LIST(create) (NULL, F_GROUP(free_list), NULL);
    return self;
}


Faction_Group F_GROUP(create_without_free_list) (void) {
    Faction_Group self = LIST(create) (NULL, NULL, NULL);
    return self;
}


Faction_List F_GROUP(get_random_target) (Faction_Group self) {
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


void F_GROUP(link) (Faction_Group self, List item) {
    item->list->owner = self;
    self->insert(self, item);
}


Iterator F_GROUP(to_iterator) (Faction_Group self) {
    Iterator iterator = ITER(create) ();

    for (Faction_List list = self->reset_iterator(self); list != NULL;
         list = self->next(self, list)) {
        ITER(copy) (iterator, list);
    }
    return iterator;
}
