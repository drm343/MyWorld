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
