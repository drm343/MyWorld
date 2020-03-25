#include "List-Iterator.h"


typedef struct Custom_Property {
    void *next;
    void (*super_free)(Iterator self);
} *Custom_Property;


static void Iterator_free (Iterator self) {
    Custom_Property property = self->property;
    void (*super_free)(List self) = property->super_free;

    free(property);
    super_free(self);
}


Iterator Iterator_create (void) {
    Iterator self = list_create (NULL, NULL, NULL);
    self->property = NEW(Custom_Property);
    void (*super_free)(List self) = self->free;
    self->property->super_free = super_free;
    self->free = Iterator_free;
    return self;
}


void Iterator_copy (Iterator self, List from) {
    for (void *item = from->reset_iterator(from); item != NULL;
         item = from->next(from, item)) {
        self->insert(self, item);
    }
}


void *Iterator_reset_iterator (Iterator self) {
    void *next = self->reset_iterator(self);
    self->property->next = next;
    return next;
}


void *Iterator_next (Iterator self) {
    void *next = self->next(self, self->property->next);
    self->property->next = next;
    return next;
}


bool Iterator_done (Iterator self) {
    if (self->property->next == NULL) {
        self->free(self);
        return true;
    } else {
        return false;
    }
}
