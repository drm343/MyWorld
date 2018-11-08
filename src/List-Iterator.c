#include "List-Iterator.h"


typedef struct Custom_Property {
    void *next;
    void (*super_free) (Iterator self);
} *Custom_Property;


static void ITER(free) (Iterator self) {
    void (*super_free) (List self) = self->property->super_free;

    free(self->property);
    super_free(self);
}


Iterator ITER(create) (void) {
    Iterator self = LIST(create) (NULL, NULL, NULL);
    self->property = NEW(Custom_Property);
    self->property->super_free = self->free;
    self->free = ITER(free);
    return self;
}


void ITER(copy) (Iterator self, List from) {
    for (void *item = from->reset_iterator(from); item != NULL;
         item = from->next(from, item)) {
        self->insert(self, item);
    }
}


void *ITER(reset_iterator) (Iterator self) {
    void *next = self->reset_iterator(self);
    self->property->next = next;
    return next;
}


void *ITER(next) (Iterator self) {
    void *next = self->next(self, self->property->next);
    self->property->next = next;
    return next;
}


bool ITER(done) (Iterator self) {
    if (self->property->next == NULL) {
        self->free(self);
        return true;
    } else {
        return false;
    }
}
