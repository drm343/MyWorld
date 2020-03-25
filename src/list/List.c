#include "List.h"


typedef struct Custom_Property {
} *Custom_Property;


static void list_free (List self) {
    if (self == NULL) {
        return;
    }
    List_Property list = self->list;
    Node current = NULL;
    Node next = NULL;
    void (*free_content)(void *content) = self->free_content;

    if (list->first != NULL) {
        current = list->first;
    }
    while (current != NULL) {
        next = current->next;

        if (free_content != NULL) {
            free_content(current->content);
        }

        free(current);
        current = next;
    }
    list->owner = NULL;
    free(list);
    free(self);
}


static uint8_t list_insert (List self, void *item) {
    List_Property list = self->list;
    uint8_t counter = list->counter;

    Node node;
    Node previous;
    node = calloc(1, sizeof(*node));

    node->next = NULL;
    node->content = item;
    counter++;
    node->index = counter;

    if (list->last == NULL) {
        node->previous = NULL;
    } else {
        previous = list->last;
        previous->next = node;
        node->previous = previous;
    }
    list->last = node;

    list->counter = counter;

    if (list->first == NULL) {
        list->first = node;
    }
    return counter;
}


static bool list_remove (List self, void *item) {
    List_Property list = self->list;

    Node current = list->first;
    Node next = NULL;
    Node previous = NULL;
    void *current_item = NULL;
    bool (*content_is_equal)(void *content_1, void *content_2) =
        self->content_is_equal;

    while (current != NULL) {
        next = current->next;
        previous = current->previous;
        current_item = current->content;

        if (content_is_equal != NULL
            && content_is_equal(current_item, item) == true) {
            if (next != NULL) {
                next->previous = previous;
            }

            if (previous != NULL) {
                previous->next = next;
            }

            current->content = NULL;
            current->next = NULL;
            current->previous = NULL;

            if (current == list->first) {
                list->first = next;
            } else if (current == list->last) {
                list->last = previous;
            }
            list->counter = list->counter - 1;
            free(current);
            return true;
        }
        current = next;
    }
    return false;
}


static void list_show (List self) {
    List_Property list = self->list;
    Node current;
    Node next;
    CALLBACK show_content = self->show_content;

    if (list->first != NULL) {
        if (show_content != NULL) {
            current = list->first;

            while (current != NULL) {
                next = current->next;

                show_content(current->content);
                current = next;
            }
        }
    }
}


static void *list_reset_iterator (List self) {
    List_Property list = self->list;
    Node current = list->first;
    list->current = current;

    if (current != NULL) {
        return current->content;
    } else {
        return NULL;
    }
}


static void *list_next (List self, void *item) {
    List_Property list = self->list;

    Node current = NULL;

    Node next = NULL;
    Node previous = NULL;
    void *current_item = NULL;
    bool (*content_is_equal)(void *content_1, void *content_2) =
        self->content_is_equal;

    bool BACKWARD = true;
    bool FORWARD = true;

  FIND_ITEM:
    if (list->current != NULL) {
        current = list->current;
    } else {
        current = list->first;
    }
    while (current != NULL) {
        next = current->next;
        previous = current->previous;
        current_item = current->content;

        if (content_is_equal != NULL
            && content_is_equal(current_item, item) == true) {
            list->current = next;
            if (list->current != NULL) {
                return list->current->content;
            } else {
                return NULL;
            }
        } else if (content_is_equal == NULL && current_item == item) {
            list->current = next;
            if (list->current != NULL) {
                return list->current->content;
            } else {
                return NULL;
            }
        }

        if (BACKWARD == true) {
            current = previous;
        } else if (FORWARD == true) {
            current = next;
        } else {
            break;
        }
    }

    if (BACKWARD == true) {
        BACKWARD = false;
        goto FIND_ITEM;
    } else if (FORWARD == true) {
        FORWARD = false;
    }
    return NULL;
}


static uint8_t list_size (List self) {
    return self->list->counter;
}


List list_create (COMPARE content_is_equal, CALLBACK free_content,
                   CALLBACK show_content) {
    List self;
    self = calloc(1, sizeof(*self));

    List_Property list;
    list = calloc(1, sizeof(*list));
    list->counter = 0;
    list->first = NULL;
    list->last = NULL;
    list->owner = NULL;

    self->list = list;
    self->property = NULL;

    self->free_content = free_content;
    self->show_content = show_content;
    self->content_is_equal = content_is_equal;
    self->free = list_free;
    self->insert = list_insert;
    self->remove = list_remove;
    self->show = list_show;
    self->reset_iterator = list_reset_iterator;
    self->next = list_next;
    self->size = list_size;
    return self;
}
