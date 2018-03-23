#include "container/Character_List.h"
/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

Character_List *Character_List_start(uint8_t max_size)
{
    Character_List *result = calloc(1, sizeof(Character_List));
    Character_Node *list = calloc(max_size, sizeof(Character_Node));

    result->list = list;
    result->first = NULL;
    result->last = NULL;
    result->max_size = max_size;
    result->used = 0;
    result->instance_counter = 0;
    return result;
}

void Character_List_stop(Character_List * access)
{
    free(access->list);
    free(access);
}

void Character_List_gc(Character_List * access)
{
    uint8_t used = access->used;
    uint8_t instance_used = access->instance_counter;
    Character_Node *unused_node = NULL;
    Character_Node *current = access->first;
    Character_Node *next = NULL;
    Character_Node *previous = NULL;

    while (current != NULL) {
        next = current->next;
        previous = current->previous;

        for (uint8_t index = 0; index < used; index++) {
            unused_node = &(access->list[index]);

            if (unused_node == current) {
                break;
            } else if (unused_node->content == NULL) {
                unused_node->content = current->content;
                unused_node->next = NULL;
                unused_node->previous = NULL;
                current->content = NULL;

                if (current == access->first) {
                    access->first = unused_node;
                }

                if (current == access->last) {
                    access->last = unused_node;
                }

                if (previous != NULL) {
                    previous->next = unused_node;
                    current->previous = NULL;
                    unused_node->previous = previous;
                }

                if (next != NULL) {
                    next->previous = unused_node;
                    current->next = NULL;
                    unused_node->next = next;
                }
                break;
            }
        }
        current = next;
    }
    access->used = instance_used;
}

uint8_t Character_List_insert(Character_List * access, Character * item)
{
    uint8_t used = access->used;
    uint8_t max_size = access->max_size;

    if (used >= max_size) {
        Character_List_gc(access);
        used = access->used;
    }

    if (used < max_size) {
        Character_Node *first = access->first;
        Character_Node *last = access->last;
        Character_Node *node = &(access->list[used]);

        node->previous = NULL;
        node->next = NULL;
        node->content = item;

        if (first == NULL) {
            access->first = node;
        }

        if (last == NULL) {
            access->last = node;
        } else {
            last->next = node;
            node->previous = last;
            access->last = node;
        }
        access->used = used + 1;
        access->instance_counter = access->instance_counter + 1;
        return used;
    } else {
        return -1;
    }
}

void Character_List_remove(Character_List * access, Character * item)
{
    Character_Node *current = access->first;
    Character_Node *next = NULL;
    Character_Node *previous = NULL;
    Character *current_item = NULL;

    while (current != NULL) {
        next = current->next;
        previous = current->previous;
        current_item = current->content;

        if (current_item == item) {
            next = current->next;
            previous = current->previous;

            if (next != NULL) {
                next->previous = previous;
            }

            if (previous != NULL) {
                previous->next = next;
            }

            current->content = NULL;
            current->next = NULL;
            current->previous = NULL;

            if (current == access->first) {
                access->first = next;
            } else if (current == access->last) {
                access->last = previous;
            }
            access->instance_counter = access->instance_counter - 1;
            return;
        }
        current = next;
    }
}

Character *Character_List_get_by_index(Character_List * access,
                                       uint8_t index)
{
    Character_Node *current = access->first;
    Character_Node *next = NULL;

    for (uint8_t counter = 0; counter < index; counter++) {
        next = current->next;
        current = next;
    }
    return current->content;
}

void Character_List_copy_all(Character_List * from, Character_List * to)
{
    uint8_t used = from->instance_counter;

    Character_Node *current = from->first;
    Character_Node *next = NULL;

    for (uint8_t index = 0; index < used; index++) {
        next = current->next;
        Character_List_insert(to, current->content);
        current = next;
    }
}
