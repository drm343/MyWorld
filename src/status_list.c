#include "container/Status_List.h"
/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

Status_List *Status_List_start(uint8_t max_size)
{
    Status_List *result = calloc(1, sizeof(Status_List));
    Status_Node *list = calloc(max_size, sizeof(Status_Node));

    result->list = list;
    result->first = NULL;
    result->last = NULL;
    result->max_size = max_size;
    result->used = 0;
    result->instance_counter = 0;
    return result;
}

void Status_List_stop(Status_List * access)
{
    free(access->list);
    free(access);
}

void Status_List_gc(Status_List * access)
{
    uint8_t used = access->used;
    uint8_t instance_used = access->instance_counter;
    Status_Node *unused_node = NULL;
    Status_Node *current = access->first;
    Status_Node *next = NULL;
    Status_Node *previous = NULL;

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

uint8_t Status_List_insert(Status_List * access, Status * item)
{
    uint8_t used = access->used;
    uint8_t max_size = access->max_size;

    if (used >= max_size) {
        Status_List_gc(access);
        used = access->used;
    }

    if (used < max_size) {
        Status_Node *first = access->first;
        Status_Node *last = access->last;
        Status_Node *node = &(access->list[used]);

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

void Status_List_remove(Status_List * access, Status * item)
{
    Status_Node *current = access->first;
    Status_Node *next = NULL;
    Status_Node *previous = NULL;
    Status *current_item = NULL;

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

Status *Status_List_get_by_index(Status_List * access, uint8_t index)
{
    Status_Node *current = access->first;
    Status_Node *next = NULL;

    for (uint8_t counter = 0; counter < index; counter++) {
        next = current->next;
        current = next;
    }
    return current->content;
}

void Status_List_copy_all(Status_List * from, Status_List * to)
{
    uint8_t used = from->instance_counter;

    Status_Node *current = from->first;
    Status_Node *next = NULL;

    for (uint8_t index = 0; index < used; index++) {
        next = current->next;
        Status_List_insert(to, current->content);
        current = next;
    }
}
