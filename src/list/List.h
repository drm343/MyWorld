#ifndef HEADER_LIST_CLASS
#define HEADER_LIST_CLASS

#include "BaseClass.h"
#include "String.h"


typedef void (*CALLBACK)(void *content);
typedef bool (*COMPARE)(void *content_1, void *content_2);
typedef struct List *List;


typedef struct Node {
    uint8_t index;
    struct Node *next;
    struct Node *previous;
    void *content;
} *Node;


typedef struct List_Property {
    uint8_t counter;
    Node first;
    Node last;
    Node current;
    List owner;
} *List_Property;


typedef struct List {
    Class class;
     List_Property list;
    Custom_Property property;

    void (*free_content)(void *content);
    void (*show_content)(void *content);
    bool (*content_is_equal)(void *content_1, void *content_2);
    void (*free)(struct List * self);
     uint8_t(*insert) (struct List * self, void *item);
    bool (*remove)(struct List * self, void *item);
    void (*show)(struct List * self);
    void *(*reset_iterator)(struct List * self);
    void *(*next)(struct List * self, void *item);
     uint8_t(*size) (struct List * self);
} *List;


List list_create (COMPARE content_is_equal, CALLBACK free_content,
                   CALLBACK show_content);
#endif
