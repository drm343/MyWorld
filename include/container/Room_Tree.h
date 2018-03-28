#ifndef HEADER_CONTAINER_TREE_Room_Tree
#define HEADER_CONTAINER_TREE_Room_Tree

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include "room.h"

/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

typedef struct Room_Tree_Result {
    Room *parent;
    Room *child;
} Room_Tree_Result;

typedef struct Room_Tree_Node {
    struct Room_Tree_Node *parent;
    struct Room_Tree_Node *left;
    struct Room_Tree_Node *right;
    Room *content;
} Room_Tree_Node;

typedef struct Room_Tree {
    Room_Tree_Node *pool;
    Room_Tree_Node *root;
    uint32_t max_size;
    uint32_t used;
} Room_Tree;

#define Room_Tree_destruct(PARENT, CHILD, RESULT) \
{\
Room_Tree_Result *DISCARD = RESULT;\
PARENT = DISCARD->parent;\
CHILD = DISCARD->child;\
free(DISCARD);\
}

Room_Tree *Room_Tree_start(uint32_t max_size);
void Room_Tree_stop(Room_Tree * self);
Room_Tree_Result *Room_insert(Room_Tree * self, Room_Tree * item);

#endif