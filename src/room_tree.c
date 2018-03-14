#include "container/Room_Tree.h"
/**
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

Room_Tree *Room_Tree_start(uint32_t max_size)
{
    Room_Tree *self = calloc(1, sizeof(Room_Tree));
    Room_Tree_Node *pool = calloc(max_size, sizeof(Room_Tree));

    self->root = NULL;
    self->pool = pool;
    self->max_size = max_size;
    self->used = 0;
    return self;
}

void Room_Tree_stop(Room_Tree * self)
{
    free(self->pool);
    free(self);
}

Room_Tree_Result *Room_Tree_insert(Room_Tree * self, Room * item)
{
    Room_Tree_Result *result = calloc(1, sizeof(Room_Tree_Result));
    bool is_left = true;

    Room_Tree_Node *root = self->root;
    Room_Tree_Node *parent = NULL;
    Room_Tree_Node *current = &(self->pool[self->used]);

    current->parent = NULL;
    current->left = NULL;
    current->right = NULL;
    current->content = item;
    self->used = self->used + 1;

    if (root == NULL) {
        self->root = current;
        result->parent = current->content;
        result->child = NULL;
        return result;
    }

 next:
    if (root == NULL) {
        current->parent = parent;

        if (is_left) {
            parent->left = current;
        } else {
            parent->right = current;
        }
        result->parent = parent->content;
        result->child = current->content;
        return result;
    } else if (Room_compare_small(item, root->content)) {
        is_left = true;
        parent = root;
        root = root->left;
        goto next;
    } else {
        is_left = false;
        parent = root;
        root = root->right;
        goto next;
    }
}
