#include "user.h"

#define STACK_OBJ_SIZE 2
#define HEAP_OBJ_SIZE 2
#include "pool-memory.c"
#undef MEM_OBJ_SIZE


struct _user *user_malloc(void)
{
    return pool_malloc(sizeof(struct _user));
}


static void user_free_callback(void *ptr)
{
    struct _user *a = ptr;

    if (a->name != NULL) {
        free(a->name);
        a->name = NULL;
    }
    a->age = 0;
}


void user_free(void *ptr)
{
    pool_free(ptr, user_free_callback);
}


void user_stop(void)
{
    pool_stop(sizeof(struct _user), user_free_callback);
}


int user_check(struct _user *ptr)
{
    return pool_mem_check(ptr);
}


char user_size(struct _user *ptr)
{
    return pool_mem_size(ptr);
}
