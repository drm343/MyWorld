#include "int.h"

#define STACK_OBJ_SIZE 2
#define HEAP_OBJ_SIZE 2
#define STACK_BLOCK_SIZE (2 + 1)
#include "pool-memory.c"


struct _int *int_malloc(void)
{
    return pool_malloc(sizeof(struct _int));
}


static void int_free_callback(void *ptr)
{
    struct _int *a = ptr;
    a->a = 0;
    a->b = 0;
}


void int_free(void *ptr)
{
    // pool_free(ptr, NULL);
    pool_free(ptr, int_free_callback);
}


void int_stop(void)
{
    pool_stop(sizeof(struct _int), int_free_callback);
}


void int_a(struct _int *a, int8_t num)
{
    a->a = num;
}


void int_b(struct _int *a, int8_t num)
{
    a->b = num;
}


int8_t int_result(struct _int *a)
{
    return a->a + a->b;
}
