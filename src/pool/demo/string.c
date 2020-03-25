#include "string.h"

#define STACK_OBJ_SIZE 2
#define HEAP_OBJ_SIZE 3
#define STACK_BLOCK_SIZE (8 + 1)
#include "pool-memory.c"


static String *string_malloc(void)
{
    return pool_malloc(sizeof(String));
}


String *string_new(char *value)
{
    String *result = string_malloc();
    *result = strdup(value);
    return result;
}


static void string_free_callback(void *ptr)
{
    String *a = ptr;
    free(*a);
    *a = NULL;
}


void string_free(void *ptr)
{
    pool_free(ptr, string_free_callback);
}


void string_stop(void)
{
    pool_stop(sizeof(String), string_free_callback);
}


int string_check(String * ptr)
{
    return pool_mem_check(ptr);
}


char string_size(String * ptr)
{
    return pool_mem_size(ptr);
}
