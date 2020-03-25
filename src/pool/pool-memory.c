#ifndef POOL_MEMORY_H
#define POOL_MEMORY_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

//#define NDEBUG
#include <assert.h>

#ifndef STACK_OBJ_SIZE
#define STACK_OBJ_SIZE 2
#endif

#ifndef STACK_BLOCK_SIZE
#define STACK_BLOCK_SIZE (16 + 1)
#endif

#ifndef HEAP_OBJ_SIZE
#define HEAP_OBJ_SIZE 2
#endif

#ifndef POOL_SIZE
#define POOL_SIZE 2
#endif


#define DEBUG_P printf("%s %d: %d %p %d\n", __FILE__, __LINE__, is_on_stack, mem, obj_size, current_mem);
#define CURRENT_STATE printf("%s %d: %d %p %d %d\n", __FILE__, __LINE__, is_on_stack, mem, current_mem, counter);


#define P_IN_USE 1
#define P_POSITION 1            // 0 for stack and 1 for heap
#define P_SIZE 6

struct _header {
    char in_use:P_IN_USE, position:P_POSITION, size:P_SIZE;
};
static int _header_size = sizeof(struct _header);
static char position = 0;


static char STACK_VAR_MEM[STACK_BLOCK_SIZE * STACK_OBJ_SIZE + 1];
static char *pool_mem[POOL_SIZE] = { NULL, NULL };

static int counter = 0;
static char is_on_stack = 0;
static char *mem = STACK_VAR_MEM;
static int current_mem = 0;


static void pool_debug_value(void)
{
    printf("debug %d\n", STACK_OBJ_SIZE);
    printf("debug %d\n", STACK_BLOCK_SIZE);
    printf("debug %d\n", HEAP_OBJ_SIZE);
}


static uint8_t pool_mem_check(void *ptr)
{
    struct _header *header = NULL;

    char *in_use_mem = ptr;
    header = (struct _header *) (in_use_mem - _header_size);
    uint8_t in_use = header->in_use;

    return in_use;
}


static uint8_t pool_mem_position(void *ptr)
{
    struct _header *header = NULL;

    char *in_use_mem = ptr;
    header = (struct _header *) (in_use_mem - _header_size);
    uint8_t position = header->position;

    return position;
}


static uint8_t pool_mem_size(void *ptr)
{
    struct _header *header = NULL;

    char *in_use_mem = ptr;
    header = (struct _header *) (in_use_mem - _header_size);
    uint8_t size = header->size;

    return size;
}


static void *pool_malloc(int user_size)
{
    int cycle = 0;
    int compare_value = 0;

  START:
    if (mem == NULL) {
        if (HEAP_OBJ_SIZE <= 0) {
            return NULL;
        }
        mem = calloc(HEAP_OBJ_SIZE, user_size + _header_size);
        pool_mem[current_mem] = mem;
    }

    struct _header *header = NULL;

    char *ptr = NULL;
  RERUN:
    if (is_on_stack == 0) {
        compare_value = STACK_OBJ_SIZE;
    } else {
        compare_value = HEAP_OBJ_SIZE;
    }

    if (counter >= compare_value) {
        if (is_on_stack == 0) {
            is_on_stack = 1;
            current_mem = 0;
        } else {
            current_mem++;

            if (current_mem >= POOL_SIZE) {
                is_on_stack = 0;
                current_mem = 0;

                if (cycle >= 1) {
                    counter = 0;
                    return NULL;
                } else {
                    cycle++;
                }
            }
        }

        counter = 0;
        if (is_on_stack == 0) {
            mem = STACK_VAR_MEM;
        } else {
            mem = pool_mem[current_mem];
        }
        goto START;
    } else {
        ptr = &(mem[counter * (_header_size + user_size)]);
        header = (struct _header *) ptr;

        if (pool_mem_check(ptr + _header_size)) {
            counter++;
            goto RERUN;
        } else {
            header->in_use = 1;
            header->position = is_on_stack;
            header->size = user_size;
            counter++;
            ptr += _header_size;
            return ptr;
        }
    }
}


static void pool_free(void *ptr, void (*free_callback)(void *ptr))
{
    struct _header *header = NULL;

    if (free_callback != NULL) {
        free_callback(ptr);
    }
    char *in_use_mem = ptr;
    header = (struct _header *) (in_use_mem - _header_size);
    header->in_use = 0;
    ptr = NULL;
    return;
}


static void pool_stop(int user_size, void (*free_callback)(void *ptr))
{
    struct _header *header = NULL;
    is_on_stack = 0;
    current_mem = 0;
    mem = STACK_VAR_MEM;
    char *ptr = NULL;

    int obj_size = STACK_OBJ_SIZE;
    uint8_t in_use = 0;

  RERUN:
    if (mem == NULL) {
        goto NEXT_MEM;
    }

    for (counter = 0; counter < obj_size; counter++) {
        ptr = &(mem[counter * (_header_size + user_size)]);

        header = (struct _header *) ptr;

        in_use = header->in_use;
        if (in_use) {
            header->in_use = 0;
            ptr += _header_size;

            if (free_callback != NULL) {
                free_callback(ptr);
            }
        }
    }

  NEXT_MEM:
    if (is_on_stack == 0) {
        is_on_stack = 1;
        current_mem = 0;
        mem = pool_mem[current_mem];
        obj_size = HEAP_OBJ_SIZE;
        goto RERUN;
    } else if (current_mem < POOL_SIZE) {
        free(mem);
        pool_mem[current_mem] = NULL;
        obj_size = HEAP_OBJ_SIZE;
        current_mem++;

        if (current_mem < POOL_SIZE) {
            mem = pool_mem[current_mem];

            if (mem == NULL) {
                goto NEXT_MEM;
            } else {
                goto RERUN;
            }
        }
    }
}
#endif
