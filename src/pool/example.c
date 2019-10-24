#include <stdio.h>
#include <stdlib.h>

#include "pool.h"

typedef struct {
    int a;
    int b;
} Room;

void *create_content(void) {
    Room *result = NULL;
    result = malloc(sizeof(*result));
    result->a = 0;
    result->b = 0;
    printf("create %p\n", result);
    return result;
}

void free_content(void * room) {
    printf("free %p\n", room);
    free(room);
}


void init_content(void * access) {
    Room *room = access;
    room->a = 30;
    room->b = 20;
}


void demo(Pool *pool) {
    Item * item = Pool_malloc(pool, init_content);
    Room * room = Item_content(item);
    printf("%d %d\n", room->a, room->b);
    //Item_free(item);
    item = Pool_malloc(pool, NULL);
    room = Item_content(item);
    printf("%d %d\n", room->a, room->b);
}


int main(void) {
    Pool * pool = Pool_start(10, create_content, free_content);
    demo(pool);
    Pool_stop(pool);
}
