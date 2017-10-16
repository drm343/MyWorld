#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "container/pool.h"
#include "container/list.h"


typedef struct {
  char *name;
  char *race;
} F;


CONTAINER_LIST(F_List, F);
CONTAINER_LIST_FUNCTIONS(F_List, F);

CONTAINER_POOL(F_Pool, F);
CONTAINER_POOL_FUNCTIONS(F_Pool, F);


static void F_List_debug(F_List *access, char *message) {
  uint8_t used = access->used;
  uint8_t counter = 0;
  F *content = NULL;
  F_Node *item = NULL;

  item = access->first;

  printf("[%s]\n", message);
  while (item != NULL) {
    if (item != NULL) {
      content = item->content;
      printf("[name]%s\n", content->name);
      printf("[race]%s\n", content->race);
    }

    counter = counter + 1;
    item = item->next;
  }
  printf("used %d\n", access->used);
  printf("--------------------\n");
}


int main(void) {
  F_List *list = F_List_start(2);
  F_Pool *pool = F_Pool_start(10);

  F *item_1 = F_Pool_malloc(pool);
  item_1->name = "a";
  item_1->race = "gob";
  F_List_insert(list, item_1);
  F_List_debug(list, "insert item 1");

  F *item_2 = F_Pool_malloc(pool);
  item_2->name = "b";
  item_2->race = "job";
  F_List_insert(list, item_2);
  F_List_debug(list, "insert item 2");

  F_List_remove(list, item_1);
  F_List_debug(list, "remove item 1");

  F_List_insert(list, item_1);
  F_List_debug(list, "insert item 1");

  F_List_remove(list, item_1);
  F_List_gc(list);
  F_List_debug(list, "remvoe item 1");

  F_List_insert(list, item_1);
  F_List_debug(list, "insert item 1");

  F *item_3 = F_Pool_malloc(pool);
  item_3->name = "c";
  item_3->race = "god";
  F_List_insert(list, item_3);
  F_List_debug(list, "insert item 3");

  F_List_remove(list, item_2);
  F_List_insert(list, item_3);
  F_List_debug(list, "insert item 3");

  F_Pool_stop(pool);
  F_List_stop(list);
  return 0;
}
