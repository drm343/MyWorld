#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "string_pool.h"


String pool_malloc(String_Pool_Access pool_access, int from) {
  int16_t start = pool_access->max_size - pool_access->current_size;
  String result = &(pool_access->pool)[start];

  pool_access->current_size -= (from + 1);
  return result;
}


void pool_reset(String_Pool_Access pool_access) {
  memset(pool_access->pool, 0, pool_access->max_size);

  pool_access->current_size = pool_access->max_size;
}


void pool_stop_heap(String_Pool_Access pool_access) {
  free(pool_access->pool);
  free(pool_access);
}


void pool_stop_stack(String_Pool_Access pool_access) {
  free(pool_access->pool);
}


String_Pool_Access String_Pool_start_heap(int size) {
  String string_memory = calloc(size, sizeof(char));
  String_Pool_Access pool_access = calloc(1, sizeof(String_Pool));

  pool_access->pool         = string_memory;
  pool_access->max_size     = size;
  pool_access->current_size = size;

  pool_access->malloc = pool_malloc;
  pool_access->reset  = pool_reset;
  pool_access->stop   = pool_stop_heap;

  return pool_access;
}


void String_Pool_start_stack(String_Pool_Access pool_access, int size) {
  String string_memory = calloc(size, sizeof(char));

  pool_access->pool         = string_memory;
  pool_access->max_size     = size;
  pool_access->current_size = size;

  pool_access->malloc = pool_malloc;
  pool_access->reset  = pool_reset;
  pool_access->stop   = pool_stop_stack;
}
