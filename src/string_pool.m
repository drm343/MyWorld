#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "string_pool.h"


String pool_malloc(String_Pool_Access pool_access, uint16_t from) {
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


String_Pool_Access String_Pool_start_heap(uint16_t size) {
  String string_memory = calloc(size, sizeof(char));
  String_Pool_Access pool_access = calloc(1, sizeof(String_Pool));

  pool_access->pool         = string_memory;
  pool_access->max_size     = size;
  pool_access->current_size = size;

  return pool_access;
}

// -----------------------------------
// Export API
// -----------------------------------
String_Pool_API string_pool = {
  .start = String_Pool_start_heap,
  .stop = pool_stop_heap,

  .malloc = pool_malloc,
  .reset  = pool_reset
};
