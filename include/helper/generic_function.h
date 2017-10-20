#ifndef HEADER_GENERIC_FUNCTION_H
#define HEADER_GENERIC_FUNCTION_H

#include <stdint.h>
#include <stdlib.h>


#define GENERIC_POOL(name, struct_name) typedef struct { \
  struct_name pool; \
  uint8_t max_size; \
  uint8_t current_size; \
} name


#define GENERIC_ARRAY(name, struct_name) typedef struct { \
  struct_name **pool; \
  uint8_t max_size; \
  uint8_t used; \
} name; \
\
typedef name * name##_Access


#define GENERIC_ARRAY_FUNCTIONS(name, struct_name) \
static name##_Access name##_start(uint8_t size) { \
  struct_name **pool = calloc(size, sizeof(struct_name *)); \
  name##_Access pool_access = calloc(1, sizeof(name)); \
\
  pool_access->pool = pool; \
  pool_access->max_size = size; \
  pool_access->used = 0; \
\
  return pool_access; \
} \
\
static void name##_stop(name##_Access pool_access) { \
  free(pool_access->pool); \
  free(pool_access); \
} \
static void name##_add(name##_Access pool_access, struct_name *item) { \
  uint8_t current = pool_access->used; \
  uint8_t max_size = pool_access->max_size; \
\
  if (current < max_size) { \
    pool_access->pool[current] = item; \
    pool_access->used = current + 1; \
  } \
} \
static void name##_copy(name##_Access from, name##_Access to) { \
  uint8_t count = 0; \
  uint8_t used = from->used; \
\
  Status *item = NULL; \
\
  for (count; count < used; count++) { \
    item = from->pool[count]; \
    name##_add(to, item); \
  } \
}

#define STRCMP(x, y) !strcmp(x, y)

#endif
