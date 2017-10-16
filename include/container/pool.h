#ifndef HEADER_CONTAINER_POOL_H
#define HEADER_CONTAINER_POOL_H

#include "base_type.h"


#define CONTAINER_POOL(name, struct_name) \
typedef struct _##struct_name##_Item { \
  struct_name *content; \
  Use_Type is_used; \
} struct_name##_Item; \
\
typedef struct { \
  struct_name##_Item *pool; \
  struct_name *item; \
  uint8_t max_size; \
} name


#define CONTAINER_POOL_FUNCTIONS(name, struct_name) \
static name* name##_start(uint8_t max_size) { \
  name *result = calloc(1, sizeof(name)); \
  struct_name *instance = calloc(max_size, sizeof(struct_name)); \
  struct_name##_Item *pool = calloc(max_size, sizeof(struct_name##_Item)); \
\
  struct_name *content = NULL; \
  struct_name##_Item *item = NULL; \
\
  result->pool = pool; \
  result->item = instance; \
  result->max_size = max_size; \
\
  for (uint8_t index = 0; index < max_size; index++) { \
    content = &(result->item[index]); \
\
    item = &(result->pool[index]); \
    item->is_used = UNUSE;\
    item->content = content; \
  } \
  return result; \
} \
\
static void name##_stop(name *access) { \
  free(access->pool); \
  free(access->item); \
  free(access); \
} \
\
static struct_name * name##_malloc(name *access) { \
  struct_name *content = NULL; \
  struct_name##_Item *item = NULL; \
  uint8_t max_size = access->max_size; \
\
  for (uint8_t index = 0; index < max_size; index++) { \
    item = &(access->pool[index]); \
\
    if (item->is_used == UNUSE) { \
      item->is_used = IN_USE; \
      content = item->content; \
      break; \
    } \
  } \
  return content; \
}
#endif
