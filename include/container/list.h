#ifndef HEADER_CONTAINER_LIST_H
#define HEADER_CONTAINER_LIST_H


#define CONTAINER_LIST(name, struct_name) \
typedef struct _##struct_name##_Node { \
  struct _##struct_name##_Node *next; \
  struct _##struct_name##_Node *previous; \
  struct_name *content; \
} struct_name##_Node; \
\
typedef struct { \
  struct_name##_Node *list; \
  uint8_t max_size; \
  uint8_t used; \
  uint8_t instance_counter; \
  struct_name##_Node *first; \
  struct_name##_Node *last; \
} name


#define CONTAINER_LIST_FUNCTIONS(name, struct_name) \
static name* name##_start(uint8_t max_size) { \
  name *result = calloc(1, sizeof(name)); \
  struct_name##_Node *list = calloc(max_size, sizeof(struct_name##_Node)); \
\
  result->list = list; \
  result->first = NULL; \
  result->last = NULL; \
  result->max_size = max_size; \
  result->used = 0; \
  result->instance_counter = 0; \
  return result; \
} \
\
static void name##_stop(name *access) { \
  free(access->list); \
  free(access); \
} \
\
static void name##_gc(name *access) { \
  printf("call gc\n"); \
  uint8_t used = access->used; \
  uint8_t instance_used = access->instance_counter; \
  struct_name##_Node *unused_node = NULL; \
  struct_name##_Node *current = access->first; \
  struct_name##_Node *next = NULL; \
  struct_name##_Node *previous = NULL; \
\
  while (current != NULL) { \
    next = current->next; \
    previous = current->previous; \
\
    for (uint8_t index = 0; index < used; index++) { \
      unused_node = &(access->list[index]); \
\
      if (unused_node == current) { \
        break; \
      } \
      else if (unused_node->content == NULL) { \
        unused_node->content = current->content; \
        unused_node->next = NULL; \
        unused_node->previous = NULL; \
        current->content = NULL; \
\
        if (current == access->first) { \
          access->first = unused_node; \
        } \
\
        if (current == access->last) { \
          access->last = unused_node; \
        } \
\
        if (previous != NULL) { \
          previous->next = unused_node; \
          current->previous = NULL; \
          unused_node ->previous = previous; \
        } \
\
        if (next != NULL) { \
          next->previous = unused_node; \
          current->next = NULL; \
          unused_node->next = next; \
        } \
        break; \
      } \
    } \
\
    current = next; \
  } \
  access->used = instance_used; \
} \
\
static uint8_t name##_insert(name *access, struct_name *item) { \
  uint8_t used = access->used; \
  uint8_t max_size = access->max_size; \
\
  if (used >= max_size) { \
    name##_gc(access); \
    used = access->used; \
  } \
\
  if (used < max_size) { \
    struct_name##_Node *first = access->first; \
    struct_name##_Node *last = access->last; \
    struct_name##_Node *node = &(access->list[used]); \
\
    node->previous = NULL; \
    node->next = NULL; \
    node->content = item; \
\
    if (first == NULL) { \
      access->first = node; \
    } \
\
    if (last == NULL) { \
      access->last = node; \
    } \
    else { \
      last->next = node; \
      node->previous = last; \
      access->last = node; \
    } \
    access->used = used + 1; \
    access->instance_counter = access->instance_counter + 1; \
    return used; \
  } \
  else { \
    return -1; \
  } \
} \
\
static void name##_remove(name *access, struct_name *item) { \
  struct_name##_Node *current = access->first; \
  struct_name##_Node *next = NULL; \
  struct_name##_Node *previous = NULL; \
  struct_name *current_item = NULL; \
\
  while (current != NULL) { \
    next = current->next; \
    previous = current->previous; \
    current_item = current->content; \
\
    if (current_item == item) { \
      next = current->next; \
      previous = current->previous; \
\
      if (next != NULL) { \
        next->previous = previous; \
      } \
\
      if (previous != NULL) { \
        previous->next = next; \
      } \
\
      current->content = NULL; \
      current->next = NULL; \
      current->previous = NULL; \
\
      if (current == access->first) { \
        access->first = next; \
      } \
      else if (current == access->last) { \
        access->last = previous; \
      } \
      access->instance_counter = access->instance_counter - 1; \
      return; \
    } \
    current = next; \
  } \
} \
\
static struct_name* name##_get_by_index(name *access, uint8_t index) { \
  struct_name##_Node *current = access->first; \
  struct_name##_Node *next = NULL; \
\
  for (uint8_t counter = 0; counter < index; counter++) { \
    next = current->next; \
    current = next; \
  } \
  return current->content; \
} \
\
static void name##_copy_all(name *from, name *to) { \
  uint8_t used = from->instance_counter; \
\
  struct_name##_Node *current = from->first; \
  struct_name##_Node *next = NULL; \
\
  for (uint8_t index = 0; index < used; index++) { \
    next = current->next; \
    name##_insert(to, current->content); \
    current = next; \
  } \
}

#endif
