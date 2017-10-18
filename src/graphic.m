#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphic.h"
#include "helper_function/generic_function.h"


#define SELF(name) GRAPHIC_##name

static Style_Access SELF(malloc) (Style_Pool_Access pool_access) {
  int8_t start = pool_access->max_size - pool_access->current_size;
  Style_Access result = &(pool_access->pool[start]);

  pool_access->current_size -= 1;
  return result;
}

static Style_Access SELF(find) (Style_Pool_Access pool_access, const char *name) {
  uint8_t count = 0;
  uint8_t used = pool_access->max_size - pool_access->current_size;
  Style_Access result = NULL;

  for (count; count < used; count++) {
    result = &(pool_access->pool[count]);

    if (result->name == name) {
      return result;
    }
  }
  result = NULL;

  return result;
}

static void print_graph(Style_Pool_Access pool_access) {
  uint8_t count = 0;
  uint8_t used = pool_access->max_size - pool_access->current_size;
  Style_Access result = NULL;

  printf("---- graph ----\n");
  for (count; count < used; count++) {
    result = &(pool_access->pool[count]);
    printf("%s : %s\n", result->name, result->mark);
  }
  printf("---- graph done ----\n");
}

static Style_Access SELF(next) (Style_Pool_Access pool_access, uint8_t * current_counter) {
  uint8_t counter = *current_counter;
  uint8_t used = pool_access->max_size - pool_access->current_size;
  Style_Access result = NULL;

  if (counter < used) {
    result = &(pool_access->pool[counter]);
    *current_counter += 1;
  }

  return result;
}

static Style_Pool_Access SELF(start) (int size) {
  Style_Access style_memory = calloc(size, sizeof(Style));
  Style_Pool_Access pool_access = calloc(1, sizeof(Style_Pool));

  pool_access->pool = style_memory;
  pool_access->max_size = size;
  pool_access->current_size = size;
  return pool_access;
}

static void SELF(gc) (Style_Pool_Access pool_access) {
  uint8_t count = 0;
  uint8_t used = pool_access->max_size - pool_access->current_size;
  Style_Access result = NULL;

  for (count; count < used; count++) {
    result = &(pool_access->pool[count]);

    SDL_DestroyTexture(result->access);
    result->access = NULL;
  }
}

static void SELF(stop) (Style_Pool_Access pool_access) {
  free(pool_access->pool);
  free(pool_access);
}

STYLE_POOL_INTERFACE Style_Pool_Interface = {
  .start = SELF(start),
  .stop = SELF(stop),
  .malloc = SELF(malloc),
  .gc = SELF(gc),
  .find = SELF(find),
  .next = SELF(next),
  .debug = print_graph
};

#undef SELF
