#ifndef HEADER_STRING_HELPER
#define HEADER_STRING_HELPER

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
  void (*remove_last)(char **);
  size_t (*strlen)(char *);
  size_t (*strlen_ascii)(char *);
  size_t (*count_width)(char *, int64_t);
} String_API;

extern String_API string;

#endif
