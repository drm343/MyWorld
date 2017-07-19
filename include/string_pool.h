#ifndef HEADER_STRING_POOL
#define HEADER_STRING_POOL
#include <stdint.h>

typedef char * String;
typedef struct String_Pool_Origin {
  String pool;
  int16_t max_size;
  int16_t current_size;
} String_Pool;
typedef String_Pool * String_Pool_Access;

typedef struct {
  String_Pool_Access (*start)(uint16_t);
  void (*stop)(String_Pool_Access);
  String (*malloc)(String_Pool_Access, uint16_t);
  void (*reset)(String_Pool_Access);
} String_Pool_API;
extern String_Pool_API string_pool;
#endif
