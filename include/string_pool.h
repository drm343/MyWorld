#ifndef HEADER_STRING_POOL
#define HEADER_STRING_POOL

#include <stdint.h>

#define STRING_POOL_init(obj, value) String_Pool obj = {}; \
String_Pool_start(obj, value);

#define STRING_POOL_ON_STACK(var) String_Pool var##_on_stack = {};\
String_Pool_Access var = &var##_on_stack

typedef char * String;
typedef struct String_Pool_Origin {
  String pool;
  int16_t max_size;
  int16_t current_size;

	String (*malloc)(struct String_Pool_Origin *, int);
	void (*reset)(struct String_Pool_Origin *);
	void (*stop)(struct String_Pool_Origin *);
} String_Pool;
typedef String_Pool * String_Pool_Access;

//void String_Pool_start(String_Pool_Access, int);
String_Pool_Access String_Pool_start_heap(int);
void String_Pool_start_stack(String_Pool_Access, int);

#endif
