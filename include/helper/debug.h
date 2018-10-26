#ifndef HEADER_DEBUG_FUNCTION
#define HEADER_DEBUG_FUNCTION

#include <stdio.h>

#define DEBUG_PRINT(fmt, ...) \
do { if (DEBUG) fprintf(stderr, "\n[%s:%d]\n%s() -> " fmt, __FILE__, \
                        __LINE__, __func__, __VA_ARGS__); } while (0) \


#define DEBUG_MESSAGE(fmt) \
{ if (DEBUG) fprintf(stderr, "\n[%s:%d]\n%s() -> " fmt, __FILE__, \
                        __LINE__, __func__); }

#define BOOL_STRING(result) result ? "true" : "false"
#endif
