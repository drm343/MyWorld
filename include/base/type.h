#ifndef HEADER_BASE_TYPE
#define HEADER_BASE_TYPE

#include <stdint.h>
#include <stdbool.h>

typedef uint8_t Natural;

typedef enum {
    NOT_FOUND = 0,
    FOUND
} Found_Result;

typedef enum {
    EXECUTE_SUCCESS = 0,
    EXECUTE_FAILED,
} Execute_Result;

typedef enum {
    UNUSE,
    IN_USE
} Use_Type;

typedef enum {
    _NO,
    _YES
} Yes_No;

#endif
