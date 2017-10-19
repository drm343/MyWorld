#ifndef HEADER_BASE_TYPE
#define HEADER_BASE_TYPE
#include <stdint.h>
#include <stdbool.h>


typedef uint8_t Natural;

typedef enum {
    EXECUTE_SUCCESS = 0,
    EXECUTE_FAILED,
} Execute_Result;

typedef enum {
    UNUSE,
    IN_USE
} Use_Type;

/**
 * Remove Ojbective-C version's BOOL type
 */
#undef NO
#undef YES

typedef enum {
    NO,
    YES
} Yes_No;
#endif
