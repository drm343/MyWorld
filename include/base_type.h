#ifndef HEADER_BASE_TYPE
#define HEADER_BASE_TYPE

#include <stdint.h>
#include <stdbool.h>

typedef uint8_t Natural;

typedef struct {
  uint16_t x;
  uint16_t y;
} Point_Type;
typedef Point_Type * Point_Access;


typedef struct {
  bool (*eq)(Point_Access, Point_Access);
} POINT_INTERFACE;

extern POINT_INTERFACE Point;

typedef enum {
  EXECUTE_SUCCESS = 0,
  EXECUTE_FAILED,
} Execute_Result;

#endif
