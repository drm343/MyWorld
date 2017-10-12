#ifndef HEADER_BASE_TYPE
#define HEADER_BASE_TYPE
#include <stdint.h>
#include <stdbool.h>

#import <Foundation/Foundation.h>


typedef uint8_t Natural;

typedef enum {
  EXECUTE_SUCCESS = 0,
  EXECUTE_FAILED,
} Execute_Result;

typedef enum {
  UNUSE,
  IN_USE
} Use_Type;

typedef BOOL Yes_No;

typedef struct {
  int32_t x;
  int32_t y;
} Point_Type;
typedef Point_Type * Point_Access;


typedef struct {
  bool (*eq)(Point_Access, Point_Access);
  void (*print)(Point_Access);
} POINT_INTERFACE;

extern POINT_INTERFACE Point;

#endif
