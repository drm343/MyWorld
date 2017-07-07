#include "base_type.h"


static bool eq(Point_Access point_1, Point_Access point_2) {
  bool result = false;

  if(point_1->x == point_2->x && point_1->y == point_2->y) {
    result = true;
  }
  return result;
}

POINT_INTERFACE Point = {
  .eq = eq
};
