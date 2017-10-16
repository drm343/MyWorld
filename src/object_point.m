#include <stdio.h>
#include "object/point.h"


@implementation Point_Type

@synthesize x;
@synthesize y;

+ (id) create {
  Point_Access result = [[Point_Type alloc] init];
  return [result autorelease];
}

- (id) init {
  self = [super init];
  if (self) {
    [self setX: 0];
    [self setY: 0];
  }
  return self;
}

- (id) addX: (int32_t) value {
  [self setX: x + value];
  return self;
}

- (id) addY: (int32_t) value {
  [self setY: y + value];
  return self;
}

- (bool) eq: (Point_Type *) other {
  bool result = false;

  if ([self x] == [other x] && [self y] == [other y]) {
    result = true;
  }
  return result;
}

- (void) print {
  printf("point (%d, %d)\n", [self x], [self y]);
}

@end


@implementation Rectangle_Type
+ (id) create {
  Rectangle_Access rectangle = [[Rectangle_Type alloc] init];
  return [rectangle autorelease];
}

- (id) init {
  self = [super init];
  if (self) {
    [self set_top_left_point: [Point_Type create]];
    [self set_down_right_point: [Point_Type create]];
  }
  return self;
}

- (Point_Access) top_left_point {
  return top_left_point;
}

- (id) set_top_left_point: (Point_Access) point {
  top_left_point = point;
}

- (Point_Access) down_right_point {
  return down_right_point;
}

- (id) set_down_right_point: (Point_Access) point {
  down_right_point = point;
}
@end
