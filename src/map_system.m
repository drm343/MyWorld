#import "map_system.h"

@implementation Map_Type

@synthesize start;
@synthesize end;

+ (id) create {
  Map_Access map = [[Map_Type alloc] init];
  return [map autorelease];
}

- (id) init {
  self = [super init];
  if (self) {
    [self setStart: Point_Type_create()];
    [self setEnd: Point_Type_create()];
  }
  return self;
}

- (void) dealloc {
  Point_Type_free(start);
  Point_Type_free(end);
  [super dealloc];
}

- (id) init_start: (int32_t) x and: (int32_t) y {
  Point_Access_change(start);
  Point_Access_set_x(x);
  Point_Access_set_y(y);
  return self;
}

- (id) init_end: (int32_t) x and: (int32_t) y {
  Point_Access_change(end);
  Point_Access_set_x(x);
  Point_Access_set_y(y);
  return self;
}

- (int32_t) get_start_x {
  return Point_Type_x(start);
}

- (int32_t) get_start_y {
  return Point_Type_y(start);
}

- (int32_t) get_end_x {
  return Point_Type_x(end);
}

- (int32_t) get_end_y {
  return Point_Type_y(end);
}

- (id) add_start_x: (int32_t) x and_y: (int32_t) y {
  Point_Access_change(start);
  Point_Access_add_x(x);
  Point_Access_add_y(y);
  return self;
}

- (id) add_end_x: (int32_t) x and_y: (int32_t) y {
  Point_Access_change(end);
  Point_Access_add_x(x);
  Point_Access_add_y(y);
  return self;
}

- (id) add_start_x: (int32_t) x {
  Point_Type_add_x(start, x);
  return self;
}

- (id) add_start_y: (int32_t) y {
  Point_Type_add_y(start, y);
  return self;
}

- (id) add_end_x: (int32_t) x {
  Point_Type_add_x(end, x);
  return self;
}

- (id) add_end_y: (int32_t) y {
  Point_Type_add_y(end, y);
  return self;
}

@end