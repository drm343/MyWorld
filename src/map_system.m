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
    [self setStart: [Point_Type create]];
    [self setEnd: [Point_Type create]];
  }
  return self;
}

- (id) init_start: (int32_t) x and: (int32_t) y {
  [start setX: x];
  [start setY: y];
  return self;
}

- (id) init_end: (int32_t) x and: (int32_t) y {
  [end setX: x];
  [end setY: y];
  return self;
}

- (int32_t) get_start_x {
  return [start x];
}

- (int32_t) get_start_y {
  return [start y];
}

- (int32_t) get_end_x {
  return [end x];
}

- (int32_t) get_end_y {
  return [end y];
}

- (id) add_start_x: (int32_t) x and_y: (int32_t) y {
  [start addX: x];
  [start addY: y];
  return self;
}

- (id) add_end_x: (int32_t) x and_y: (int32_t) y {
  [end addX: x];
  [end addY: y];
  return self;
}

- (id) add_start_x: (int32_t) x {
  [start addX: x];
  return self;
}

- (id) add_start_y: (int32_t) y {
  [start addY: y];
  return self;
}

- (id) add_end_x: (int32_t) x {
  [end addX: x];
  return self;
}

- (id) add_end_y: (int32_t) y {
  [end addY: y];
  return self;
}

@end
