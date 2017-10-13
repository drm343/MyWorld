#ifndef HEADER_MAP_SYSTEM
#define HEADER_MAP_SYSTEM
#include "base_type.h"


@interface Map_Type : NSObject {
  Point_Type *start;
  Point_Type *end;
}

@property (nonatomic, assign) Point_Type *start;
@property (nonatomic, assign) Point_Type *end;

+ (id) create;
- (id) init;
- (id) init_start: (int32_t) x and: (int32_t) y;
- (id) init_end: (int32_t) x and: (int32_t) y;
- (int32_t) get_start_x;
- (int32_t) get_start_y;
- (int32_t) get_end_x;
- (int32_t) get_end_y;
- (id) add_start_x: (int32_t) x and_y: (int32_t) y;
- (id) add_end_x: (int32_t) x and_y: (int32_t) y;
- (id) add_start_x: (int32_t) x;
- (id) add_start_y: (int32_t) y;
- (id) add_end_x: (int32_t) x;
- (id) add_end_y: (int32_t) y;
@end
typedef Map_Type * Map_Access;
#endif
