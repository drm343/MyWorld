#ifndef HEADER_OBJECT_POINT_H
#define HEADER_OBJECT_POINT_H

#import <Foundation/Foundation.h>


@interface Point_Type : NSObject {
  int32_t x;
  int32_t y;
}

@property (assign) int32_t x;
@property (assign) int32_t y;

+ (id) create;
- (id) init;
- (id) addX: (int32_t) value;
- (id) addY: (int32_t) value;
- (bool) eq: (Point_Type *) other;
- (void) print;
@end
typedef Point_Type * Point_Access;


@interface Rectangle_Type : NSObject {
  Point_Type *top_left_point;
  Point_Type *down_right_point;
}

+ (id) create;
- (id) init;
- (Point_Access) top_left_point;
- (id) set_top_left_point: (Point_Access) point;
- (Point_Access) down_right_point;
- (id) set_down_right_point: (Point_Access) point;
@end
typedef Rectangle_Type * Rectangle_Access;
#endif
