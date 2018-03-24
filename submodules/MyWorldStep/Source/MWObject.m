#include "MWObject.h"

@implementation MWObject
+(id)alloc {
    return class_createInstance(self, 0);
}
-(void)dealloc {
    object_dispose(self);
}
@end
