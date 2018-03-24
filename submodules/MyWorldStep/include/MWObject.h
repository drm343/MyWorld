#pragma once
#include <objc/runtime.h>


@interface MWObject
{
    Class isa;
}
+(id)alloc;
-(void)dealloc;
@end
