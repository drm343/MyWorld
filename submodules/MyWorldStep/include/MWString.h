#pragma once
#include "MWObject.h"
#include <CoreFoundation/CoreFoundation.h>


@interface MWString : MWObject {
    CFStringRef mstr;
}
+(id)create_with_c_string: (char *) str;
-(id)set: (char *) str;
-(void)debug;
-(void)dealloc;
@end
