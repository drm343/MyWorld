#pragma once
#include "MWObject.h"
#include <CoreFoundation/CoreFoundation.h>


@interface MWMutableString : MWObject {
    CFMutableStringRef mstr;
}
+(id)create_with_c_string: (const char *) str;
+(id)copy: (id) other;
-(void)dealloc;
-(CFMutableStringRef)get;
-(const char *)get_c_string;
-(id)set: (const char *) str;
-(id)set_by_cf: (CFMutableStringRef) str;
-(id)append: (const char *) str;
-(void)debug;
@end
