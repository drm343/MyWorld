#include "MWMutableString.h"

#include <stdio.h>


@implementation MWMutableString
+(id)create_with_c_string: (const char *) str {
    MWMutableString *obj = [[MWMutableString alloc] set: str];
    return obj;
}
+(id)copy: (id) other {
    MWMutableString *obj = [MWMutableString alloc];
    [obj set_by_cf: CFStringCreateMutableCopy( NULL, 0, [other get])];
    return obj;
}
-(void)dealloc {
    CFRelease( mstr );
    [super dealloc];
}
-(CFMutableStringRef)get {
    return mstr;
}
-(const char *)get_c_string {
    return CFStringGetCStringPtr(mstr, kCFStringEncodingASCII);
}
-(id)set: (const char *) str {
    if (mstr != NULL) {
        CFRelease(mstr);
        mstr = NULL;
    }
    mstr = CFStringCreateMutable( NULL, 0 );
    CFStringAppendCString( mstr, str, kCFStringEncodingASCII);
    return self;
}
-(id)set_by_cf: (CFMutableStringRef) str {
    if (mstr != NULL) {
        CFRelease(mstr);
        mstr = NULL;
    }
    mstr = str;
    return self;
}
-(id)append: (const char *) str {
    CFStringAppendCString( mstr, str, kCFStringEncodingASCII);
    return self;
}
-(void)debug{
    CFShow( mstr );
}
@end
