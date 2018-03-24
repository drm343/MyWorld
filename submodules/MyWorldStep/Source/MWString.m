#include "MWString.h"

#include <stdio.h>


@implementation MWString
+(id)create_with_c_string: (char *) str {
    MWString *obj = [[MWString alloc] set: str];
    return obj;
}
-(id)set: (char *) str {
    if (mstr != NULL) {
        CFRelease(mstr);
        mstr = NULL;
    }
    mstr = CFStringCreateWithCString( NULL, str, kCFStringEncodingASCII );
    return self;
}
-(void)dealloc {
    CFRelease( mstr );
    [super dealloc];
}
-(void)debug{
    CFShow( mstr );
}
@end
