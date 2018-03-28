#pragma once
#include <string.h>


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
-(Boolean)equal: (id) other;
/** @brief 求出 ascii 版本的 string 長度
 * @return 長度
 */
-(size_t) ascii_length;

/** @brief 求出 UTF8 版本的 string 長度
 * @return 長度
 */
-(size_t) length;

/** @brief 求出顯示在螢幕所需要的長度
 * @param full_size 一個全形文字的長度
 * @return 長度
 */
-(size_t) String_width_length: (int64_t) full_size;
@end
