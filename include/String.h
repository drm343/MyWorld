#ifndef STRING_CLASS
#define STRING_CLASS
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "base/type.h"
#include "helper/debug.h"


typedef struct ImmutableString {
    char *str;
    size_t utf8_length;
    size_t ascii_length;
    size_t width_length;
} *ImmutableString;


/** @brief 設定字體在螢幕上的寬度
 * @param size 想設定的字體寬度
 *
 * 在開始使用 String 之前一定要先設定寬度。
 */
void String_reset_width(int64_t size);

/** @brief 建立 ImmutableString 物件
 * @param str c 版本的 char *
 * @return ImmutableString 物件
 */
ImmutableString String_create(const char *str);

/** @brief 複製字串
 * @param self 想複製的 String 來源
 * @return 新字串
 */
ImmutableString String_copy(ImmutableString self);

/** @brief 將兩個 String 相連產生新字串
 * @param self 原始 String
 * @param str 想插入的 c String 來源
 * @return 新字串
 */
ImmutableString String_append_c_str(ImmutableString self, const char *str);

/** @brief 釋放 ImmutableString 物件
 * @param self String 物件
 */
void String_free(ImmutableString self);

/** @brief 比較兩個字串是否相等
 * @param a 想比較的 String 來源
 * @param b 想比較的 String 目標 
 * @return 是否相等
 *
 * 在開始使用 String 之前一定要先設定寬度。
 */
Yes_No String_equal(ImmutableString a, ImmutableString b);
#endif
