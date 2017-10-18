#ifndef HEADER_STRING_HELPER
#define HEADER_STRING_HELPER
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>


#include "strings.h"


typedef struct strings * String_Intern;


/** @brief 求出 UTF8 版本的 string 長度
 * @param str 想求長度的 String 物件
 * @return 長度
 */
size_t String_length(const char *str);


/** @brief 求出 ascii 版本的 string 長度
 * @param str 想求長度的 String 物件
 * @return 長度
 */
size_t String_ascii_length(const char *str);


/** @brief 求出顯示在螢幕所需要的長度
 * @param str 想求長度的 String 物件
 * @param full_size 一個全形文字的長度
 * @return 長度
 */
size_t String_width_length(const char *str, int64_t full_size);
#endif
