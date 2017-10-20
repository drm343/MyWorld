#include "helper/strings.h"


/** @brief 求出 UTF8 版本的 string 長度
 * @param str 想求長度的 String 物件
 * @return 長度
 */
size_t String_length(const char *str)
{
    size_t max_counter = strlen(str);
    size_t move_position = 0;
    int64_t len = 0;
    unsigned char check = 0;

    for (size_t index = 0; index < max_counter; index += move_position) {
        check = str[index];
        len++;

        if (check < 0b11000000) {
            move_position = 1;
        } else if (check < 0b11100000 && check > 0b11000000) {
            move_position = 2;
        } else if (check < 0b11110000 && check > 0b11100000) {
            move_position = 3;
        } else if (check > 0b11110000) {
            move_position = 4;
        } else if (check == 0) {
            break;
        } else {
            #ifdef DEBUG
            DEBUG_PRINT("%s is not utf8 string or bug\n", str);
            #endif
            break;
        }
    }
    return len;
}


/** @brief 求出 ascii 版本的 string 長度
 * @param str 想求長度的 String 物件
 * @return 長度
 */
size_t String_ascii_length(const char *str)
{
    return strlen(str);
}


/** @brief 求出顯示在螢幕所需要的長度
 * @param str 想求長度的 String 物件
 * @param full_size 一個全形文字的長度
 * @return 長度
 */
size_t String_width_length(const char *str, int64_t full_size)
{
    size_t max_counter = strlen(str);
    size_t index = 0;
    size_t move_position = 0;
    int64_t len = 0;
    unsigned char check = 0;

    for (index = 0; index < max_counter; index += move_position) {
        check = str[index];
        len += 2;

        if (check < 0b11000000) {
            move_position = 1;
            len--;
        } else if (check < 0b11100000 && check > 0b11000000) {
            move_position = 2;
        } else if (check < 0b11110000 && check > 0b11100000) {
            move_position = 3;
        } else if (check > 0b11110000) {
            move_position = 4;
        } else if (check == 0) {
            break;
        } else {
            #ifdef DEBUG
            DEBUG_PRINT("%s is not utf8 string or bug\n", str);
            #endif
            break;
        }
    }
    return len * (full_size / 2);
}
