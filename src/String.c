#include "String.h"


/** @brief 求出 UTF8 版本的 string 長度
 * @param str 想求長度的 String 物件
 * @return 長度
 */
static size_t String_length(const char *str)
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


/** @brief 求出顯示在螢幕所需要的長度
 * @param str 想求長度的 String 物件
 * @param full_size 一個全形文字的長度
 * @return 長度
 */
static size_t String_width_length(const char *str, int64_t full_size)
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

/** @brief 建立 ImmutableString 物件
 * @param str c 版本的 char *
 * @return ImmutableString 物件
 */
ImmutableString String_create(const char *str)
{
    ImmutableString self = calloc(1, sizeof(ImmutableString));
    char *tmp_str = calloc(1, strlen(str) + 1);
    strcpy(tmp_str, str);
    self->str = tmp_str;
    self->ascii_length = strlen(str);
    self->utf8_length = String_length(str);
    self->width_length = String_width_length(str);
    return self;
}

/** @brief 釋放 ImmutableString 物件
 * @param self String 物件
 */
void String_free(ImmutableString self)
{
    free(self->str);
    free(self);
}
