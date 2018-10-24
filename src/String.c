#include "String.h"


// @brief 字體在螢幕上的寬度
static int64_t full_size = 0;

/** @brief 設定字體在螢幕上的寬度
 * @param size 想設定的字體寬度
 */
void String_reset_width(int64_t size)
{
    full_size = size;
}


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
 * @return 長度
 */
static size_t String_width_length(const char *str)
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
    ImmutableString self = calloc(1, sizeof(struct ImmutableString));
    self->str = calloc(1, strlen(str) + 1);
    strcpy(self->str, str);
    self->ascii_length = strlen(str);
    self->utf8_length = String_length(str);
    self->width_length = String_width_length(str);
    return self;
}

/** @brief 複製字串
 * @param self 想複製的 String 來源
 * @return 新字串
 */
ImmutableString String_copy(ImmutableString self)
{
    ImmutableString other = calloc(1, sizeof(struct ImmutableString));
    other->str = calloc(1, self->ascii_length + 1);
    strcpy(other->str, self->str);
    other->ascii_length = self->ascii_length;
    other->utf8_length = self->utf8_length;
    other->width_length = self->width_length;
    return other;
}

/** @brief 將兩個 String 相連產生新字串
 * @param self 原始 String
 * @param str 想插入的 c String 來源
 * @return 新字串
 */
ImmutableString String_append_c_str(ImmutableString self, const char *str)
{
    ImmutableString other = calloc(1, sizeof(struct ImmutableString));
    other->ascii_length = self->ascii_length + strlen(str);
    other->str = calloc(1, other->ascii_length + 1);
    strcpy(other->str, self->str);
    strcat(other->str, str);
    other->utf8_length = String_length(str);
    other->width_length = String_width_length(str);
}

/** @brief 釋放 ImmutableString 物件
 * @param self String 物件
 */
void String_free(ImmutableString self)
{
    free(self->str);
    free(self);
}


/** @brief 比較兩個字串是否相等
 * @param a 想比較的 String 來源
 * @param b 想比較的 String 目標 
 * @return 是否相等
 *
 * 在開始使用 String 之前一定要先設定寬度。
 */
Yes_No String_equal(ImmutableString a, ImmutableString b)
{
    if (a->ascii_length != b->ascii_length)
    {
        return NO;
    }
    else if(strcmp(a->str, b->str) == 0)
    {
        return YES;
    }
    else
    {
        return NO;
    }
}
