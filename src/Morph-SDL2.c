#include "Morph-SDL2.h"


TTF_Font *USE_FONT = NULL;


/** @brief 設定 SDL2 使用的 Font
 * @param font 成功開啟的 TTF_Font
*/
void M_SDL2(set_font) (TTF_Font * font) {
    USE_FONT = font;
}


/** @brief 設定 SDL2 使用的 Font
 * @return 取出 TTF_Font 設定給 SDL2 based 的 Morph 使用
*/
TTF_Font *M_SDL2(font) (void) {
    return USE_FONT;
}
