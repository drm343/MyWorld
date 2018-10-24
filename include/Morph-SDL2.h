#ifndef HEADER_MORPH_SDL2
#define HEADER_MORPH_SDL2

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>


#include "Morph.h"


#define M_SDL2(name) Morph_SDL2_##name


/** @brief 設定 SDL2 使用的 Font
 * @param font 成功開啟的 TTF_Font
*/
void M_SDL2(set_font) (TTF_Font * font);


/** @brief 設定 SDL2 使用的 Font
 * @return 取出 TTF_Font 設定給 SDL2 based 的 Morph 使用
*/
TTF_Font *M_SDL2(font) (void);

#endif
