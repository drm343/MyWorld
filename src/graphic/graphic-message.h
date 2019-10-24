#ifndef HEADER_GRAPHIC_MESSAGE
#define HEADER_GRAPHIC_MESSAGE

#include "style.h"
#include "String.h"
#include "Morph-SubWindow.h"

/** @brief 啟動訊息欄
 * @param render 要顯示的 SDL_Renderer
 * @param font 要使用的字型
 * @return 訊息欄的 Access
 *
 * 未來會將 SDL_Renderer 改為 Morph_World
*/
Morph_SubWindow Message_Box_start(SDL_Renderer * render, TTF_Font * font);
#endif
