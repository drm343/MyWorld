#ifndef HEADER_GRAPHIC_MESSAGE
#define HEADER_GRAPHIC_MESSAGE


#include "namespace.h"

#include "style.h"
#include "String.h"
#include "Morph-SubWindow.h"

/** @brief namespace for Message_Box
*/
#define BOX(name) Message_Box_##name

/** @brief 啟動訊息欄
 * @param render 要顯示的 SDL_Renderer
 * @return 訊息欄的 Access
 *
 * 未來會將 SDL_Renderer 改為 Morph_World
*/
Morph_SubWindow BOX(start) (SDL_Renderer * render, TTF_Font * font);
#endif
