#ifndef HEADER_MORPH_SDL2
#define HEADER_MORPH_SDL2

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>


#include "Morph.h"


/** @brief 確認基本設定是否設定完成
*/
bool morph_sdl2_is_setup_success (void);


/** @brief 設定預設的 SDL_Renderer
 * @param new_render 新的 render 位置
 * @return 原來的 render 位置
*/
SDL_Renderer *morph_sdl2_change_render (SDL_Renderer * new_render);


/** @brief 回傳預設的 SDL_Renderer
 * @return SDL_Renderer
*/
SDL_Renderer *morph_sdl2_render (void);


/** @brief 設定 SDL2 使用的 Font
 * @param font 成功開啟的 TTF_Font
*/
void morph_sdl2_set_font (TTF_Font * font);


/** @brief 設定 SDL2 使用的 Font
 * @return 取出 TTF_Font 設定給 SDL2 based 的 Morph 使用
*/
TTF_Font *morph_sdl2_font (void);


/** @brief 自動建立 SDL2 使用的 Color
 * @param self Morph 物件
*/
void morph_sdl2_init_color (Morph self);


/** @brief 釋放顏色的記憶體
 * @param self Morph 物件
*/
void morph_sdl2_free_color (Morph self);

#endif
