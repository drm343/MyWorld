#include "Morph-SDL2.h"


static TTF_Font *USE_FONT = NULL;
static SDL_Renderer *render = NULL;


/** @brief 確認基本設定是否設定完成
*/
bool M_SDL2(is_setup_success) (void) {
    return render != NULL && USE_FONT != NULL;
}


/** @brief 設定預設的 SDL_Renderer
 * @param new_render 新的 render 位置
 * @return 原來的 render 位置
*/
SDL_Renderer *M_SDL2(change_render) (SDL_Renderer * new_render) {
    SDL_Renderer *old_render = render;
    render = new_render;
    return old_render;
}


/** @brief 回傳預設的 SDL_Renderer
 * @return SDL_Renderer
*/
SDL_Renderer *M_SDL2(render) (void) {
    return render;
}


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

/** @brief 自動建立 SDL2 使用的 Color
 * @param self Morph 物件
*/
void M_SDL2(init_color) (Morph self) {
    SDL_Color *color = calloc(1, sizeof(SDL_Color));
    color->r = 255;
    color->r = 255;
    color->b = 255;
    color->a = 0;
    self->set_color(self, color);
}


/** @brief 釋放顏色的記憶體
 * @param self Morph 物件
*/
void M_SDL2(free_color) (Morph self) {
    SDL_Color *color = self->color(self);
    free(color);
}
