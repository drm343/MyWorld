#include "graphic-message.h"

//--------------------------------
// Control API
//--------------------------------
/** @brief 啟動訊息欄
 * @param render 要顯示的 SDL_Renderer
 * @param font 要使用的字型
 * @return 訊息欄的 Access
 *
 * 未來會將 SDL_Renderer 改為 Morph_World
*/
Morph_SubWindow Message_Box_start(SDL_Renderer * render, TTF_Font * font)
{
    M_SDL2(change_render) (render);
    M_SDL2(set_font) (font);
    Morph_SubWindow self = SUBWINDOW(create) ();
    Morph_Message message = NULL;

    self->set_position(self, 0, 20 * 24);
    self->set_extent(self, 799, 120);
    SUBWINDOW(set_box) (self);

    for (int counter = 0; counter < 5; counter++) {
        char name[10];
        sprintf(name, "%d", counter);
        message = M_MESSAGE(create) ();
        self->add(self, message);
        message->set_name(message, name);
    }
    return self;
}
