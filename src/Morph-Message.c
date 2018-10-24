#include "Morph-Message.h"


typedef struct Morph_Message {
    Morph super;
    ImmutableString string;
    SDL_Texture *texture;
    SDL_Renderer *render;
} *Morph_Message;


/** @brief 建立 Morph_Message
 * @return Morph Message
*/
Morph_Message M_MESSAGE(create) (SDL_Renderer * render) {
    Morph_Message self = NEW(Morph_Message);
    self->super = MORPH(create) ();
    self->render = render;
    return self;
}


/** @brief 刪除 Morph_Message
 * @param self Morph Message 物件
*/
void M_MESSAGE(free) (Morph_Message self) {
    MORPH(free) (self->super);
    String_free(self->string);
    SDL_DestroyTexture(self->texture);
    free(self);
}


/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void M_MESSAGE(add_message) (Morph_Message self, const char *message) {
    TTF_Font *USE_FONT = M_SDL2(font) ();
    self->string = String_create(message);
    MORPH(set_extent) (self->super, self->string->width_length, 24);

    Point position = MORPH(position) (self->super);
    Point extent = MORPH(extent) (self->super);

    SDL_Rect box = {
        .x = Point_x(position),
        .y = Point_y(position),
        .w = Point_x(extent),
        .h = Point_y(extent)
    };
    SDL_Color *color = MORPH(color) (self->super);

    SDL_Surface *surface = TTF_RenderUTF8_Solid(USE_FONT, message, *color);
    self->texture = SDL_CreateTextureFromSurface(self->render, surface);
    SDL_FreeSurface(surface);
}
