#include "Morph-Message.h"

static Class CLASS_ID = NULL;


typedef struct Custom_Property {
    ImmutableString string;
    SDL_Texture *texture;
    SDL_Renderer *render;

    /** @brief 呼叫 Morph 的上一層 free 函數
     * @param self Morph 物件
     */
    void (*super_free)(Morph self);
} *Custom_Property;


/** @brief 刪除 Morph_Message
 * @param self Morph Message 物件
*/
static void morph_msg_free (Morph_Message self) {
    if (self != NULL) {
        Custom_Property property = self->property;

        if (property->string != NULL) {
            String_free(property->string);
        }
        SDL_DestroyTexture(property->texture);
        void (*super_free)(Morph) = property->super_free;
        super_free(self);
        free(property);
    }
}


/** @brief 建立 Custom_Property
 * @param self Morph
 * @param message 要儲存的訊息
 * @return Morph Message
*/
static Custom_Property morph_msg_new_custom_property (Morph_Message self,
                                                       const char *message)
{
    TTF_Font * USE_FONT = morph_sdl2_font ();

    Custom_Property origin_property = self->property;
    Custom_Property property = NEW(Custom_Property);

    property->super_free = origin_property->super_free;
    property->render = origin_property->render;
    property->string = String_create(message);

    SDL_Color *color = self->color(self);
    SDL_Surface *surface = TTF_RenderUTF8_Solid(USE_FONT, message, *color);
    property->texture =
        SDL_CreateTextureFromSurface(property->render, surface);
    SDL_FreeSurface(surface);

    self->property = property;
    self->set_extent(self, property->string->width_length, 24);
    return origin_property;
}


/** @brief 更改 Custom_Property
 * @param self Morph
 * @param message 要儲存的訊息
*/
static void morph_msg_reset_custom_property (Morph_Message self,
                                              const char *message) {
    TTF_Font * USE_FONT = morph_sdl2_font ();
    Custom_Property property = self->property;

    String_free(property->string);
    property->string = String_create(message);

    SDL_DestroyTexture(property->texture);
    self->set_extent(self, property->string->width_length, 24);

    SDL_Color *color = self->color(self);

    SDL_Surface *surface = TTF_RenderUTF8_Solid(USE_FONT, message, *color);
    property->texture =
        SDL_CreateTextureFromSurface(property->render, surface);
    SDL_FreeSurface(surface);
}


/** @brief 釋放 Custom_Property
 * @param property
*/
static void morph_msg_free_custom_property (Custom_Property property) {
    if (property->string != NULL) {
        String_free(property->string);
    }
    if (property->texture != NULL) {
        SDL_DestroyTexture(property->texture);
    }
}


/** @brief 修改 property
 * @param self 訊息欄
 * @param property 想修改的 property
*/
static void morph_msg_update_property (Morph_Message self,
                                        Custom_Property property) {
    Custom_Property origin_property = self->property;
    self->property = property;
    if (self->property->string != NULL) {
        self->set_extent(self, property->string->width_length, 24);
    } else {
        self->set_extent(self, 0, 24);
    }

    if (self->morph->submorph != NULL) {
        if (morph_msg_is_morph_message (self->morph->submorph) == true) {
            morph_msg_update_property (self->morph->submorph,
                                        origin_property);
            return;
        }
    }
    morph_msg_free_custom_property (origin_property);
}


/** @brief 在螢幕上畫出 Morph 物件
 * @param self Morph 物件
 */
static void morph_msg_draw (Morph_Message self) {
    Point position = self->position(self);
    Point extent = self->extent(self);

    SDL_Renderer *render = self->property->render;
    SDL_Texture *texture = self->property->texture;
    SDL_Rect box = {
        .x = position->x,
        .y = position->y,
        .w = extent->x,
        .h = extent->y
    };

    if (texture != NULL) {
        SDL_RenderCopy(render, texture, NULL, &box);
    }

    if (self->morph->submorph != NULL) {
        Morph submorph = self->submorph(self);
        submorph->draw(submorph);
    }
}


/** @brief 建立 Morph_Message
 * @return Morph Message
*/
Morph_Message morph_msg_create (void) {
    if (morph_sdl2_is_setup_success () == false) {
        DEBUG_MESSAGE("This line will not appear\n");
        return NULL;
    }

    if (CLASS_ID == NULL) {
        CLASS_ID = NEW_CLASS_ID();
    }
    else {
        CLASS_ID->counter = CLASS_ID->counter + 1;
    }

    Morph_Message self =
        morph_create (morph_sdl2_init_color, morph_sdl2_free_color);
    self->class = CLASS_ID;
    Custom_Property property = NEW(Custom_Property);
    self->property = property;
    self->property->render = morph_sdl2_render ();
    self->property->string = NULL;
    self->property->texture = NULL;
    self->property->super_free = self->free;
    self->free = morph_msg_free;
    self->draw = morph_msg_draw;
    return self;
}


/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void morph_msg_add_message(Morph_Message self, const char *message) {
    if (self->morph->submorph != NULL) {
        if (morph_msg_is_morph_message (self->morph->submorph) == true) {
            Custom_Property property =
                morph_msg_new_custom_property (self,
                        message);
            morph_msg_update_property (self->morph->submorph, property);
            return;
        }
    }
    morph_msg_reset_custom_property(self, message);
}


/** @brief 確認是不是訊息顯示欄
 * @param self 訊息顯示欄
*/
bool morph_msg_is_morph_message (Morph_Message self) {
    return CHECK_CLASS(self->class, CLASS_ID);
}
