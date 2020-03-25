#include "Morph-SubWindow.h"
#include "debug.h"


static Class CLASS_ID = NULL;


//--------------------------------
// Graphic Box points
//--------------------------------

static SDL_Point box_array[5] = {
    {
     0, 0 },
    {
     0, 120 },
    {
     799, 120 },
    {
     799, 0 },
    {
     0, 0 }
};

/** @brief 設定畫面用的 box 點座標
 * @param self 訊息欄
*/
void morph_sw_set_box (Morph_SubWindow self) {
    Point position = self->position(self);
    Point extent = self->extent(self);

    box_array[0].x = position->x;
    box_array[0].y = position->y;
    box_array[1].x = position->x;
    box_array[1].y = position->y + extent->y;
    box_array[2].x = position->x + extent->x;
    box_array[2].y = position->y + extent->y;
    box_array[3].x = position->x + extent->x;
    box_array[3].y = position->y;
    box_array[4].x = position->x;
    box_array[4].y = position->y;
}


typedef struct Custom_Property {
    int8_t max_counter;
    int8_t current_counter;
    SDL_Renderer *render;
    Morph last;

    void (*super_add)(Morph self, Morph other);
} *Custom_Property;


/** @brief 建立 Morph_SubWindow
 * @return Morph sub window
*/
static void morph_sw_add (Morph self, Morph other) {
    if (self->property->current_counter <= 0) {
        self->property->last = other;
        other->set_position(other, 0,
                            self->morph->position->y +
                            self->property->current_counter * 24);
        other->set_extent(other, 0, 24);
        self->property->super_add(self, other);
        self->property->current_counter = 1;
    } else if (self->property->current_counter <
               self->property->max_counter) {
        Morph last = self->property->last;
        self->property->super_add(last, other);
        other->set_position(other, 0,
                            self->morph->position->y +
                            self->property->current_counter * 24);
        other->set_extent(other, 0, 24);
        self->property->current_counter =
            self->property->current_counter + 1;
        self->property->last = other;
    }
}


/** @brief 在螢幕上畫出 Morph 物件
 * @param self Morph 物件
 */
static void morph_sw_draw (Morph_SubWindow self) {
    SDL_Color *color = self->color(self);
    SDL_Renderer *render = self->property->render;

    Point position = self->position(self);
    Point extent = self->extent(self);

    SDL_SetRenderDrawColor(render, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_Rect clean_area = {
        .x = position->x,
        .y = position->y,
        .w = extent->x,
        .h = extent->y
    };
    SDL_RenderFillRect(render, &clean_area);

    SDL_SetRenderDrawColor(render, color->r, color->g, color->b, 0);
    SDL_Point *points = box_array;
    SDL_RenderDrawLines(render, points, 5);

    if (self->morph->submorph != NULL) {
        Morph submorph = self->submorph(self);
        submorph->draw(submorph);
    }
}


/** @brief 建立 Morph_SubWindow
 * @return Morph sub window
*/
Morph_SubWindow morph_sw_create (void) {
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

    Morph_SubWindow self = morph_create (morph_sdl2_init_color, morph_sdl2_free_color);
    self->class = CLASS_ID;
    self->property = NEW(Custom_Property);
    self->property->max_counter = 5;
    self->property->current_counter = 0;
    self->property->render = morph_sdl2_render ();
    self->property->super_add = self->add;
    self->add = morph_sw_add;
    self->property->last = NULL;
    self->draw = morph_sw_draw;
    return self;
}


/** @brief 建立 Morph_SubWindow
 * @param self Morph sub window
 * @param size 可以顯示的最大訊息數量，預設值為 5
*/
void morph_sw_set_max_message_size(Morph_SubWindow self, int8_t size) {
   self->property->max_counter = size;
}


/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void morph_sw_add_message(Morph_SubWindow self, const char *message) {
    if (self->morph->submorph != NULL) {
        if (morph_msg_is_morph_message(self->morph->submorph) == true) {
            morph_msg_add_message(self->morph->submorph, message);
        }
    }
}
