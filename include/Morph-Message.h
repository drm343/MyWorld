#ifndef HEADER_MORPH_MESSAGE_CLASS
#define HEADER_MORPH_MESSAGE_CLASS

#include "Morph-SDL2.h"


#define M_MESSAGE(name) Morph_Message_##name


typedef struct Morph_Message *Morph_Message;


/** @brief 建立 Morph_Message
 * @return Morph Message
*/
Morph_Message M_MESSAGE(create) (SDL_Renderer * render);


/** @brief 刪除 Morph_Message
 * @param self Morph Message 物件
*/
void M_MESSAGE(free) (Morph_Message self);


/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void M_MESSAGE(add_message) (Morph_Message self, const char *message);

#endif
