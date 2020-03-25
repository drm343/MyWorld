#ifndef HEADER_MORPH_MESSAGE_CLASS
#define HEADER_MORPH_MESSAGE_CLASS

#include "Morph-SDL2.h"


typedef Morph Morph_Message;


/** @brief 建立 Morph_Message
 * @return Morph Message
*/
Morph_Message morph_msg_create (void);


/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void morph_msg_add_message (Morph_Message self, const char *message);


/** @brief 確認是不是訊息顯示欄
 * @param self 訊息顯示欄
*/
bool morph_msg_is_morph_message (Morph_Message self);

#endif
