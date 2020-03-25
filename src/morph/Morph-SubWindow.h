#ifndef HEADER_MORPH_morph_sw__CLASS
#define HEADER_MORPH_morph_sw__CLASS

#include "Morph-Message.h"


typedef Morph Morph_SubWindow;


/** @brief 建立 Morph_SubWindow
 * @return Morph sub window
*/
Morph_SubWindow morph_sw_create (void);


/** @brief 建立 Morph_SubWindow
 * @param self Morph sub window
 * @param size 可以顯示的最大訊息數量，預設值為 5
*/
void morph_sw_set_max_message_size (Morph_SubWindow self, int8_t size);


/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void morph_sw_add_message (Morph_SubWindow self, const char *message);


/** @brief 設定畫面用的 box 點座標
 * @param self 訊息欄
*/
void morph_sw_set_box (Morph_SubWindow self);

#endif
