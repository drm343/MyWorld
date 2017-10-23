#include "graphic-message.h"


  /** @brief 直接使用 SDL 內建結構，可以直接用到 SDL 中
  */
typedef SDL_Point *Message_Box_Point;


  /** @brief 訊息欄
   *
   * 用來顯示訊息用，不限定實作方式，也不限定開發者的設計方式，可以直接
   * 跟主遊戲畫面合在一起，如同目前的做法，也可以拆開讓訊息欄在外部作為
   * 一個顯示器單獨存在或直接顯示在 terminal 也可。
   *
   * @warning 目前因為 histroy 的設計方式只會顯示一份，如果有相同文字
   * 會顯示不出來，必須修改成 array 儲存數字的方式。
  */
typedef struct Message_Box {
    Message_Box_Point box; /**< 顯示器的設定，可以畫出一個長方形 */
    String_Intern history; /**< String Intern，可以確保相同文字只會有一份 */
} Message_Box;
typedef Message_Box *Message_Box_Access;


static SDL_Point box_array[5] = {
    {0, 20 * 24},
    {0, 600},
    {799, 600},
    {799, 20 * 24},
    {0, 20 * 24}
};


// --------------------------------
// Control API
// --------------------------------
  /** @brief 啟動訊息欄
   * @return 訊息欄的 Access
  */
Message_Box_Access BOX(start) (void) {
    Message_Box_Access self = calloc(1, sizeof(Message_Box));
    self->box = box_array;
    self->history = strings_new();
    return self;
}


  /** @brief 釋放訊息欄
   * @param self 要釋放的訊息欄
  */
void BOX(stop) (Message_Box_Access self) {
    strings_free(self->history);
    free(self);
}


// --------------------------------
// Graphic Box points
// --------------------------------
  /** @brief 取出畫面用的 box 點座標
   * @param self 訊息欄
   * @return 點座標
  */
SDL_Point *BOX(box) (Message_Box_Access self) {
    return self->box;
}


  /** @brief 設定畫面用的 box 點座標
   * @param self 訊息欄
   * @param start_x 起始點 x 位置
   * @param start_y 起始點 y 位置
   * @param width 訊息欄寬度
   * @param height 訊息欄高度
  */
void BOX(set_box) (Message_Box_Access self,
                   int64_t start_x, int64_t start_y,
                   int64_t width, int64_t height) {
    box_array[0].x = start_x;
    box_array[0].y = start_y;
    box_array[4].x = start_x;
    box_array[4].y = start_y;
    box_array[1].x = start_x;
    box_array[1].y = start_y + height;
    box_array[2].x = start_x + width;
    box_array[2].y = start_y + height;
    box_array[3].x = start_x + width;
    box_array[3].y = start_y;
}


// --------------------------------
// History
// --------------------------------
  /** @brief 目前的歷史訊息總數
   * @param self 訊息欄
   * @return 歷史訊息總數
  */
int BOX(history_count) (Message_Box_Access self) {
    return strings_count(self->history);
}


  /** @brief 根據 index 取出對應的歷史訊息
   * @param self 訊息欄
   * @param index 想取出的訊息數字
   * @return 歷史訊息
  */
const char *BOX(get_history_by_index) (Message_Box_Access self, int index) {
    return strings_lookup_id(self->history, index);
}


  /** @brief 新增訊息到訊息欄
   * @param self 訊息欄
   * @param message 想加入的訊息
  */
void BOX(add_message) (Message_Box_Access self, char *message) {
    strings_intern(self->history, message);
}
