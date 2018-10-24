#include "graphic-message.h"


/** @brief Namespace BOX
 */
#define EXPORT(name) BOX(name)

/** @brief 直接使用 SDL 內建結構，可以直接用到 SDL 中
*/
typedef SDL_Point *Message_Box_Point;

/** @brief 訊息欄
 *
 * 用來顯示訊息用，不限定實作方式，也不限定開發者的設計方式，可以直接
 * 跟主遊戲畫面合在一起，如同目前的做法，也可以拆開讓訊息欄在外部作為
 * 一個顯示器單獨存在或直接顯示在 terminal 也可。
*/
typedef struct Message_Box {
    Message_Box_Point box;      /**< 顯示器的設定，可以畫出一個長方形 */
    bpt_t history;              /**< 儲存要顯示的訊息 */
    int64_t counter;            /**< 提供給使用者抓出最新資訊用的計數器 */
    bool is_need_updated;            /**< 是否有更新 */
} Message_Box;
typedef Message_Box *Message_Box_Access;

static SDL_Point box_array[5] = {
    {
     0, 0},
    {
     0, 120},
    {
     799, 120},
    {
     799, 0},
    {
     0, 0}
};

//--------------------------------
//Control API
// --------------------------------
/** @brief 啟動訊息欄
 * @return 訊息欄的 Access
*/
Message_Box_Access EXPORT(start) (void) {
    Message_Box_Access self = calloc(1, sizeof(Message_Box));
    self->box = box_array;
    self->history = NULL;
    self->counter = 0;
    self->is_need_updated = true;
    return self;
}

/** @brief 釋放訊息欄
 * @param self 要釋放的訊息欄
*/
void EXPORT(stop) (Message_Box_Access self) {
    bpt_release(self->history);
    free(self);
}

//--------------------------------
//Graphic Box points
// --------------------------------
/** @brief 取出畫面用的 box 點座標
 * @param self 訊息欄
 * @return 點座標
*/
SDL_Point *EXPORT(box) (Message_Box_Access self) {
    return self->box;
}

/** @brief 設定畫面用的 box 點座標
 * @param self 訊息欄
 * @param start_x 起始點 x 位置
 * @param start_y 起始點 y 位置
 * @param width 訊息欄寬度
 * @param height 訊息欄高度
*/
void
EXPORT(set_box) (Message_Box_Access self,
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

//--------------------------------
//History
// --------------------------------
/** @brief 目前的歷史訊息總數
 * @param self 訊息欄
 * @return 歷史訊息總數
*/
bpt_key_t EXPORT(history_count) (Message_Box_Access self) {
    return self->counter;
}

/** @brief 根據 index 取出對應的歷史訊息
 * @param self 訊息欄
 * @param index 想取出的訊息數字
 * @return 歷史訊息
*/
ImmutableString EXPORT(get_history_by_index) (Message_Box_Access self,
                                              bpt_key_t index) {
    if (index >= 0) {
        if (bpt_has_key(self->history, index)) {
            ImmutableString result =
                (ImmutableString) bpt_get(self->history, index);
            return result;
        }
    }
    return NULL;
}

/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void EXPORT(add_message) (Message_Box_Access self, const char *message) {
    void auto_release(bpt_key_t key, void *value) {
        ImmutableString str = (ImmutableString) value;
        String_free(str);
    }

    bpt_t bpt_assoc_and_release(bpt_t bpt, bpt_key_t key, void *value) {
        bpt_t new_bpt = bpt_assoc(bpt, key, value);
        bpt_set_dealloc_hook(new_bpt, key, auto_release);
        bpt_release(bpt);
        return new_bpt;
    }

    ImmutableString result = String_create(message);
    if (self->counter == 0) {
        self->history = bpt_assoc(NULL, 0, result);
        bpt_set_dealloc_hook(self->history, 0, auto_release);
    } else {
        self->history =
            bpt_assoc_and_release(self->history, self->counter, result);
    }
    self->counter = self->counter + 1;
    self->is_need_updated = true;
}

//--------------------------------
// Update Status
// --------------------------------
/** @brief 檢查是否有更新
 * @param self 訊息欄
 * @return 是否有新訊息
*/
bool EXPORT(is_updated) (Message_Box_Access self) {
    return !(self->is_need_updated);
}

/** @brief 告訴 Message Box 更新已完成
 * @param self 訊息欄
*/
void EXPORT(update_done) (Message_Box_Access self) {
    self->is_need_updated = false;
}

#undef EXPORT
