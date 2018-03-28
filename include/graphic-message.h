#ifndef HEADER_GRAPHIC_MESSAGE
#define HEADER_GRAPHIC_MESSAGE


#include "bitmapped_patricia_tree.h"

#include "namespace.h"

#include "style.h"
#include "helper/strings.h"

/** @brief Namespace BOX
 */
#define EXPORT(name) BOX(name)

/** @brief 訊息欄結構
 *
 * 用來顯示訊息用，不限定實作方式，也不限定開發者的設計方式，可以直接
 * 跟主遊戲畫面合在一起，如同目前的做法，也可以拆開讓訊息欄在外部作為
 * 一個顯示器單獨存在或直接顯示在 terminal 也可。
*/
typedef struct Message_Box Message_Box;

/** @brief 訊息欄物件
 *
 * 請參閱訊息欄結構。
*/
typedef Message_Box *Message_Box_Access;

/** @brief 啟動訊息欄
 * @return 訊息欄的 Access
*/
Message_Box_Access EXPORT(start) (void);

/** @brief 釋放訊息欄
 * @param self 要釋放的訊息欄
*/
void EXPORT(stop) (Message_Box_Access self);

/** @brief 取出畫面用的 box 點座標
 * @param self 訊息欄
 * @return 點座標
*/
SDL_Point *EXPORT(box) (Message_Box_Access self);

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
                 int64_t width, int64_t height);

/** @brief 新增訊息到訊息欄
 * @param self 訊息欄
 * @param message 想加入的訊息
*/
void EXPORT(add_message) (Message_Box_Access self, const char *message);

/** @brief 目前的歷史訊息總數
 * @param self 訊息欄
 * @return 歷史訊息總數
*/
bpt_key_t EXPORT(history_count) (Message_Box_Access self);

/** @brief 根據 index 取出對應的歷史訊息
 * @param self 訊息欄
 * @param index 想取出的訊息數字
 * @return 歷史訊息
*/
const char *EXPORT(get_history_by_index) (Message_Box_Access self,
                                          bpt_key_t index);

#undef EXPORT
#endif
