#ifndef HEADER_GRAPHIC
#define HEADER_GRAPHIC

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include "base/type.h"
#include "namespace.h"

/** @brief Namespace STYLE_P
 */
#define EXPORT(name) STYLE_P(name)

typedef SDL_Texture *SDL_Texture_Access;

/** @brief 字型設定
 */
typedef struct {
    SDL_Color *color;           /**< 使用的顏色 */
    TTF_Font *font;             /**< 使用的字型 */
    int8_t width;               /**< 字的寬度 */
    int8_t height;              /**< 字的高度 */
} FontConfig;

/** @brief 顯示在螢幕上的圖形結構
 */
typedef struct {
    const char *name;           /**< 比對用的名稱，通常為種族名稱 */
    const char *mark;           /**< 顯示在畫面上的代表圖形 */
    Yes_No crossable;           /**< 可不可以跨過去 */
    Yes_No attackable;          /**< 可不可以被攻擊 */
    SDL_Texture_Access access;  /**< SDL_Texture 的 Access */
} Style;
typedef Style *Style_Access;

/** @brief 圖形池，用來儲存可以顯在畫面上的圖形
 */
typedef struct {
    Style_Access pool;          /**< 可分配出去的 Style 儲存位置 */
    uint8_t max_size;           /**< 可使用的最大值 */
    uint8_t current_size;               /**< 當前使用量 */
} Style_Pool;
typedef Style_Pool *Style_Pool_Access;

/** @brief 建立圖形池
 * @param size 圖形池可以保存的的 Style 最大數量
*/
Style_Pool_Access EXPORT(start) (int size);

/** @brief 釋放圖形池
 * @param self 要釋放的圖形池
 *
 * 本函數不會釋放 Style 的 name 跟 mark，預設 String 全部透過外部 String 池或類似工具管理，
 * 若需要釋放，請透過外部管理函數釋放 name 跟 mark。
*/
void EXPORT(stop) (Style_Pool_Access self);

/** @brief 將圖形池中所有的材質貼圖釋放
 * @param self 要使用的圖形池
 *
 * 因為 SDL_init 執行的時間比圖形池建立的時間晚，因此會優先結束 SDL，但是在結束前必須
 * 先釋放掉 SDL_Texture，所以不能將這個步驟整合進 GRAPHIC_stop 內。
 * 如果整合進去，則啟動跟結束的時機必須在 SDL 結束前，為了確保不會因為釋放而發生記憶體錯誤，必
 * 須修改程式中建立跟釋放 Style_Pool 的時機。
*/
void EXPORT(free_texture) (Style_Pool_Access self);

/** @brief 從圖形池中分配出一個 Style 物件
 * @param self 要使用的圖形池
 * @return Style 物件的 Access
*/
Style_Access EXPORT(malloc) (Style_Pool_Access self);

/** @brief 從圖形池中找出特定名稱的 Style 物件
 * @param self 要使用的圖形池
 * @param name 要尋找的名稱
 * @return Style 物件的 Access，沒找到則會回傳 NULL
*/
Style_Access EXPORT(find) (Style_Pool_Access self, const char *name);

/** @brief 從圖形池中拿出下一個已建立的 Style 物件
 * @param self 要使用的圖形池
 * @param current_counter 當前記數器，必須傳 Access
 * @return Style 物件的 Access，沒找到則會回傳 NULL
 *
 * 請不要從外部手動修改 current_counter，後續可自行撰寫 macro 簡化成 foreach-like 型式。
*/
Style_Access EXPORT(next) (Style_Pool_Access self, uint8_t * current_counter);

/** @brief 顯示圖形池中的資料
 * @param self 要使用的圖形池
 *
 * 請開啟 DEBUG 參數來讓本函數可以運作。
*/
void EXPORT(debug) (Style_Pool_Access self);

#undef EXPORT
#endif
