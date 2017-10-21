#ifndef HEADER_GRAPHIC
#define HEADER_GRAPHIC

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>

#include "base_type.h"

typedef SDL_Texture *SDL_Texture_Access;


/** @brief 字型設定
 */
typedef struct {
    SDL_Color *color; /**< 使用的顏色 */
    TTF_Font *font; /**< 使用的字型 */
    int8_t width; /**< 字的寬度 */
    int8_t height; /**< 字的高度 */
} FontConfig;


/** @brief 顯示在螢幕上的圖形結構
 */
typedef struct {
    const char *name; /**< 比對用的名稱，通常為種族名稱 */
    const char *mark; /**< 顯示在畫面上的代表圖形 */
    SDL_Texture_Access access; /**< SDL_Texture 的 Access */
} Style;
typedef Style *Style_Access;


/** @brief 圖形池，用來儲存可以顯在畫面上的圖形
 */
typedef struct {
    Style_Access pool; /**< 可分配出去的 Style 儲存位置 */
    uint8_t max_size; /**< 可使用的最大值 */
    uint8_t current_size; /**< 當前使用量 */
} Style_Pool;
typedef Style_Pool *Style_Pool_Access;


/** @brief Namespace Style_Pool_
 *
 * 當使用 EXPORT 的函數時，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如 start 必須寫成 Style_Pool_start，如果外部程式要簡化呼叫，
 * 可以在程式中自行定義新的 macro，例如下面範例。
 *
 * \#define SP(name) Style_Pool_#\#name<br>
 * Style_Pool_Access pool = SP(start)(20);
 */
#define EXPORT(name) Style_Pool_##name


  /** @brief 建立 Style_Pool 物件
   * @param size 要建立的 Style 數量
  */
Style_Pool_Access EXPORT(start) (int size);


  /** @brief 要釋放的 Style_Pool 物件
   * @param self Style 物件 Access
  */
void EXPORT(stop) (Style_Pool_Access self);


  /** @brief 將 Style_Pool 中所有的 SDL_Texture 釋放
   * @param self Style_Pool 物件 Access
   *
   * 因為 SDL_init 執行的時間比 Style_Pool 建立的時間晚，因此會優先結束 SDL，但是在結束前必須
   * 先釋放掉 SDL_Texture，所以不能將這個步驟整合進 GRAPHIC_stop 內。
   * 如果整合進去，則啟動跟結束的時機必須在 SDL 結束前，為了確保不會因為釋放而發生記憶體錯誤，必
   * 須修改程式中建立跟釋放 Style_Pool 的時機。
  */
void EXPORT(free_texture) (Style_Pool_Access self);


  /** @brief 從 Style_Pool 中分配出一個 Style 物件
   * @param self Style_Pool 物件 Access
   * @return Style 物件的 Access
  */
Style_Access EXPORT(malloc) (Style_Pool_Access self);


  /** @brief 從 Style_Pool 中找出特定名稱的 Style 物件
   * @param self Style_Pool 物件 Access
   * @param name 要尋找的名稱
   * @return Style 物件的 Access，沒找到則會回傳 NULL
  */
Style_Access EXPORT(find) (Style_Pool_Access self, const char *name);


  /** @brief 從 Style_Pool 中拿出下一個已建立的 Style 物件
   * @param self Style_Pool 物件 Access
   * @param current_counter 當前記數器，必須傳 Access
   * @return Style 物件的 Access，沒找到則會回傳 NULL
   *
   * 請不要從外部手動修改 current_counter，後續可自行撰寫 macro 簡化成 foreach-like 型式。
  */
Style_Access EXPORT(next) (Style_Pool_Access self,
                           uint8_t * current_counter);


  /** @brief 顯示 Style_Pool 中的資料
   * @param self Style_Pool 物件 Access
   *
   * 請開啟 DEBUG 參數來讓本函數可以運作。
  */
void EXPORT(debug) (Style_Pool_Access self);


#undef EXPORT
#endif
