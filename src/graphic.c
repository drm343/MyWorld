#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "graphic.h"
#include "helper/generic_function.h"
#include "helper/debug.h"


/** @brief Namespace Style_Pool_
 *
 * 當使用 EXPORT 的函數時，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如 start 必須寫成 Style_Pool_start，如果外部程式要簡化呼叫，
 * 可以在程式中自行定義新的 macro，例如下面範例。
 *
 * #define SP(name) Style_Pool_##name<br>
 * Style_Pool_Access pool = SP(start)(20);
 */
#define EXPORT(name) Style_Pool_##name


  /** @brief 建立 Style_Pool 物件
   * @param size 要建立的 Style 數量
  */
Style_Pool_Access EXPORT(start) (int size) {
    Style_Access style_memory = calloc(size, sizeof(Style));
    Style_Pool_Access self = calloc(1, sizeof(Style_Pool));

    self->pool = style_memory;
    self->max_size = size;
    self->current_size = size;
    return self;
}


  /** @brief 要釋放的 Style_Pool 物件
   * @param self Style_Pool 物件 Access
   *
   * 本函數不會釋放 Style 的 name 跟 mark，預設 String 全部透過外部 String 池或類似工具管理，
   * 若需要釋放，請透過外部管理函數釋放 name 跟 mark。
  */
void EXPORT(stop) (Style_Pool_Access self) {
    free(self->pool);
    free(self);
}


  /** @brief 將 Style_Pool 中所有的 SDL_Texture 釋放
   * @param self Style_Pool 物件 Access
   *
   * 因為 SDL_init 執行的時間比 Style_Pool 建立的時間晚，因此會優先結束 SDL，但是在結束前必須
   * 先釋放掉 SDL_Texture，所以不能將這個步驟整合進 GRAPHIC_stop 內。
   * 如果整合進去，則啟動跟結束的時機必須在 SDL 結束前，為了確保不會因為釋放而發生記憶體錯誤，必
   * 須修改程式中建立跟釋放 Style_Pool 的時機。
  */
void EXPORT(free_texture) (Style_Pool_Access self) {
    uint8_t count = 0;
    uint8_t used = self->max_size - self->current_size;
    Style_Access result = NULL;

    for (count; count < used; count++) {
        result = &(self->pool[count]);

        SDL_DestroyTexture(result->access);
        result->access = NULL;
    }
}


  /** @brief 從 Style_Pool 中分配出一個 Style 物件
   * @param self Style_Pool 物件 Access
   * @return Style 物件的 Access
  */
Style_Access EXPORT(malloc) (Style_Pool_Access self) {
    int8_t start = self->max_size - self->current_size;
    Style_Access result = &(self->pool[start]);

    self->current_size -= 1;
    return result;
}


  /** @brief 從 Style_Pool 中找出特定名稱的 Style 物件
   * @param self Style_Pool 物件 Access
   * @param name 要尋找的名稱
   * @return Style 物件的 Access，沒找到則會回傳 NULL
  */
Style_Access EXPORT(find) (Style_Pool_Access pool_access,
                                const char *name) {
    uint8_t count = 0;
    uint8_t used = pool_access->max_size - pool_access->current_size;
    Style_Access result = NULL;

    for (count; count < used; count++) {
        result = &(pool_access->pool[count]);

        if (result->name == name) {
            return result;
        }
    }
    result = NULL;

    return result;
}


  /** @brief 從 Style_Pool 中拿出下一個已建立的 Style 物件
   * @param self Style_Pool 物件 Access
   * @param current_counter 當前記數器，必須傳 Access
   * @return Style 物件的 Access，沒找到則會回傳 NULL
   *
   * 請不要從外部手動修改 current_counter，後續可自行撰寫 macro 簡化成 foreach-like 型式。
  */
Style_Access EXPORT(next) (Style_Pool_Access self,
                                uint8_t * current_counter) {
    uint8_t counter = *current_counter;
    uint8_t used = self->max_size - self->current_size;
    Style_Access result = NULL;

    if (counter < used) {
        result = &(self->pool[counter]);
        *current_counter += 1;
    }

    return result;
}


  /** @brief 顯示 Style_Pool 中的資料
   * @param self Style_Pool 物件 Access
   *
   * 請開啟 DEBUG 參數來讓本函數可以運作。
  */
void EXPORT(debug)(Style_Pool_Access self)
{
#ifdef DEBUG
    uint8_t count = 0;
    uint8_t used = self->max_size - self->current_size;
    Style_Access result = NULL;

    DEBUG_PRINT("style is null? %s\n", BOOL_STRING(result));
    for (count; count < used; count++) {
        result = &(self->pool[count]);
        DEBUG_PRINT("%s : %s\n", result->name, result->mark);
    }
#endif
}

#undef EXPORT
