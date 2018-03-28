#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "style.h"
#include "helper/generic_function.h"
#include "helper/debug.h"

/** @brief Namespace STYLE_P
 */
#define EXPORT(name) STYLE_P(name)

/** @brief 建立圖形池
 * @param size 圖形池可以保存的的 Style 最大數量
*/
Style_Pool_Access EXPORT(start) (int size) {
    Style_Access style_memory = calloc(size, sizeof(Style));
    Style_Pool_Access self = calloc(1, sizeof(Style_Pool));

    self->pool = style_memory;
    self->max_size = size;
    self->current_size = size;
    return self;
}

/** @brief 釋放圖形池
 * @param self 要釋放的圖形池
 *
 * 本函數不會釋放 Style 的 name 跟 mark，預設 String 全部透過外部 String 池或類似工具管理，
 * 若需要釋放，請透過外部管理函數釋放 name 跟 mark。
*/
void EXPORT(stop) (Style_Pool_Access self) {
    uint8_t counter = 0;
    Style_Access item = NULL;
    while (item = EXPORT(next)(self, &counter)) {
        [item->name dealloc];
        [item->mark dealloc];
    }
    free(self->pool);
    free(self);
}

/** @brief 將圖形池中所有的材質貼圖釋放
 * @param self 要使用的圖形池
 *
 * 因為 SDL_init 執行的時間比圖形池建立的時間晚，因此會優先結束 SDL，但是在結束前必須
 * 先釋放掉 SDL_Texture，所以不能將這個步驟整合進 GRAPHIC_stop 內。
 * 如果整合進去，則啟動跟結束的時機必須在 SDL 結束前，為了確保不會因為釋放而發生記憶體錯誤，必
 * 須修改程式中建立跟釋放 Style_Pool 的時機。
*/
void EXPORT(free_texture) (Style_Pool_Access self) {
    uint8_t used = self->max_size - self->current_size;
    Style_Access result = NULL;

    for (uint8_t count = 0; count < used; count++) {
        result = &(self->pool[count]);

        SDL_DestroyTexture(result->access);
        result->access = NULL;
    }
}

/** @brief 從圖形池中分配出一個 Style 物件
 * @param self 要使用的圖形池
 * @return Style 物件的 Access
*/
Style_Access EXPORT(malloc) (Style_Pool_Access self) {
    int8_t start = self->max_size - self->current_size;
    Style_Access result = &(self->pool[start]);

    self->current_size -= 1;
    return result;
}

/** @brief 從圖形池中找出特定名稱的 Style 物件
 * @param self 要使用的圖形池
 * @param name 要尋找的名稱
 * @return Style 物件的 Access，沒找到則會回傳 NULL
*/
Style_Access EXPORT(find) (Style_Pool_Access pool_access, const char *name) {
    uint8_t used = pool_access->max_size - pool_access->current_size;
    Style_Access result = NULL;
    MWMutableString *find_name = [MWMutableString create_with_c_string: name];

    for (uint8_t count = 0; count < used; count++) {
        result = &(pool_access->pool[count]);

        if ([result->name equal: find_name]) {
            goto DONE;
        }
    }
    result = NULL;

DONE:
    [find_name dealloc];
    return result;
}

/** @brief 從圖形池中拿出下一個已建立的 Style 物件
 * @param self 要使用的圖形池
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

/** @brief 顯示圖形池中的資料
 * @param self 要使用的圖形池
 *
 * 請開啟 DEBUG 參數來讓本函數可以運作。
*/
void EXPORT(debug) (Style_Pool_Access self) {
#ifdef DEBUG
    uint8_t used = self->max_size - self->current_size;
    Style_Access result = NULL;

    DEBUG_PRINT("style is null? %s\n", BOOL_STRING(result));
    for (uint8_t count = 0; count < used; count++) {
        result = &(self->pool[count]);
        DEBUG_PRINT("%s : %s : %s\n", result->name, result->mark,
                    BOOL_STRING(result->access != NULL));
    }
#endif
}

#undef EXPORT
