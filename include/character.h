#ifndef HEADER_CHARACTER
#define HEADER_CHARACTER

#include "graphic.h"
#include "point-use_self.h"


/** @brief Namespace Character_Base_
 *
 * 當使用 EXPORT 的函數時，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如 init 必須寫成 Character_Base_init，如果外部程式要簡化呼叫，
 * 可以在程式中自行定義新的 macro，例如下面範例。
 *
 * \#define CHARA_BASE(name) Character_Base_#\#name<br>
 * CHARA_BASE(init)(self);
 */
#define EXPORT(name) Character_Base_##name


/** @brief 角色基本資料結構
 *
 * 可以考慮透過 C11 匿名結構的方式來新增自己需要的新資料。
 */
typedef struct Character_Base {
    const char *name; /**< 角色名稱 */
    Style_Access Mark; /**< 角色圖形的 Access */
    Point_Access Real_Position; /**< 角色的真實座標 */
    Point_Access Graph_Position; /**< 顯示在畫面上的座標 */
    Yes_No crossable; /**< 可不可以跨過去，後續會修改 */
    Yes_No attackable; /**< 可不可以被攻擊，後續會修改 */
    bool is_alive; /**< 角色是否還存活，可以依靠此資訊取代 crossable */
    Use_Type status; /**< 是否使用中，可以依靠 Character_Pool 來分辨，後續會移除 */
} Character_Base;


  /** @brief 初始化角色基本資料
   * @param self 要初始化的角色物件
   *
   * 請確保傳進去的物件不是 NULL
  */
void EXPORT(init) (Character_Base * self);


  /** @brief 釋放角色基本資料
   * @param self 要釋放的角色物件
   *
   * 請確保傳進去的物件不是 NULL。
  */
void EXPORT(free) (Character_Base * self);


  /** @brief 複製角色基本資料
   * @param self 目標
   * @param origin 要被複製的角色物件
   *
   * 請確保傳進去的物件不是 NULL。
   *
   * 由於 name、Mark 皆為不會變動的指標，因此可以放心複製。
  */
void EXPORT(copy) (Character_Base * self, Character_Base * origin);

#undef EXPORT
#endif
