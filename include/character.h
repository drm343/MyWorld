#ifndef HEADER_CHARACTER
#define HEADER_CHARACTER


#include "namespace.h"

#include "graphic.h"
#include "point-use_self.h"


/** @brief Namespace C_BASE
 */
#define EXPORT(name) C_BASE(name)


/** @brief 角色生存狀態
 *
 * @note 未來可能需要加上 namespace。
 */
typedef enum {
    DEAD,
    ALIVE
} Is_Alive;


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
    Is_Alive is_alive; /**< 角色是否還存活，可以依靠此資訊取代 crossable */
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


/** @brief 顯示角色部份訊息，Debug 用
 * @param self 要顯示的角色
 * @return 角色存活狀態
 */
Is_Alive EXPORT(is_alive) (Character_Base * self);

#undef EXPORT
#endif
