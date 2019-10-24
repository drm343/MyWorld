#ifndef HEADER_LIST_ITERATOR_CLASS
#define HEADER_LIST_ITERATOR_CLASS

#include "List.h"


#define ITER(name) Iterator_##name


/* @brief 用來簡化 Faction_Group 的 reset_iterator 跟 next 操作
 */
typedef List Iterator;

Iterator ITER(create) (void);
void ITER(copy) (Iterator self, List from);


/* @brief 重設 Iterator 並且回傳第一個資料
 * @param self Iterator 自己
 * @return 第一個資料
 */
void *ITER(reset_iterator) (Iterator self);


/* @brief 取出 Iterator 的下一個資料
 * @param self Iterator 自己
 * @return 下一個資料
 */
void *ITER(next) (Iterator self);


/* @brief 驗證 Iterator 是否完成
 * @param self Iterator 自己
 * @return 是否已完成
 *
 * 本函數會自己釋放 Iterator，請勿再次釋放。
 *
 * 需要不會自己釋放的版本來重複使用 Iterator 的記憶體並且減少要求記憶體的次數。
 */
bool ITER(done) (Iterator self);
#endif
