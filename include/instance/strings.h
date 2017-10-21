#ifndef HEADER_STRINGS_REPO_ACCESS
#define HEADER_STRINGS_REPO_ACCESS

#include "helper/strings.h"


/** @brief 設定 strings intern 的 repo
 * @param repo string intern 的 repo
 * @return 舊的 intern
 */
String_Intern String_Repo_change(String_Intern repo);


/** @brief 註冊 String 到 repo 中，然後回傳註冊後的新物件
 * @param str 想註冊的 String
 * @return 註冊後的新物件
 *
 * 本函數不會自動釋放傳入的參數。
 */
const char *String_Repo_sign_in(const char *str);


/** @brief 從 repo 中取出對應 String
 * @param str 想取出的 String
 * @return Repo 中的物件
 *
 * Repo 中完全相同的 String 只會有一個 address，因此取出後的 Access
 * 可以用來驗證兩個 String 是否相等。
 */
const char *String_Repo_search(const char *str);
#endif
