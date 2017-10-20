#include "instance/strings.h"

static String_Intern self = NULL;


/** @brief 設定 strings intern 的 repo
 * @param repo string intern 的 repo
 * @return 舊的 intern
 */
String_Intern String_Repo_change(String_Intern repo)
{
    String_Intern origin = self;
    self = repo;
    return origin;
}


/** @brief 註冊 String 到 repo 中，然後回傳註冊後的新 Access
 * @param 想註冊的 String
 * @return 註冊後的 Access
 */
const char *String_Repo_sign_in(const char *str)
{
    uint32_t str_id = strings_intern(self, str);
    return strings_lookup_id(self, str_id);
}


/** @brief 從 repo 中取出對應 string 的 Access
 * @param 想取出的 String
 * @return Repo 中的 Access
 *
 * Repo 中完全相同的 String 只會有一個 address，因此取出後的 address
 * 可以用來驗證兩個 String 是否相等。
 */
const char *String_Repo_search(const char *str)
{
    uint32_t str_id = strings_lookup(self, str);
    return strings_lookup_id(self, str_id);
}
