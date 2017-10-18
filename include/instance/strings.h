#ifndef HEADER_STRINGS_REPO_ACCESS
#define HEADER_STRINGS_REPO_ACCESS

#include "helper_function/strings.h"


/** @brief 設定 strings intern 的 repo
 * @param repo string intern 的 repo
 * @return 舊的 intern
 */
String_Intern String_Repo_change(String_Intern repo);


/** @brief 註冊 String 到 repo 中，然後回傳註冊後的新 Access
 * @param 想註冊的 String
 * @return 註冊後的 Access
 */
const char * String_Repo_sign_in(const char *str);
#endif
