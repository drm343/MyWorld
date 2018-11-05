#ifndef HEADER_NAMESPACE
#define HEADER_NAMESPACE

/** @file namespace.h
 *
 * 當定義一個結構跟對應的函數時，請在函數外面加上 EXPORT 這個 macro，
 * EXPORT 則轉包函數名稱給對應的 namespace macro。
 *
 * 使用時只要直接使用對應的 namespace macro 就可以呼叫到正確的函數了。
 *
 * 如果不使用 macro，必須加上 namespace 才能呼叫到正確的函數。
 *
 * 例如當 Character_Pool 在 create 時必須寫成 Character_Pool_create，
 * 如果外部程式要簡化呼叫，可以使用 namespace 中的 CP，並且簡寫成 CP(create)。
 */

/** @brief namespace for Character_Pool
*/
#define CP(name) Character_Pool_##name

/** @brief namespace for Character_Pool_Access
*/
#define CP_OBJECT(name) Character_Pool_Access_##name

/** @brief namespace for Character_Factory
*/
#define CF(name) Character_Factory_##name

/** @brief namespace for May_Type
*/
#define MAP(name) Map_Type_##name

/** @brief namespace for Camera
*/
#define CAMERA(name) Graphic_Camera_##name

/** @brief namespace for Game Status
*/
#define GAME(name) Game_Status_##name

/** @brief namespace for Character_Status
*/
#define STATUS(name) Status_##name

/** @brief namespace for Character_Status
*/
#define STYLE_P(name) Style_Pool_##name

#endif
