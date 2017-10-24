#ifndef HEADER_MAIN
#define HEADER_MAIN

#include <unistd.h>
#include <libgen.h>

#include <libconfig.h>

#include "helper/strings.h"
#include "graphic-camera.h"


#define SP(name) Style_Pool_##name
#include "main_set_config.h"

/*
#define MAP(name) Map_Type_##name
#define CP_OBJECT(name) Character_Pool_Access_##name
#define CP(name) Character_Pool_##name
#define CAMERA(name) Graphic_Camera_##name
#define STATUS(name) Status_##name
*/
#endif

/** @mainpage Create your own roguelike in C
 *
 * 本專案使用 C 撰寫，需要安裝 SDL2、SDL2-ttf、libconfig，如果你是 windows 的使用者，請考慮使用 "Bash on Ubuntu"。
 *
 * 下列工具為選擇性的，可根據需求選擇是否安裝。<br>
 * doxygen、graphviz、git、make、indent。
 *
 * @see 開發相關紀錄 https://drm343.github.io
 *
 * @section 名詞定義
 *
 * @subsection 結構
 *
 * 文件中凡是指稱到結構的地方，一律指該結構本身。
 *
 * @note 程式中儘量不要直接用結構，而是使用結構的指標，只在生命周期極短可以使用 auto 的狀況下使用結構本身。
 *
 * @subsection 物件
 *
 * 文件中凡是指稱到物件的地方，一律指該結構的指標，注意這點跟 C 標準中所指的物件並不相同。
 *
 * @subsection Access
 *
 * 這個名詞從 Ada 中取過來的，類似 C 的 Pointer，但擁有更多特性，凡是看到 type 後面加上 _Access 的
 * 一律等同於前面所提物件在程式上的定義，這是為了避免使用太多 * 而採用的名稱慣例。
 *
 * 文件中單純只寫 Access 的情況則可以看成物件在記憶體中的實際位置。
 *
 * @note 要注意這是 C 不是 Ada，因此 Access 並沒有真的像 Ada 一樣有更多特性可以使用。
 *
 * @subsection API
 *
 * 凡是以 API 做結尾的，都是早期的設計，將會被重構，請儘量不要使用。
 *
 * @subsection 角色
 *
 * 若沒有特別註解，則文件中的「角色」皆代表 Character_Status 物件。
 *
 * @subsection 種族資料
 *
 * 目前的基本結構等同於角色，可以看成角色原型，所有的角色都會根據種族來建立出新角色。
 *
 * @note 目前只有玩家沒有種族資料，因為還沒建立玩家使用的種族設定跟程式碼。
 *
 * @subsection 角色池
 *
 * 若沒有特別註解，則文件中的「角色池」皆代表 Character_Pool 物件，至少包含種族資料
 * 跟實際產生的角色資料。
 *
 * @subsection 實體化
 *
 * 指從角色池中找出對應種族，複製產生新角色後，將 Access 回傳讓使用者做後續處理的過程。
 *
 * @subsection 圖形池
 *
 * 若沒有特別註解，則文件中的「圖形池」皆代表 Style_Pool 的物件。
 *
 * @subsection 地圖
 *
 * 若沒有特別註解，則文件中的「地圖」皆代表 Map_Type 物件。
 *
 * @warning 關於地圖的結構，後續可能會修改。
 *
 * @subsection 訊息欄 Message_Box
 *
 * 用來顯示訊息的地方，預設使用 SDL 實作，原本在 Ada 中是打算 Generics 的方式來讓實作
 * 跟定義分開，並且通用化，目前用 C 先不要考慮通用的問題。
 */
