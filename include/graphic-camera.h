#ifndef HEADER_GRAPHIC_CAMERA
#define HEADER_GRAPHIC_CAMERA


#include "namespace.h"


#include "game_status.h"
#include "graphic-message.h"


/** @brief Namespace CAMERA
 */
#define EXPORT(name) CAMERA(name)


/** @brief Camera 模式
 *
 * 目前提供的三種模式中，UNDEFINE 是遊戲初始化時使用的，一旦遊戲
 * 開始即可根據 Map 物件確定 Camera 模式，並且做切換。
 */
typedef enum {
    CAMERA_UNDEFINE, /**< 模式為未義，初始化用 */
    CAMERA_FIX, /**< 攝影機固定，玩家角色移動，用在攝影機碰到邊界時 */
    CAMERA_MOVE, /**< 攝影機移動，玩家角色固定，用在攝影機沒碰到邊界的情況 */
} Camera_Mode;


/** @brief Camera 物件定義
 *
 * 為了簡化操作，所以這邊有儲存玩家資料，但真正需要用到角色資料時，可以從外部傳入，因此 player 後續可以拿掉。
 *
 * start 跟 end 這兩個資料後續可以用 Rectangle 或 Two_Point 處理。
 */
typedef struct {
    Character_Access player; /**< 玩家角色資料 */
    Style_Access dead; /**< 死亡用圖形 */
    Point_Access center; /**< 中心位置 */
    int64_t max_x; /**< 最大 x 值，預設為 25 */
    int64_t max_y; /**< 最大 y 值，預設為 21 */
    Point_Access start; /**< 攝影機左上角位置 */
    Point_Access end; /**< 攝影機右下角位置 */
    Camera_Mode horizon; /**< 攝影機水平方向模式 */
    Camera_Mode vertical; /**< 攝影機垂直方向模式 */
    Map_Access map; /**< 地圖物件 */
} Camera_Type;
typedef Camera_Type *Camera_Access;


  /** @brief 建立新的 Camera 物件
   * @return Camera 物件
   *
   * MAX_X 的預設值為 25，MAX_Y 的預設值為 21，可透過 EXPORT(set_max_x) 跟 EXPORT(set_max_y)
   * 來修改。
  */
Camera_Access EXPORT(start) (void);


  /** @brief 釋放 Camera 物件
   * @param self 要初始化的角色物件
  */
void EXPORT(stop) (Camera_Access self);


  /** @brief 重設螢幕顯示最大值
   * @param self Camera 物件
   * @param other 要設定的數值
  */
void EXPORT(set_max_by_point) (Camera_Access self, Point_Access other);


  /** @brief 重設螢幕顯示最大值
 * @param self 點座標
 */
#define Graphic_Camera_set_max(self, ...)\
{\
    Point_Type item = { __VA_ARGS__ };\
    EXPORT(set_max_by_point)(self, &item);\
}


  /** @brief 設定玩家角色
   * @param self Camera 物件
   * @param player 指定的玩家物件
   *
   * @warning 初始化時會將真實座標直接當成圖形座標，因為初始座標是固定在螢幕中間的點，這個點會固定不變，之後會修改
  */
void EXPORT(set_player) (Camera_Access self, Character_Access player);


  /** @brief 設定死亡時顯示的圖形
   * @param self Camera 物件
   * @param dead 指定的圖形
  */
void EXPORT(set_dead_style) (Camera_Access self, Style_Access dead);


  /** @brief 設定地圖物件
   * @param self Camera 物件
   * @param map 指定的地圖物件
  */
void EXPORT(set_map) (Camera_Access self, Map_Access map);


  /** @brief 處理訊息並更改角色資料
   * @param self Camera 物件
   * @param from_pool 角色池
   * @param box_access 訊息視窗
   * @param current 當前發出訊息的角色
   * @param message 角色發出的訊息
   * @return 當前必定回傳 true
  */
bool EXPORT(take) (Camera_Access self,
                   Game_Status_Access from_pool,
                   Message_Box_Access box_access,
                   Character_Access current, Message_Type message);

#undef EXPORT

#endif
