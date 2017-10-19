#ifndef HEADER_CHARACTER_POOL_OBJECT
#define HEADER_CHARACTER_POOL_OBJECT

#include <libconfig.h>
#include <time.h>

#include "character-skill.h"
#include "helper_function/generic_function.h"

#include "container/Status_List.h"
#include "map_system.h"
#include "rectangle-use_this.h"
#include "instance/strings.h"


#define EXPORT(name) Character_Pool_##name


/** @brief 角色池基本定義
 * 
 * 建議不要直接使用本結構名稱，而是改用 Character_Pool。
 */
struct _Character_Pool;

/** @brief 角色池基本定義
 * 
 * 建議使用本名稱，僅提供 API 供使用者操作本結構。
 */
typedef struct _Character_Pool Character_Pool;

/** @brief 角色池 Access
 * 
 * 僅提供 API 供使用者操作本結構。
 */
typedef Character_Pool *Character_Pool_Access;


  /** @brief 產生 Character_Pool 物件
   * @param max_config_size 種族池最大值
   * @param max_instance_size 實體角色池最大值
   * @return 新的 Character_Pool 物件
  */
Character_Pool *EXPORT(create) (uint8_t max_config_size,
                                uint8_t max_instance_size);

  /** @brief 結束 Character_Pool 物件
   * @param access 要使用的 Character_Pool
  */
void EXPORT(free) (Character_Pool * access);

  /** @brief 分析角色設定，產生種族池
   * @param access 要使用的 Character_Pool
   * @param file_path 設定檔位置
   * @param style_pool 實體化圖形介面用的圖形池
   * @return 設定結果
  */
Execute_Result EXPORT(parse_npc_config) (Character_Pool * access,
                                         const char *file_path,
                                         Style_Pool_Access style_pool);

  /** @brief 註冊種族資料到角色池
   * @param access 要使用的 Character_Pool
   * @return 待設定的種族資料
  */
Status_Access EXPORT(sign_in) (Character_Pool * access);

  /** @brief 尋找該位置是否有任何角色
   * @param access 要使用的 Character_Pool
   * @param npc 回傳 NPC 的 Access
   * @param point 尋找的位置
   * @return 搜尋結果
   *
   * npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的
   * Address，該 npc 變數即可在函數外部使用。
  */
Found_Result EXPORT(find_character) (Character_Pool * access,
                                     Status_Access * npc,
                                     Point_Access point);

  /** @brief 重新計算所有角色在畫面上的顯示位置
   * @param access 要使用的 Character_Pool
   * @param rectangle 當前圖形所在的方形
   *
   * 根據方形的兩個點重新計算角色位置。
   */
void EXPORT(calculate_graph_position) (Character_Pool * access,
                                       Rectangle_Access rectangle);

  /** @brief 實體化友方角色
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_ally) (Character_Pool * access, const char *race,
                                const char *name, Map_Access map);

  /** @brief 實體化敵方角色
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_enemy) (Character_Pool * access, const char *race,
                                 const char *name, Map_Access map);

  /** @brief 實體化中立角色
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_neutral) (Character_Pool * access,
                                   const char *race, const char *name,
                                   Map_Access map);

  /** @brief 將玩家角色實體化
   * @param access 要使用的 Character_Pool
   * @return 回傳玩家角色已供後續設定
   */
Status_Access EXPORT(use_player) (Character_Pool * access);

  /** @brief 回傳角色實體總和
   * @param access 要使用的 Character_Pool
   * @return 回傳總和數值
   */
uint8_t EXPORT(instance_count) (Character_Pool * access);

  /** @brief 回傳角色實體
   * @param access 要使用的 Character_Pool
   * @param index 要找出來的角色編號
   * @return 回傳角色
   */
Status_Access EXPORT(get_instance_by_index) (Character_Pool * access,
                                             int index);

  /** @brief 回傳角色行動訊息
   * @param access 要使用的 Character_Pool
   * @param current_character 當前進行行動的角色
   * @return 回傳該角色的行動結果
   *
   *  ALLY
   *  1. 確定屬於 FACTION_ENEMY 的 NPC 數量 count
   *  2. 根據 count 隨機取出一個 NPC
   *  3. 根據雙方位置判斷該如何移動(先做測試用路徑搜尋)
   *
   *  Enemy
   *  1. 從 FACTION_ALLY、FACTION_NEUTRAL、FACTION_PLAYER 中隨機挑出一個 FACTION
   *  2. 確定屬於該 FACTION 的 NPC 數量 count
   *  3. 根據 count 隨機取出一個 NPC
   *  4. 根據雙方位置判斷該如何移動(先做測試用路徑搜尋)
   *
   *  NEUTRAL
   *  隨機移動
  */
Message_Type EXPORT(action) (Character_Pool * access,
                             Status_Access current_character);

  /** @brief 攻擊敵方角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @param target 被攻擊的敵方角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_enemy_by) (Character_Pool * access,
                                  Status_Access current,
                                  Status_Access target);

  /** @brief 攻擊友軍角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @param target 被攻擊的友軍角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_ally_by) (Character_Pool * access,
                                 Status_Access current,
                                 Status_Access target);

  /** @brief 攻擊中立角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @param target 被攻擊的中立角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但 character-pool 內也有
   * 記錄角色立場的欄位，直接呼叫 character.attack 不會改變 pool 內的立
   * 場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_neutral_by) (Character_Pool * access,
                                    Status_Access current,
                                    Status_Access target);

  /** @brief 攻擊玩家角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @return 目標的生存狀況
  */
Is_Alive EXPORT(attack_player_by) (Character_Pool * access,
                                   Status_Access current);
#undef EXPORT
#endif
