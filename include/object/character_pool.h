#ifndef HEADER_CHARACTER_POOL_OBJECT
#define HEADER_CHARACTER_POOL_OBJECT

#include <time.h>

#include "character-pool.h"


#define SELF(name) Character_Pool_##name

typedef struct {
    Character_Pool_Type *prepare;
    Character_Pool_Type *used;
    Status_List *ally;
    Status_List *enemy;
    Status_List *neutral;
} Character_Pool;
typedef Character_Pool *Character_Pool_Access;


  /** @brief 產生 Character_Pool 物件
   * @param max_config_size 種族池最大值
   * @param max_instance_size 實體角色池最大值
   * @return 新的 Character_Pool 物件
  */
Character_Pool *SELF(create) (uint8_t max_config_size,
                              uint8_t max_instance_size);

  /** @brief 結束 Character_Pool 物件
   * @param access 要使用的 Character_Pool
  */
void SELF(free) (Character_Pool * access);

  /** @brief 分析角色設定，產生種族池
   * @param access 要使用的 Character_Pool
   * @param file_path 設定檔位置
   * @param style_pool 實體化圖形介面用的圖形池
   * @return 設定結果
  */
Execute_Result SELF(parse_npc_config) (Character_Pool * access,
                                       const char *file_path,
                                       Style_Pool_Access style_pool);

  /** @brief 設定種族池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
void SELF(set_prepare) (Character_Pool * access, uint8_t max_size);

  /** @brief 取出種族池
   * @param access 要使用的 Character_Pool
   * @return 種族池
  */
Character_Pool_Type *SELF(prepare) (Character_Pool * access);

  /** @brief 設定實體化的角色池最大值
   * @param access 要使用的 Character_Pool
   * @param max_size 想設定的最大值
  */
void SELF(set_used) (Character_Pool * access, uint8_t max_size);

  /** @brief 取出實體化的角色池
   * @param access 要使用的 Character_Pool
   * @return 實體化的角色池
  */
Character_Pool_Type *SELF(used) (Character_Pool * access);

  /** @brief 註冊種族資料到角色池
   * @param access 要使用的 Character_Pool
   * @return 待設定的種族資料
  */
Status_Access SELF(sign_in) (Character_Pool * access);

  /** @brief 尋找該位置是否有任何角色
   * @param access 要使用的 Character_Pool
   * @param npc 回傳 NPC 的 Access
   * @param point 尋找的位置
   * @return 搜尋結果
   *
   * npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的
   * Address，該 npc 變數即可在函數外部使用。
  */
Found_Result SELF(find_character) (Character_Pool * access,
                                   Status_Access * npc,
                                   Point_Access point);

  /** @brief 重新計算所有角色在畫面上的顯示位置
   * @param access 要使用的 Character_Pool
   * @param rectangle 當前圖形所在的方形
   *
   * 根據方形的兩個點重新計算角色位置。
   */
void SELF(calculate_graph_position) (Character_Pool * access,
                                     Rectangle_Access rectangle);

  /** @brief 設定友方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   *
   * 使用者不需要呼叫本函數
   */
void SELF(set_ally) (Character_Pool * access, uint8_t max_size);

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
Status_Access SELF(use_ally) (Character_Pool * access, const char *race,
                              const char *name, Map_Access map);

  /** @brief 設定 NPC 為友方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   *
   * 使用者不需要呼叫本函數
   */
void SELF(add_ally) (Character_Pool * access, Status_Access npc);

  /** @brief 設定敵方角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   *
   * 使用者不需要呼叫本函數
   */
void SELF(set_enemy) (Character_Pool * access, uint8_t max_size);

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
Status_Access SELF(use_enemy) (Character_Pool * access, const char *race,
                               const char *name, Map_Access map);

  /** @brief 設定 NPC 為敵方角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   *
   * 使用者不需要呼叫本函數
   */
void SELF(add_enemy) (Character_Pool * access, Status_Access npc);

  /** @brief 設定中立角色總量的上限值
   * @param access 要使用的 Character_Pool
   * @param max_size List 的最大值
   *
   * 使用者不需要呼叫本函數
   */
void SELF(set_neutral) (Character_Pool * access, uint8_t max_size);

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
Status_Access SELF(use_neutral) (Character_Pool * access, const char *race,
                                 const char *name, Map_Access map);

  /** @brief 設定 NPC 為中立角色
   * @param access 要使用的 Character_Pool
   * @param npc 要設定的角色
   *
   * 使用者不需要呼叫本函數
   */
void SELF(add_neutral) (Character_Pool * access, Status_Access npc);

  /** @brief 分配一個半完成初始化的角色實體
   * @param access 要使用的 Character_Pool
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 半實體化的角色 Access
   *
   * 本函數會根據角色種族來實體化角色資料，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   *
   * 不要呼叫本函數
   */
Status_Access SELF(use_npc) (Character_Pool * access, const char *race,
                             const char *name, Map_Access map);

  /** @brief 將玩家角色實體化
   * @param access 要使用的 Character_Pool
   * @return 回傳玩家角色已供後續設定
   */
Status_Access SELF(use_player) (Character_Pool * access);

  /** @brief 回傳角色實體總和
   * @param access 要使用的 Character_Pool
   * @return 回傳總和數值
   */
uint8_t SELF(instance_count) (Character_Pool * access);

  /** @brief 回傳角色實體
   * @param access 要使用的 Character_Pool
   * @param index 要找出來的角色編號
   * @return 回傳角色
   */
Status_Access SELF(get_instance_by_index) (Character_Pool * access,
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
Message_Type SELF(action) (Character_Pool * access,
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
Is_Alive SELF(attack_enemy_by) (Character_Pool * access,
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
Is_Alive SELF(attack_ally_by) (Character_Pool * access,
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
Is_Alive SELF(attack_neutral_by) (Character_Pool * access,
                                  Status_Access current,
                                  Status_Access target);

  /** @brief 攻擊玩家角色
   * @param access 要使用的 Character_Pool
   * @param current 進行攻擊的角色
   * @return 目標的生存狀況
  */
Is_Alive SELF(attack_player_by) (Character_Pool * access,
                                 Status_Access current);
#undef SELF
#endif