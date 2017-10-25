#include "character_pool-use_self.h"


/** @brief Namespace CP_OBJECT
 */
#define EXPORT(name) CP_OBJECT(name)
#define SUPER(name) CP(name)


static Character_Pool_Access self = NULL;


// ----------------------------------------------
// External API
// ----------------------------------------------
  /** @brief 更換使用的角色池
   * @param this 想要更換的角色池
   * @return 舊的角色池
   *
   * 本函數不會自動釋放舊的角色池，使用者必須自行釋放。
  */
Character_Pool_Access EXPORT(change) (Character_Pool_Access this) {
    Character_Pool_Access origin = self;
    self = this;
    return origin;
}


  /** @brief 分析角色設定檔，產生角色池
   * @param file_path 設定檔位置
   * @param style_pool 用來儲存圖形的圖形池
   * @return 設定結果
  */
Execute_Result EXPORT(parse_npc_config) (const char *file_path,
                                         Style_Pool_Access style_pool) {
    return SUPER(parse_npc_config) (self, file_path, style_pool);
}


  /** @brief 註冊種族資料到角色池
   * @return 待設定的種族資料
  */
Status_Access EXPORT(sign_in) (void) {
    return SUPER(sign_in) (self);
}


  /** @brief 尋找該位置是否有任何角色
   * @param npc 回傳 NPC 的 Access
   * @param point 尋找的位置
   * @return 搜尋結果
   *
   * @note npc 初始值必須設定為 NULL, 如果有找到，則會被設定為該 NPC 的 Access，該 npc 變數即可在函數外部使用。
  */
Found_Result EXPORT(find_character) (Status_Access * npc,
                                     Point_Access point) {
    return SUPER(find_character) (self, npc, point);
}


  /** @brief 重新計算所有角色在畫面上的顯示位置
   * @param rectangle 當前螢幕對應實際位置所產生的的方形
   *
   * 根據方形的兩個點重新計算角色位置。
   */
void EXPORT(calculate_graph_position) (Rectangle_Access rectangle) {
    SUPER(calculate_graph_position) (self, rectangle);
}


  /** @brief 實體化友方角色
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色
   *
   * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_ally) (const char *race,
                                const char *name, Map_Access map) {
    return SUPER(use_ally) (self, race, name, map);
}


  /** @brief 實體化敵方角色
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色
   *
   * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_enemy) (const char *race,
                                 const char *name, Map_Access map) {
    return SUPER(use_enemy) (self, race, name, map);
}


  /** @brief 實體化中立角色
   * @param race 角色種族
   * @param name 角色名稱
   * @param map 使用的地圖
   * @return 實體化的角色
   *
   * 本函數會根據種族來實體化角色，並回傳 Access，請使用該 Access 進行後續
   * 設定。
   */
Status_Access EXPORT(use_neutral) (const char *race, const char *name,
                                   Map_Access map) {
    return SUPER(use_neutral) (self, race, name, map);
}


  /** @brief 將玩家角色實體化
   * @return 回傳玩家角色以供後續設定
   */
Status_Access EXPORT(use_player) (void) {
    return SUPER(use_player) (self);
}


  /** @brief 回傳實體化角色總和
   * @return 回傳總和數值
   */
uint8_t EXPORT(instance_count) (void) {
    return SUPER(instance_count) (self);
}


  /** @brief 根據編號回傳角色 Access
   * @param index 要找出來的角色編號
   * @return 回傳角色 Access
   */
Status_Access EXPORT(get_instance_by_index) (int index) {
    return SUPER(get_instance_by_index) (self, index);
}


  /** @brief 回傳角色行動訊息
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
Message_Type EXPORT(action) (Status_Access current_character) {
    return SUPER(action) (self, current_character);
}


  /** @brief 攻擊敵方角色
   * @param current 進行攻擊的角色
   * @param target 被攻擊的敵方角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但角色池內也有記錄角色立場的欄位，
   * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_enemy_by) (Status_Access current,
                                  Status_Access target) {
    return SUPER(attack_enemy_by) (self, current, target);
}


  /** @brief 攻擊友軍角色
   * @param current 進行攻擊的角色
   * @param target 被攻擊的友軍角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但角色池內也有記錄角色立場的欄位，
   * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_ally_by) (Status_Access current,
                                 Status_Access target) {
    return SUPER(attack_ally_by) (self, current, target);
}


  /** @brief 攻擊中立角色
   * @param current 進行攻擊的角色
   * @param target 被攻擊的中立角色
   * @return 目標的生存狀況
   *
   * 執行 character.attack 後，NPC 會改變立場，但角色池內也有記錄角色立場的欄位，
   * 直接呼叫 character.attack 不會改變 pool 內的立場，因此必須透過 pool 轉呼叫。
  */
Is_Alive EXPORT(attack_neutral_by) (Status_Access current,
                                    Status_Access target) {
    return SUPER(attack_neutral_by) (self, current, target);
}


  /** @brief 攻擊玩家角色
   * @param current 進行攻擊的角色
   * @return 目標的生存狀況
  */
Is_Alive EXPORT(attack_player_by) (Status_Access current) {
    return SUPER(attack_player_by) (self, current);
}

#undef EXPORT
#undef SUPER
