#ifndef HEADER_CHARACTER_POOL
#define HEADER_CHARACTER_POOL

#include <libconfig.h>

#include "character-skill.h"
#include "helper_function/generic_function.h"

#include "container/Status_List.h"
#include "map_system.h"
#include "instance/rectangle.h"


typedef enum {
  NOT_FOUND = 0,
  FOUND
} Found_Result;


Message_Type Point_Type_over_there(Point_Type *self, Point_Type *other);
Message_Type Point_Type_near_by(Point_Type *self, Point_Type *other);


GENERIC_POOL(Status_Pool, Status_Access);
typedef Status_Pool * Status_Pool_Access;


GENERIC_POOL(Character_Base_Pool, Character_Base_Access);
typedef Character_Base_Pool * Character_Base_Pool_Access;


typedef struct {
  Status_Pool_Access status;
  Character_Base_Pool_Access base;
} Character_Pool_Type;
typedef Character_Pool_Type * Character_Pool_Type_Access;


@interface Character_Pool: NSObject {
  Character_Pool_Type *prepare;
  Character_Pool_Type *used;
  Status_List *ally;
  Status_List *enemy;
  Status_List *neutral;
}

+ (id) create: (uint8_t) max_config_size with_instance_size: (uint8_t) max_instance_size;
+ (Status_Access) malloc: (Character_Pool_Type_Access) pool_access;
- (Execute_Result) parse_npc_config: (const char *) file_path with_style: (Style_Pool_Access) style_pool;
- (id) setPrepare: (uint8_t) max_size;
- (Character_Pool_Type *) prepare;
- (id) setUsed: (uint8_t) max_size;
- (Character_Pool_Type *) used;
- (void) dealloc;
- (Status_Access) sign_in;
- (Found_Result) find_character: (Status_Access *) npc with_position: (Point_Access) point;
- (id) calculate_graph_position: (Rectangle_Access) rectangle;
- (Status_Access) use_npc: (NSString *) race with_name: (NSString *) name and_map: (Map_Access) map;

- (id) set_ally: (uint8_t) max_size;
- (Status_Access) use_ally: (NSString *) race with_name: (NSString *) name and_map: (Map_Access) map;
- (id) add_ally: (Status_Access) npc;

- (id) set_enemy: (uint8_t) max_size;
- (Status_Access) use_enemy: (NSString *) race with_name: (NSString *) name and_map: (Map_Access) map;
- (id) add_enemy: (Status_Access) npc;

- (id) set_neutral: (uint8_t) max_size;
- (Status_Access) use_neutral: (NSString *) race with_name: (NSString *) name and_map: (Map_Access) map;
- (id) add_neutral: (Status_Access) npc;

- (Status_Access) use_player;
- (uint8_t) instance_count;
- (Status_Access) get_instance_by_index: (int) index;

- (Message_Type) action: (Status_Access) current_character;

- (Is_Alive) attack_enemy_by: (Status_Access) current with_target: (Status_Access) target;
- (Is_Alive) attack_ally_by: (Status_Access) current with_target: (Status_Access) target;
- (Is_Alive) attack_neutral_by: (Status_Access) current with_target: (Status_Access) target;
- (Is_Alive) attack_player_by: (Status_Access) current;
@end
typedef Character_Pool * Character_Pool_Access;

#endif
