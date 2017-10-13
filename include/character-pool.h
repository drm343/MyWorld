#ifndef HEADER_CHARACTER_POOL
#define HEADER_CHARACTER_POOL
#include "character-skill.h"
#include "helper_function.h"

#import "map_system.h"

typedef enum {
  NOT_FOUND = 0,
  FOUND
} Found_Result;


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
}

+ (id) create: (uint8_t) max_config_size with_instance_size: (uint8_t) max_instance_size;
+ (Status_Access) malloc: (Character_Pool_Type_Access) pool_access;
- (id) setPrepare: (uint8_t) max_size;
- (Character_Pool_Type *) prepare;
- (id) setUsed: (uint8_t) max_size;
- (Character_Pool_Type *) used;
- (void) dealloc;
- (Status_Access) sign_in;
- (Found_Result) find_character: (Status_Access *) npc with_position: (Point_Access) point;
- (id) calculate_graph_position: (Rectangle_Access) rectangle;
- (Status_Access) use_npc: (NSString *) race with_name: (NSString *) name and_map: (Map_Access) map;
- (Status_Access) use_player;
- (uint8_t) instance_count;
- (Status_Access) get_instance_by_index: (int) index;

@end
typedef Character_Pool * Character_Pool_Access;

#endif