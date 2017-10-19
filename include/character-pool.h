#ifndef HEADER_CHARACTER_POOL_TYPE
#define HEADER_CHARACTER_POOL_TYPE

#include <libconfig.h>

#include "character-skill.h"
#include "helper_function/generic_function.h"

#include "container/Status_List.h"
#include "map_system.h"
#include "rectangle-use_this.h"
#include "instance/strings.h"


typedef enum {
    NOT_FOUND = 0,
    FOUND
} Found_Result;


Message_Type Point_Type_over_there(Point_Type * self, Point_Type * other);
Message_Type Point_Type_near_by(Point_Type * self, Point_Type * other);


GENERIC_POOL(Status_Pool, Status_Access);
typedef Status_Pool *Status_Pool_Access;


GENERIC_POOL(Character_Base_Pool, Character_Base_Access);
typedef Character_Base_Pool *Character_Base_Pool_Access;


typedef struct {
    Status_Pool_Access status;
    Character_Base_Pool_Access base;
} Character_Pool_Type;
typedef Character_Pool_Type *Character_Pool_Type_Access;


Status_Access Character_Pool_Type_malloc(Character_Pool_Type * access);
#endif
