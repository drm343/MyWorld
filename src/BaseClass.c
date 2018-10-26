#include "BaseClass.h"


char *NEW_CLASS_ID(void)
{
    uuid_t binary_uuid;
    uuid_generate(binary_uuid);
    char *uuid = malloc(37);
    uuid_unparse(binary_uuid, uuid);
    return uuid;
}
