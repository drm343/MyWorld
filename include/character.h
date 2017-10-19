#ifndef HEADER_CHARACTER
#define HEADER_CHARACTER

#include <stdbool.h>

#include "graphic.h"

#include "instance/point.h"


typedef struct Character_Base {
    const char *name;
    Style_Access Mark;
    Point_Access Real_Position;
    Point_Access Graph_Position;
    Yes_No crossable;
    Yes_No attackable;
    bool is_alive;
    Use_Type status;

    void (*copy) (struct Character_Base *, struct Character_Base *);
} Character_Base_Type;
typedef Character_Base_Type *Character_Base_Access;


void Character_Base_init(Character_Base_Type *);
void Character_Base_free(Character_Base_Type *);
#endif
