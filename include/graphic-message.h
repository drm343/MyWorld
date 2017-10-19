#ifndef HEADER_GRAPHIC_MESSAGE
#define HEADER_GRAPHIC_MESSAGE
#include "graphic.h"
#include "helper_function/strings.h"


typedef char *String_Access;

typedef SDL_Point *Message_Box_Points;
typedef struct {
    int8_t grid_width;
    int8_t grid_heigth;
    Message_Box_Points box;
    String_Intern history;
} Message_Box;
typedef Message_Box *Message_Box_Access;


typedef struct {
    Message_Box_Access(*start) (void);
    void (*stop) (Message_Box_Access);
    void (*set_box) (Message_Box_Access, int64_t, int64_t, int64_t,
                     int64_t);
    void (*add) (Message_Box_Access, char *);
} Message_Box_API;
extern Message_Box_API message_box;
#endif
