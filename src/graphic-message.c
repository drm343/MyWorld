#include "graphic-message.h"

static SDL_Point box_array[5] = {
    {0, 20 * 24},
    {0, 600},
    {799, 600},
    {799, 20 * 24},
    {0, 20 * 24}
};


static void set_box(Message_Box_Access box_1,
                    int64_t start_x, int64_t start_y,
                    int64_t width, int64_t height)
{
    box_array[0].x = start_x;
    box_array[0].y = start_y;
    box_array[4].x = start_x;
    box_array[4].y = start_y;
    box_array[1].x = start_x;
    box_array[1].y = start_y + height;
    box_array[2].x = start_x + width;
    box_array[2].y = start_y + height;
    box_array[3].x = start_x + width;
    box_array[3].y = start_y;
}


static void add_message(Message_Box_Access access, char *message)
{
    strings_intern(access->history, message);
}


// --------------------------------
// Control API
// --------------------------------
static Message_Box_Access message_box_start(void)
{
    Message_Box_Access access = calloc(1, sizeof(Message_Box));
    access->box = box_array;
    access->history = strings_new();
    return access;
}


static void message_box_stop(Message_Box_Access access)
{
    strings_free(access->history);
    free(access);
}


// --------------------------------
// Export API
// --------------------------------
Message_Box_API message_box = {
    .start = message_box_start,
    .stop = message_box_stop,

    .set_box = set_box,

    .add = add_message
};
