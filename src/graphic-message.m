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
    int64_t width, int64_t height) {
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


static void add_message(Message_Box_Access access, String message) {
  String_Pool_Access pool = NULL;

  if (access->current_pool == 0) {
      pool = access->pool_1;
  }
  else {
      pool = access->pool_2;
  }
  String new_message = string_pool.malloc(pool, string.strlen_ascii(message));
  strcpy(new_message, message);

  access->history[access->current] = new_message;
  access->current = access->current + 1;

  if (access->current >= 10) {
    access->current = 0;
  }

  access->counter = access->counter + 1;
  if (access->counter > 20) {
    access->current_pool = !(access->current_pool);
    access->counter = 0;
  }
}


// --------------------------------
// Control API
// --------------------------------
static Message_Box_Access message_box_start(void) {
  Message_Box_Access access = calloc(1, sizeof(Message_Box));
  access->box = box_array;
  access->pool_1 = string_pool.start(1000);
  access->pool_2 = string_pool.start(1000);
  access->current_pool = 0;
  access->history = calloc(10, sizeof(String));
  access->current = 0;
  return access;
}


static void message_box_stop(Message_Box_Access access) {
  string_pool.stop(access->pool_1);
  string_pool.stop(access->pool_2);
  free(access->history);
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
