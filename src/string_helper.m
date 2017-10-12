#include "string_helper.h"

static size_t strlen_utf8(char *str) {
  size_t max_counter = strlen(str);
  size_t move_position = 0;
  int64_t len = 0;
  unsigned char check = 0;

  for (size_t index = 0; index < max_counter; index += move_position) {
    check = str[index];
    len++;

    if (check < 0b11000000) {
      move_position = 1;
    }
    else if (check < 0b11100000 && check > 0b11000000) {
      move_position = 2;
    }
    else if (check < 0b11110000 && check > 0b11100000) {
      move_position = 3;
    }
    else if (check > 0b11110000) {
      move_position = 4;
    }
    else if (check == 0) {
      break;
    }
    else {
      printf("bug\n");
      break;
    }
  }
  return len;
}


static size_t strlen_ascii(char *str) {
  return strlen(str);
}

static size_t calculate_grid_width_utf8(char *str, int64_t full_size) {
  size_t max_counter = strlen(str);
  size_t index = 0;
  size_t move_position = 0;
  int64_t len = 0;
  unsigned char check = 0;

  for (index = 0; index < max_counter; index += move_position) {
    check = str[index];
    len += 2;

    if (check < 0b11000000) {
      move_position = 1;
      len--;
    }
    else if (check < 0b11100000 && check > 0b11000000) {
      move_position = 2;
    }
    else if (check < 0b11110000 && check > 0b11100000) {
      move_position = 3;
    }
    else if (check > 0b11110000) {
      move_position = 4;
    }
    else if (check == 0) {
      break;
    }
    else {
      printf("bug\n");
      break;
    }
  }
  return len * (full_size / 2);
}


static void remove_last(char **str) {
  size_t i = 0;//strlen(*str);

  for (i = strlen(*str); i > 0; i--) {
    unsigned char b = (unsigned char)((*str)[i - 1]) & 0b11000000;

    if (b == 0b10000000) {
      (*str)[i - 1] = 0;
    }
    else {
      (*str)[i - 1] = 0;
      break;
    }
  }
}

String_API string = {
  .remove_last = remove_last,
  .strlen = strlen_utf8,
  .count_width = calculate_grid_width_utf8,
  .strlen_ascii = strlen_ascii
};
