#ifndef STRING_H
#define STRING_H

#include <stdint.h>


#define USER_FREE(ptr) user_free(ptr); ptr = NULL;


typedef char *String;


String *string_new(char *value);
void string_free(void *ptr);
int string_check(String * ptr);
char string_size(String * ptr);
void string_stop(void);
#endif
