#ifndef USER_H
#define USER_H

#include <stdint.h>


#define USER_FREE(ptr) user_free(ptr); ptr = NULL;


struct _user {
    int8_t age;
    int8_t code;
    int8_t a1;
    int8_t a2;
    int32_t a3;
    char *name;
};


struct _user *user_malloc(void);
void user_free(void *ptr);
int user_check(struct _user *ptr);
char user_size(struct _user *ptr);
void user_stop(void);
#endif
