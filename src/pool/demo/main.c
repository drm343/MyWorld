#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "string.h"
#include "int.h"
#include "user.h"

#define DEBUG_MESSAGE(FORMAT, ...) printf("%s %d: ", __FILE__, __LINE__); \
    printf(FORMAT, ## __VA_ARGS__);

#define DEBUG_INT(INT, MESSAGE) printf("%s %d: %s %d\n", __FILE__, __LINE__, MESSAGE, int_result(INT));
#define DEBUG_U(USER) printf("%s %d: %s %d %d\n", __FILE__, __LINE__, USER->name, USER->age, user_size(USER));


struct _user set_user(char *name, int age, int code)
{
    struct _user user = {
        .name = strdup(name),
        .age = age,
        .code = code
    };
    return user;
}


int main(void)
{
    struct _int *num_1 = int_malloc();
    int_a(num_1, 1);
    int_b(num_1, 2);
    DEBUG_INT(num_1, "1 + 2 =");
    INT_FREE(num_1);

    num_1 = int_malloc();
    int_a(num_1, 2);
    int_b(num_1, 3);
    DEBUG_INT(num_1, "2 + 3 =");

    num_1 = int_malloc();
    int_a(num_1, 3);
    int_b(num_1, 4);
    DEBUG_INT(num_1, "3 + 4 =");

    num_1 = int_malloc();
    int_a(num_1, 4);
    int_b(num_1, 5);
    DEBUG_INT(num_1, "4 + 5 =");

    struct _int *num_2 = int_malloc();
    int_a(num_2, 5);
    int_b(num_2, 6);
    DEBUG_INT(num_2, "5 + 6 =");

    num_1 = int_malloc();
    num_1 = int_malloc();
    num_1 = int_malloc();
    if (num_1 == NULL) {
        DEBUG_MESSAGE("is null\n");
        INT_FREE(num_2);
    } else {
        DEBUG_MESSAGE("is not null!\n");
    }

    num_1 = int_malloc();
    int_a(num_1, 6);
    int_b(num_1, 7);
    DEBUG_INT(num_1, "6 + 7 =");

    int_stop();

    struct _user *a = user_malloc();
    *a = set_user("a1", 30, 0);
    DEBUG_U(a);

    struct _user *b = user_malloc();
    *b = set_user("b1", 20, 1);
    DEBUG_U(b);
    USER_FREE(b);

    struct _user *c = user_malloc();
    *c = set_user("c1", 10, 2);
    DEBUG_U(c);

    b = user_malloc();
    if (b == NULL) {
        DEBUG_MESSAGE("b null\n");
    }
    *b = set_user("b2", 7, 3);
    DEBUG_U(b);

    struct _user *d = user_malloc();
    if (d == NULL) {
        DEBUG_MESSAGE("d null\n");
    }
    USER_FREE(a);
    d = user_malloc();
    if (d == NULL) {
        DEBUG_MESSAGE("d null 2\n");
    }
    *d = set_user("a long d1 name", 7, 3);
    DEBUG_U(d);

    user_stop();

    String *s_a = string_new("hello");
    String *s_b = string_new("ok");
    DEBUG_MESSAGE("%s %s\n", *s_a, *s_b);
    String *s_c = string_new("world");
    string_stop();
}
