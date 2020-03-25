#ifndef INT_H
#define INT_H

#include <stdint.h>

struct _int {
    int8_t a;
    int8_t b;
};


struct _int *int_malloc(void);
void int_free(void *ptr);
void int_a(struct _int *ptr, int8_t num);
void int_b(struct _int *ptr, int8_t num);
int8_t int_result(struct _int *ptr);
void int_stop(void);


#define INT_FREE(ptr) int_free(ptr); ptr = NULL;
#endif
