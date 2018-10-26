#ifndef HEADER_BASE_CLASS
#define HEADER_BASE_CLASS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uuid/uuid.h>

#define NEW(type) calloc(1, sizeof(struct type));

/* @brief 建立新的 class id，必須手動 free
 *
 */
char *NEW_CLASS_ID(void);
#endif
