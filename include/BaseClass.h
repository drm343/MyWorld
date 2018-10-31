#ifndef HEADER_BASE_CLASS
#define HEADER_BASE_CLASS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <uuid/uuid.h>

#include "helper/debug.h"


#define NEW(type) calloc(1, sizeof(struct type));


typedef struct Class {
    char *id;
    int8_t counter;
} *Class;


/* @brief 建立新的 class id，必須手動 free
 * @return 新建立的 Class
 */
Class NEW_CLASS_ID(void);


/* @brief 釋放不需要的 class
 * @param class 要釋放的 Class
 */
void RELEASE_CLASS(Class class);


/* @brief 檢查兩個 Class 是否相同
 * @param class_1
 * @param class_2
 * @return 檢查結果
 */
bool CHECK_CLASS(Class class_1, Class class_2);
#endif
