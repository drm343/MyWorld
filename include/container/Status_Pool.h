#ifndef HEADER_CONTAINER_POOL_Status_Pool
#define HEADER_CONTAINER_POOL_Status_Pool

#include <stdint.h>
#include <stdbool.h>

#include "status.h"

/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。

 */

typedef struct _Status_Item {
  Status *content;
  bool is_used;
} Status_Item;

typedef struct {
  Status_Item *pool;
  Status *item;
  uint8_t max_size;
} Status_Pool;

Status_Pool* Status_Pool_start(uint8_t max_size);
void Status_Pool_stop(Status_Pool *access);
Status* Status_Pool_malloc(Status_Pool *access);
#endif