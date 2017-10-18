#include "container/Status_Pool.h"
/** \file
 * 
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。
 */

Status_Pool* Status_Pool_start(uint8_t max_size) {
  Status_Pool *result = calloc(1, sizeof(Status_Pool));
  Status *instance = calloc(max_size, sizeof(Status));
  Status_Item *pool = calloc(max_size, sizeof(Status_Item));

  Status *content = NULL;
  Status_Item *item = NULL;

  result->pool = pool;
  result->item = instance;
  result->max_size = max_size;

  for (uint8_t index = 0; index < max_size; index++) {
    content = &(result->item[index]);

    item = &(result->pool[index]);
    item->is_used = UNUSE;
    item->content = content;
  }
  return result;
}

void Status_Pool_stop(Status_Pool *access) {
  free(access->pool);
  free(access->item);
  free(access);
}

Status* Status_Pool_malloc(Status_Pool *access) {
  Status *content = NULL;
  Status_Item *item = NULL;
  uint8_t max_size = access->max_size;

  for (uint8_t index = 0; index < max_size; index++) {
    item = &(access->pool[index]);

    if (item->is_used == UNUSE) {
      item->is_used = IN_USE;
      content = item->content;
      break;
    }
  }
  return content;
}

