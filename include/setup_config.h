#ifndef HEADER_SETUP_CONFIG
#define HEADER_SETUP_CONFIG

#define BELONG(val, type) type ##_##val

#define GENERIC_POOL(name, struct_name) RECORD_TYPE(name, \
  struct_name pool; \
  uint8_t max_size; \
  uint8_t current_size; \
)

#define STRCMP(x, y) !strcmp(x, y)

#endif
