#include <stdio.h>
#include <libconfig.h>

const char *header_file_name = NULL;
const char *source_file_name = NULL;
const char *include_header_file_name = NULL;
const char *struct_header_file_name = NULL;
const char *name = NULL;
const char *struct_name = NULL;
FILE *file = NULL;


void gen_header(void) {
  char *format = "\
#ifndef HEADER_CONTAINER_POOL_%s\n\
#define HEADER_CONTAINER_POOL_%s\n\
\n\
#include <stdint.h>\n\
#include <stdbool.h>\n\
\n\
#include \"%s\"\n\
\n\
/** \\file\n\
 * \n\
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。\n\n\
 */\n\
\n\
typedef struct _%s_Item {\n\
  %s *content;\n\
  bool is_used;\n\
} %s_Item;\n\
\n\
typedef struct {\n\
  %s_Item *pool;\n\
  %s *item;\n\
  uint8_t max_size;\n\
} %s;\n\
\n\
%s* %s_start(uint8_t max_size);\n\
void %s_stop(%s *access);\n\
%s* %s_malloc(%s *access);\n\
#endif";
  int counter = snprintf(NULL, 0, format,
      name,
      name,
      struct_header_file_name,
      struct_name, struct_name, struct_name,
      struct_name, struct_name, name,
      name, name,
      name, name,
      struct_name, name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name,
      name,
      struct_header_file_name,
      struct_name, struct_name, struct_name,
      struct_name, struct_name, name,
      name, name,
      name, name,
      struct_name, name, name);

  fprintf(file, result);
}


void gen_function_include(void) {
  char *format= "\
#include \"%s\"\
\n\
/** \\file\n\
 * \n\
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。\n\
 */\n\n";

  int counter = snprintf(NULL, 0, format,
      include_header_file_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      include_header_file_name);

  fprintf(file, result);
}


void gen_function_start(void) {
  char *format = "\
%s* %s_start(uint8_t max_size) {\n\
  %s *result = calloc(1, sizeof(%s));\n\
  %s *instance = calloc(max_size, sizeof(%s));\n\
  %s_Item *pool = calloc(max_size, sizeof(%s_Item));\n\
\n\
  %s *content = NULL;\n\
  %s_Item *item = NULL;\n\
\n\
  result->pool = pool;\n\
  result->item = instance;\n\
  result->max_size = max_size;\n\
\n\
  for (uint8_t index = 0; index < max_size; index++) {\n\
    content = &(result->item[index]);\n\
\n\
    item = &(result->pool[index]);\n\
    item->is_used = false;\n\
    item->content = content;\n\
  }\n\
  return result;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name,
      name, name,
      struct_name, struct_name,
      struct_name, struct_name,
      struct_name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name,
      name, name,
      struct_name, struct_name,
      struct_name, struct_name,
      struct_name, struct_name);

  fprintf(file, result);
}


void gen_function_stop(void) {
  char *format= "\
void %s_stop(%s *access) {\n\
  free(access->pool);\n\
  free(access->item);\n\
  free(access);\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_malloc(void) {
  char *format = "\
%s* %s_malloc(%s *access) {\n\
  %s *content = NULL;\n\
  %s_Item *item = NULL;\n\
  uint8_t max_size = access->max_size;\n\
\n\
  for (uint8_t index = 0; index < max_size; index++) {\n\
    item = &(access->pool[index]);\n\
\n\
    if (item->is_used == false) {\n\
      item->is_used = true;\n\
      content = item->content;\n\
      break;\n\
    }\n\
  }\n\
  return content;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      struct_name, name, name,
      struct_name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      struct_name, name, name,
      struct_name, struct_name);

  fprintf(file, result);
}


void gen_functions(void) {
  gen_function_include();
  gen_function_start();
  gen_function_stop();
  gen_function_malloc();
}


int main(int argc, char *argv[]) {
  config_t cfg;
  config_setting_t *setting;
  const char *str;

  config_init(&cfg);

  /* Read the file. If there is an error, report it and exit. */
  if(! config_read_file(&cfg, "tools/config/pool.cfg")) {
    fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
	    config_error_line(&cfg), config_error_text(&cfg));

    goto DONE;
  }

  /* find container pool */
  setting = config_lookup(&cfg, "pool");

  if (setting != NULL) {
    int total_counter = config_setting_length(setting);
    int counter;
    const char *c_value;

    /* setup npc status with pool */
    for (counter = 0; counter < total_counter; counter++) {
      config_setting_t *pool_setting = config_setting_get_elem(setting, counter);

      config_setting_lookup_string(pool_setting, "name", &name);
      config_setting_lookup_string(pool_setting, "header", &include_header_file_name);
      config_setting_lookup_string(pool_setting, "source_path", &source_file_name);
      config_setting_lookup_string(pool_setting, "header_path", &header_file_name);
      config_setting_lookup_string(pool_setting, "struct", &struct_name);
      config_setting_lookup_string(pool_setting, "struct_from_file", &struct_header_file_name);

      remove(header_file_name);
      file = fopen(header_file_name, "a");
      gen_header();
      fclose(file);

      remove(source_file_name);
      file = fopen(source_file_name, "a");
      gen_functions();
      fclose(file);
    }
  }
  else {
    goto DONE;
  }
DONE:
  config_destroy(&cfg);
}

