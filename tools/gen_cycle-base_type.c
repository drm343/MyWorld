#include <stdio.h>
#include <libconfig.h>

const char *header_file_name = NULL;
const char *source_file_name = NULL;
const char *include_header_file_name = NULL;
const char *name = NULL;
const char *struct_name = NULL;
FILE *file = NULL;


void gen_header(void) {
  char *format = "\
#ifndef HEADER_CONTAINER_CYCLE_ARRAY_%s_WITH_BASE_TYPE\n\
#define HEADER_CONTAINER_CYCLE_ARRAY_%s_WITH_BASE_TYPE\n\
\n\
#include <stdint.h>\n\
#include <stdlib.h>\n\
#include <stdbool.h>\n\
\n\
/** \\file\n\
 * \n\
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。\n\
 */\n\
\n\
typedef struct {\n\
    %s *array;\n\
    uint8_t max_size;\n\
    uint8_t buffer;\n\
    uint8_t start;\n\
} %s;\n\
\n\
%s * %s_start(uint8_t max_size);\n\
void %s_stop(%s *self);\n\
%s * %s_insert(%s *self, %s item);\n\
uint8_t %s_index(%s *self);\n\
bool %s_next(%s *self, uint8_t *next);\n\
uint8_t %s_last(%s *self);\n\
bool %s_previous(%s *self, uint8_t *previous);\n\
%s %s_get_item(%s *self, uint8_t next);\n\
#endif";
  int counter = snprintf(NULL, 0, format,
      name, name,
      struct_name, name,
      name, name,
      name, name,
      struct_name, name, name, struct_name,
      name, name,
      name, name, //next
      name, name,
      name, name, //previous
      struct_name, name, name); //get_item
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name,
      struct_name, name,
      name, name,
      name, name,
      struct_name, name, name, struct_name,
      name, name,
      name, name, //next
      name, name,
      name, name, //previous
      struct_name, name, name); //get_item

  fprintf(file, result);
}


void gen_function_include(void) {
    printf("%s\n", include_header_file_name);
  char *format = "\
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
    %s *self = calloc(1, sizeof(%s));\n\
    %s *array = calloc(max_size + 1, sizeof(%s));\n\
\n\
    self->array = array;\n\
    self->max_size = max_size + 1;\n\
    self->buffer = 0;\n\
    self->start = 0;\n\
    return self;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name,
      name, name,
      struct_name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name,
      name, name,
      struct_name, struct_name);

  fprintf(file, result);
}


void gen_function_stop(void) {
  char *format= "\
void %s_stop(%s *self) {\n\
    free(self->array);\n\
    free(self);\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_insert(void) {
  char *format = "\
%s * %s_insert(%s *self, %s item)\n\
{\n\
    self->array[self->buffer] = item;\n\
    self->buffer = self->buffer + 1;\n\
\n\
    if (self->buffer >= self->max_size)\n\
    {\n\
        self->buffer = self->buffer - self->max_size;\n\
    }\n\
\n\
    if (self->buffer == self->start)\n\
    {\n\
        self->start = self->start + 1;\n\
        return &(self->array[self->buffer]);\n\
    }\n\
    return NULL;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      struct_name, name, name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      struct_name, name, name, struct_name);

  fprintf(file, result);
}


void gen_function_index(void) {
  char *format = "\
uint8_t %s_index(%s *self)\n\
{\n\
  return self->start;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_next(void) {
  char *format = "\
bool %s_next(%s *self, uint8_t *next)\n\
{\n\
    uint8_t counter = *next;\n\
    counter = counter + 1;\n\
\n\
    if (counter >= self->max_size)\n\
    {\n\
        counter = 0;\n\
    }\n\
\n\
    if (counter == self->buffer)\n\
    {\n\
        return false;\n\
    }\n\
    else\n\
    {\n\
        *next = counter;\n\
        return true;\n\
    }\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_last(void) {
  char *format = "\
uint8_t %s_last(%s  *self)\n\
{\n\
    uint8_t result = (self->buffer == 0) ? self->max_size - 1 : self->buffer - 1;\n\
    return result;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_previous(void) {
  char *format = "\
bool %s_previous(%s *self, uint8_t *previous)\n\
{\n\
    uint8_t counter = (*previous == 0) ? self->max_size - 1 : *previous - 1;\n\
    uint8_t end_value = (self->start == 0) ? self->max_size - 1 : self->start - 1;\n\
\n\
    if (counter == end_value)\n\
    {\n\
        return false;\n\
    }\n\
    else\n\
    {\n\
        *previous = counter;\n\
        return true;\n\
    }\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_get_item(void) {
  char *format = "\
%s %s_get_item(%s *self, uint8_t next)\n\
{\n\
    return self->array[next];\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      struct_name, name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      struct_name, name, name);

  fprintf(file, result);
}


void gen_functions(void) {
  gen_function_include();
  gen_function_start();
  gen_function_stop();
  gen_function_insert();
  gen_function_index();
  gen_function_next();
  gen_function_last();
  gen_function_previous();
  gen_function_get_item();
}


int main(int argc, char *argv[]) {
  config_t cfg;
  config_setting_t *setting;
  const char *str;

  config_init(&cfg);

  /* Read the file. If there is an error, report it and exit. */
  if(! config_read_file(&cfg, "tools/config/cycle_array_base_type.cfg")) {
    fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
	    config_error_line(&cfg), config_error_text(&cfg));

    goto DONE;
  }

  /* find container list */
  setting = config_lookup(&cfg, "array");

  if (setting != NULL) {
    int total_counter = config_setting_length(setting);
    int counter;
    const char *c_value;

    /* setup npc status with list */
    for (counter = 0; counter < total_counter; counter++) {
      config_setting_t *list_setting = config_setting_get_elem(setting, counter);

      config_setting_lookup_string(list_setting, "name", &name);
      config_setting_lookup_string(list_setting, "header", &include_header_file_name);
      config_setting_lookup_string(list_setting, "source_path", &source_file_name);
      config_setting_lookup_string(list_setting, "header_path", &header_file_name);
      config_setting_lookup_string(list_setting, "struct", &struct_name);

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