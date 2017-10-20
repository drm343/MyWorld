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
#ifndef HEADER_CONTAINER_LIST_%s\n\
#define HEADER_CONTAINER_LIST_%s\n\
\n\
#include <stdint.h>\n\
#include \"%s\"\n\
\n\
/** \\file\n\
 * \n\
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。\n\
 */\n\
\n\
typedef struct _%s_Node {\n\
    struct _%s_Node *next;\n\
    struct _%s_Node *previous;\n\
    %s *content;\n\
} %s_Node;\n\
\n\
typedef struct {\n\
    %s_Node *list;\n\
    uint8_t max_size;\n\
    uint8_t used;\n\
    uint8_t instance_counter;\n\
    %s_Node *first;\n\
    %s_Node *last;\n\
} %s;\n\
\n\
%s* %s_start(uint8_t max_size);\n\
void %s_stop(%s *access);\n\
void %s_gc(%s *access);\n\
uint8_t %s_insert(%s *access, %s *item);\n\
void %s_remove(%s *access, %s *item);\n\
%s* %s_get_by_index(%s *access, uint8_t index);\n\
void %s_copy_all(%s *from, %s *to);\n\
#endif";
  int counter = snprintf(NULL, 0, format,
      name,
      name,
      struct_header_file_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      name,
      name, name,
      name, name,
      name, name,
      name, name, struct_name,
      name, name, struct_name,
      struct_name, name, name,
      name, name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name,
      name,
      struct_header_file_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      struct_name,
      name,
      name, name,
      name, name,
      name, name,
      name, name, struct_name,
      name, name, struct_name,
      struct_name, name, name,
      name, name, name);

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
    %s_Node *list = calloc(max_size, sizeof(%s_Node));\n\
\n\
    result->list = list;\n\
    result->first = NULL;\n\
    result->last = NULL;\n\
    result->max_size = max_size;\n\
    result->used = 0;\n\
    result->instance_counter = 0;\n\
    return result;\n\
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
void %s_stop(%s *access) {\n\
    free(access->list);\n\
    free(access);\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name);

  fprintf(file, result);
}


void gen_function_gc(void) {
  char *format = "\
void %s_gc(%s *access) {\n\
    uint8_t used = access->used;\n\
    uint8_t instance_used = access->instance_counter;\n\
    %s_Node *unused_node = NULL;\n\
    %s_Node *current = access->first;\n\
    %s_Node *next = NULL;\n\
    %s_Node *previous = NULL;\n\
\n\
    while (current != NULL) {\n\
        next = current->next;\n\
        previous = current->previous;\n\
\n\
        for (uint8_t index = 0; index < used; index++) {\n\
            unused_node = &(access->list[index]);\n\
\n\
            if (unused_node == current) {\n\
                break;\n\
            }\n\
            else if (unused_node->content == NULL) {\n\
                unused_node->content = current->content;\n\
                unused_node->next = NULL;\n\
                unused_node->previous = NULL;\n\
                current->content = NULL;\n\
\n\
                if (current == access->first) {\n\
                    access->first = unused_node;\n\
                }\n\
\n\
                if (current == access->last) {\n\
                    access->last = unused_node;\n\
                }\n\
\n\
                if (previous != NULL) {\n\
                    previous->next = unused_node;\n\
                    current->previous = NULL;\n\
                    unused_node ->previous = previous;\n\
                }\n\
\n\
                if (next != NULL) {\n\
                    next->previous = unused_node;\n\
                    current->next = NULL;\n\
                    unused_node->next = next;\n\
                 }\n\
                 break;\n\
             }\n\
        }\n\
        current = next;\n\
    }\n\
    access->used = instance_used;\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name,
      struct_name, struct_name,
      struct_name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name,
      struct_name, struct_name,
      struct_name, struct_name);

  fprintf(file, result);
}


void gen_function_insert(void) {
  char *format = "\
uint8_t %s_insert(%s *access, %s *item) {\n\
    uint8_t used = access->used;\n\
    uint8_t max_size = access->max_size;\n\
\n\
    if (used >= max_size) {\n\
        %s_gc(access);\n\
        used = access->used;\n\
    }\n\
\n\
    if (used < max_size) {\n\
        %s_Node *first = access->first;\n\
        %s_Node *last = access->last;\n\
        %s_Node *node = &(access->list[used]);\n\
\n\
        node->previous = NULL;\n\
        node->next = NULL;\n\
        node->content = item;\n\
\n\
        if (first == NULL) {\n\
            access->first = node;\n\
        }\n\
\n\
        if (last == NULL) {\n\
            access->last = node;\n\
        }\n\
        else {\n\
            last->next = node;\n\
            node->previous = last;\n\
            access->last = node;\n\
        }\n\
        access->used = used + 1;\n\
        access->instance_counter = access->instance_counter + 1;\n\
        return used;\n\
    }\n\
    else {\n\
        return -1;\n\
    }\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name, struct_name,
      name,
      struct_name, struct_name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name, struct_name,
      name,
      struct_name, struct_name, struct_name);

  fprintf(file, result);
}


void gen_function_remove(void) {
  char *format = "\
void %s_remove(%s *access, %s *item) {\n\
    %s_Node *current = access->first;\n\
    %s_Node *next = NULL;\n\
    %s_Node *previous = NULL;\n\
    %s *current_item = NULL;\n\
\n\
    while (current != NULL) {\n\
        next = current->next;\n\
        previous = current->previous;\n\
        current_item = current->content;\n\
\n\
        if (current_item == item) {\n\
            next = current->next;\n\
            previous = current->previous;\n\
\n\
            if (next != NULL) {\n\
                next->previous = previous;\n\
            }\n\
\n\
            if (previous != NULL) {\n\
                previous->next = next;\n\
            }\n\
\n\
            current->content = NULL;\n\
            current->next = NULL;\n\
            current->previous = NULL;\n\
\n\
            if (current == access->first) {\n\
                access->first = next;\n\
            }\n\
            else if (current == access->last) {\n\
                access->last = previous;\n\
            }\n\
            access->instance_counter = access->instance_counter - 1;\n\
            return;\n\
        }\n\
        current = next;\n\
    }\n\
}\n\n";

  int counter = snprintf(NULL, 0, format,
      name, name, struct_name,
      struct_name, struct_name, struct_name, struct_name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name, struct_name,
      struct_name, struct_name, struct_name, struct_name);

  fprintf(file, result);
}


void gen_function_get_by_index(void) {
  char *format = "\
%s* %s_get_by_index(%s *access, uint8_t index) {\n\
    %s_Node *current = access->first;\n\
    %s_Node *next = NULL;\n\
\n\
    for (uint8_t counter = 0; counter < index; counter++) {\n\
        next = current->next;\n\
        current = next;\n\
    }\n\
    return current->content;\n\
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


void gen_function_copy_all(void) {
  char *format = "\
void %s_copy_all(%s *from, %s *to) {\n\
    uint8_t used = from->instance_counter;\n\
\n\
    %s_Node *current = from->first;\n\
    %s_Node *next = NULL;\n\
\n\
    for (uint8_t index = 0; index < used; index++) {\n\
        next = current->next;\n\
        %s_insert(to, current->content);\n\
        current = next;\n\
    }\n\
}";

  int counter = snprintf(NULL, 0, format,
      name, name, name,
      struct_name, struct_name, name);
  char result[counter];
  snprintf(result, counter + 1, format,
      name, name, name,
      struct_name, struct_name, name);

  fprintf(file, result);
}


void gen_functions(void) {
  gen_function_include();
  gen_function_start();
  gen_function_stop();
  gen_function_gc();
  gen_function_insert();
  gen_function_remove();
  gen_function_get_by_index();
  gen_function_copy_all();
}


int main(int argc, char *argv[]) {
  config_t cfg;
  config_setting_t *setting;
  const char *str;

  config_init(&cfg);

  /* Read the file. If there is an error, report it and exit. */
  if(! config_read_file(&cfg, "tools/config/list.cfg")) {
    fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
	    config_error_line(&cfg), config_error_text(&cfg));

    goto DONE;
  }

  /* find container list */
  setting = config_lookup(&cfg, "list");

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
      config_setting_lookup_string(list_setting, "struct_from_file", &struct_header_file_name);

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
