#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libconfig.h>

const char *header_file_name = NULL;
const char *source_file_name = NULL;
const char *include_header_file_name = NULL;
const char *struct_header_file_name = NULL;
const char *name = NULL;
const char *struct_name = NULL;
FILE *file = NULL;


void gen_header_struct(void) {
    char *format = "\
#ifndef HEADER_CONTAINER_TREE_%s\n\
#define HEADER_CONTAINER_TREE_%s\n\
\n\
#include <stdint.h>\n\
#include <stdlib.h>\n\
#include <stdbool.h>\n\
#include \"%s\"\n\
\n\
/** \\file\n\
 * \n\
 * 此結構與程式由 Container tools 自動產生，若非必要，請勿手動修改本檔案。\n\
 */\n\
\n\
typedef struct %s_Result {\n\
    %s *parent;\n\
    %s *child;\n\
} %s_Result;\n\
\n\
typedef struct %s_Node {\n\
    struct %s_Node *parent;\n\
    struct %s_Node *left;\n\
    struct %s_Node *right;\n\
    %s *content;\n\
} %s_Node;\n\
\n\
typedef struct %s {\n\
    %s_Node *pool;\n\
    %s_Node *root;\n\
    uint32_t max_size;\n\
    uint32_t used;\n\
} %s;\n\n";
    int counter = snprintf(NULL, 0, format,
        name, name,
        struct_header_file_name,
        name, struct_name, struct_name, name,
        name, name, name, name, struct_name, name,
        name, name, name, name);
    char result[counter];
    snprintf(result, counter + 1, format,
        name, name,
        struct_header_file_name,
        name, struct_name, struct_name, name,
        name, name, name, name, struct_name, name,
        name, name, name, name);

    fprintf(file, result);
}


void gen_header_functions(void) {
    char *format = "\
#define %s_destruct(PARENT, CHILD, RESULT) \\\n\
{\\\n\
%s_Result *DISCARD = RESULT;\\\n\
PARENT = DISCARD->parent;\\\n\
CHILD = DISCARD->child;\\\n\
free(DISCARD);\\\n\
}\n\
\n\
%s *%s_start(uint32_t max_size);\n\
void %s_stop(%s * self);\n\
%s_Result *%s_insert(%s * self, %s * item);\n\
\n\
#endif";
    int counter = snprintf(NULL, 0, format,
name, name,
name, name,
name, name, name, struct_name,
name, name);
    char result[counter];
    snprintf(result, counter + 1, format,
name, name,
name, name,
name, name, name, struct_name,
name, name);

    fprintf(file, result);
}


void gen_header(void) {
    gen_header_struct();
    gen_header_functions();
}


void gen_function_include(void) {
    char *format = "\
#include \"%s\"\
\n\
/**\n\
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
%s *%s_start(uint32_t max_size)\n\
{\n\
    %s *self = calloc(1, sizeof(%s));\n\
    %s_Node *pool = calloc(max_size, sizeof(%s));\n\
\n\
    self->root = NULL;\n\
    self->pool = pool;\n\
    self->max_size = max_size;\n\
    self->used = 0;\n\
    return self;\n\
}\n\n\
";

    int counter = snprintf(NULL, 0, format,
        name, name,
        name, name,
        name, name);
    char result[counter];
    snprintf(result, counter + 1, format,
        name, name,
        name, name,
        name, name);

    fprintf(file, result);
}


void gen_function_stop(void) {
  char *format = "\
void %s_stop(%s *self)\n\
{\n\
    free(self->pool);\n\
    free(self);\n\
}\n\n\
";

    int counter = snprintf(NULL, 0, format,
        name, name);
    char *result = calloc(counter + 1, sizeof(char));
    snprintf(result, counter + 1, format,
        name, name);

    fprintf(file, result);
    free(result);
}


void gen_function_insert(void) {
  char *format = "\
%s_Result *%s_insert(%s * self, %s * item)\n\
{\n\
    %s_Result *result = calloc(1, sizeof(%s_Result));\n\
    bool is_left = true;\n\
\n\
    %s_Node *root = self->root;\n\
    %s_Node *parent = NULL;\n\
    %s_Node *current = &(self->pool[self->used]);\n\
\n\
    current->parent = NULL;\n\
    current->left = NULL;\n\
    current->right = NULL;\n\
    current->content = item;\n\
    self->used = self->used + 1;\n\
\n\
    if (root == NULL) {\n\
        self->root = current;\n\
        result->parent = current->content;\n\
        result->child = NULL;\n\
        return result;\n\
    }\n\
\n\
  next:\n\
    if (root == NULL) {\n\
        current->parent = parent;\n\
\n\
        if (is_left) {\n\
            parent->left = current;\n\
        } else {\n\
            parent->right = current;\n\
        }\n\
        result->parent = parent->content;\n\
        result->child = current->content;\n\
        return result;\n\
    } else if (%s_compare_small(item, root->content)) {\n\
        is_left = true;\n\
        parent = root;\n\
        root = root->left;\n\
        goto next;\n\
    } else {\n\
        is_left = false;\n\
        parent = root;\n\
        root = root->right;\n\
        goto next;\n\
    }\n\
}\n\n";

    int counter = snprintf(NULL, 0, format,
        name, name, name, struct_name,
        name, name,
        name, name, name,
        struct_name);
    char result[counter];
    snprintf(result, counter + 1, format,
        name, name, name, struct_name,
        name, name,
        name, name, name,
        struct_name);

   fprintf(file, result);
}


void gen_functions(void) {
    gen_function_include();
    gen_function_start();
    gen_function_stop();
    gen_function_insert();
}


int main(int argc, char *argv[]) {
  config_t cfg;
  config_setting_t *setting;
  const char *str;

  config_init(&cfg);

  /* Read the file. If there is an error, report it and exit. */
  if(! config_read_file(&cfg, "tools/config/normal_tree.cfg")) {
    fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
	    config_error_line(&cfg), config_error_text(&cfg));

    goto DONE;
  }

  /* find container list */
  setting = config_lookup(&cfg, "tree");

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
