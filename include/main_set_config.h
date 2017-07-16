#define MAIN_STRING_malloc(len) string_pool->malloc(string_pool, len);

char *CONF_PATH   = NULL;

int WIDTH = 800;
int HEIGHT = 600;
char *FONT_FAMILY = NULL;
char *GAME_TITLE = NULL;

Style_Pool_Access style_pool = NULL;
STRING_POOL_ON_STACK(string_pool);

char *use_value(char *CONFIG_VALUE) {
  char *result = NULL;
  size_t len = strlen(CONFIG_VALUE);

  result = MAIN_STRING_malloc(len);
  strcpy(result, CONFIG_VALUE);
  return result;
}

char *use_path(char *CONFIG_NAME) {
  char *result = NULL;
  size_t path_len = strlen(CONF_PATH);
  size_t file_len = strlen(CONFIG_NAME);

  result = MAIN_STRING_malloc(path_len + file_len);
  strcpy(result, CONF_PATH);
  strcat(result, CONFIG_NAME);
  return result;
}

void setup_window(mpc_ast_t* t) {
  if (strstr(t->tag, "attibute")) {
    if (STRCMP(t->children[0]->contents, "width")) {
      WIDTH = atoi(t->children[2]->children[0]->contents);
    }
    else if (STRCMP(t->children[0]->contents, "height")) {
      HEIGHT = atoi(t->children[2]->children[0]->contents);
    }
    else if (STRCMP(t->children[0]->contents, "font-family")) {
      FONT_FAMILY = use_path(t->children[2]->contents);
    }
    else if (STRCMP(t->children[0]->contents, "title")) {
      GAME_TITLE = use_value(t->children[2]->contents);
    }
  }
}

bool find_window(mpc_ast_t* t) {
  if (strstr(t->children[0]->tag, "element")) {
    if (STRCMP(t->children[0]->contents, "window")) {
      goto SETUP;
    }
  }
  return false;

SETUP:
  for (int counter = 1; counter <= t->children_num - 1; counter++) {
    setup_window(t->children[counter]);
  }
  return true;
}

void setup_mark(mpc_ast_t* t, Style_Access style_access) {
  if (strstr(t->tag, "mark_attibute")) {
    String item = t->children[2]->contents;
    size_t len  = strlen(item);

    String result = MAIN_STRING_malloc(len);
    strcpy(result, item);
    style_access->mark = result;
  }
}

bool find_mark(mpc_ast_t* t) {
  String key = NULL;
  Style_Access style_access = NULL;

  if (strstr(t->children[0]->tag, "id_selector")) {
    size_t key_len = strlen(t->children[0]->children[1]->contents);
    key = MAIN_STRING_malloc(key_len);
    strcpy(key, t->children[0]->children[1]->contents);

    style_access = Style_Pool_Interface.malloc(style_pool);
    style_access->name = key;
    goto SETUP;
  }
  return false;

SETUP:
  for (int counter = 1; counter <= t->children_num - 1; counter++) {
    setup_mark(t->children[counter], style_access);
  }
  return true;
}

void eval_set_config(mpc_ast_t* t) {
  if (strstr(t->tag, "css")) {
    find_window(t) || find_mark(t);
    return;
  }

  for (int counter = 1; counter <= t->children_num - 1; counter++) {
    eval_set_config(t->children[counter]);
  }
}

void setup_style(char *file_path) {
  mpc_result_t r;

  mpc_parser_t* Number         = mpc_new("number");
  mpc_parser_t* Element        = mpc_new("element");
  mpc_parser_t* Value          = mpc_new("value");
  mpc_parser_t* Key            = mpc_new("key");
  mpc_parser_t* ID_Selector    = mpc_new("id_selector");
  mpc_parser_t* Class_Selector = mpc_new("class_selector");
  mpc_parser_t* Size_Value     = mpc_new("size_value");
  mpc_parser_t* Size_Key       = mpc_new("size_key");
  mpc_parser_t* Windom_Key     = mpc_new("windom_key");
  mpc_parser_t* Size_Attibute  = mpc_new("size_attibute");
  mpc_parser_t* Windom_Attibute= mpc_new("windom_attibute");
  mpc_parser_t* Mark_Attibute  = mpc_new("mark_attibute");
  mpc_parser_t* Attibute       = mpc_new("attibute");
  mpc_parser_t* Selector       = mpc_new("selector");
  mpc_parser_t* CSS_NODE       = mpc_new("css_node");
  mpc_parser_t* CSS            = mpc_new("css");

  mpca_lang(MPCA_LANG_PREDICTIVE,
      " number         : /[1-9]?[0-9]*/                        ; \n"
      " element        : /[^'{''}'':'';'' ']*/                 ; \n"
      " value          : /[^'{''}'':'';'' ']*/                 ; \n"
      " key            : /[^'{''}'':'';'' ']*/                 ; \n"
      " id_selector    : '#' <element>                         ; \n"
      " class_selector : '.' <element>                         ; \n"
      " size_value     : <number> \"px\"                       ; \n"
      " size_key       : \"width\" | \"height\"                ; \n"
      " windom_key     : \"title\" | \"font-family\"           ; \n"
      " windom_attibute: <windom_key> ':' <element>     ';'    ; \n"
      " size_attibute  : <size_key> ':' <size_value>  ';'      ; \n"
      " mark_attibute  : <key> ':' <value> ';'                 ; \n"
      " attibute       : <size_attibute> | <windom_attibute>       "
      "                | <mark_attibute>                       ; \n"
      " selector       : <id_selector> | <class_selector>          "
      "                | <element>                             ; \n"
      " css_node       : <selector> '{' <attibute>* '}'        ; \n"
      " css            : /^/ <css_node>* /$/                   ; \n",
      Number, Element, Value, Key,
      ID_Selector, Class_Selector,
      Size_Value, Size_Key, Windom_Key, Size_Attibute,
      Windom_Attibute, Mark_Attibute, Attibute,
      Selector, CSS_NODE, CSS, NULL);

  if (mpc_parse_contents(file_path, CSS, &r)) {
    eval_set_config(r.output);
    mpc_ast_delete(r.output);
  } else {
    mpc_err_delete(r.error);
  }

  mpc_cleanup(16,
      Number, Element, Value, Key,         //4
      ID_Selector, Class_Selector,         //2
      Size_Value, Size_Key, Windom_Key, Size_Attibute, //4
      Windom_Attibute, Mark_Attibute, Attibute, //3
      Selector, CSS_NODE, CSS);            //3
}
