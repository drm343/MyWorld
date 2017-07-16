#define MAIN_NPC_STRING_malloc(len) string_pool->malloc(npc_string_pool, len);

Character_Pool_Access character_prepare_pool = NULL;
Character_Pool_Access character_use_pool = NULL;
Status_Access Player = NULL;

STRING_POOL_ON_STACK(npc_string_pool);

char *CHARACTER_CONFIG_PATH = NULL;


const char *get_file_extension(const char *filename) {
  const char *dot = strrchr(filename, '.');

  if (!dot || dot == filename) {
    return "";
  }
  else {
    return dot + 1;
  }
}

bool check_json(char *file_path) {
  if (strcmp("json", get_file_extension(file_path)) == 0) {
    return true;
  }
  else {
    return false;
  }
}

void setup_character_config_path(char *CONFIG_VALUE) {
  char *CONFIG_NAME = "/config/npc/";
  size_t len1 = strlen(CONFIG_VALUE);
  size_t len2 = strlen(CONFIG_NAME);

  CHARACTER_CONFIG_PATH = MAIN_STRING_malloc(len1 + len2);
  strcpy(CHARACTER_CONFIG_PATH, CONFIG_VALUE);
  strcat(CHARACTER_CONFIG_PATH, CONFIG_NAME);
}

void set_name(Status_Access npc, char *name) {
  size_t name_size = strlen(name);

  char *character_name = MAIN_STRING_malloc(name_size);
  strcpy(character_name, name);
  character.set_name(npc, character_name);
}

void set_mark(Status_Access npc, char *mark) {
  size_t len  = strlen(mark);
  String key = MAIN_STRING_malloc(len);
  strcpy(key, mark);
  character.set_mark(npc, key);
}

void set_race(Status_Access npc, char *race) {
  size_t race_size = strlen(race);

  char *character_race = MAIN_STRING_malloc(race_size);
  strcpy(character_race, race);

  character.set_race(npc, character_race);
  character.set_name(npc, character_race);
}

void set_relation(char *key, Status_Access npc, Relation_Type relation) {
  character.set_relation_enemy(npc);
}

void eval_set_npc(mpc_ast_t* t) {
  mpc_ast_t* obj = t->children[1];
  Status_Access npc = character_pool.malloc(character_prepare_pool);
  Style_Access style_access = Style_Pool_Interface.malloc(style_pool);
  character.set_style(npc, style_access);

  for (int counter = 0; counter < obj->children_num; counter++) {
    mpc_ast_t* kv = obj->children[counter];

    if (strstr(kv->tag, "kv_item")) {
      mpc_ast_t* key = kv->children[0]->children[1];
      mpc_ast_t* value = kv->children[2];

      if (STRCMP(key->contents, "name")) {
        set_name(npc, value->children[1]->contents);
      }
      else if (strstr(key->contents, "mark")) {
        set_mark(npc, value->children[1]->contents);
      }
      else if (STRCMP(key->contents, "race")) {
        set_race(npc, value->children[1]->contents);
      }
      else {
        //printf("%s: %s\n", key->contents, value->tag);
      }
    }
  }
}

void setup_npc(char *file_path) {
  mpc_result_t r;

  mpc_parser_t* Number         = mpc_new("number");
  mpc_parser_t* Key            = mpc_new("key");
  mpc_parser_t* Json_String    = mpc_new("json_string");
  mpc_parser_t* Json_Boolean   = mpc_new("json_boolean");
  mpc_parser_t* Json_Value     = mpc_new("json_value");
  mpc_parser_t* KV_Item        = mpc_new("kv_item");
  mpc_parser_t* Json_Array     = mpc_new("json_array");
  mpc_parser_t* Json_Object    = mpc_new("json_obj");
  mpc_parser_t* JSON           = mpc_new("json");

  mpca_lang(MPCA_LANG_PREDICTIVE,
      " number         : /[0-9]+/                              ; \n"
      " key            : /[^'\"''{''}'':'';'' '',']*/          ; \n"
      " json_string    : '\"' <key> '\"'                       ; \n"
      " json_boolean   : \"true\" | \"false\"                  ; \n"
      " kv_item        : <json_string> ':'                         "
      "                   (<json_value> | <json_array>)        ; \n"
      " json_array     : '[' (<json_value> ','?)* ']'          ; \n"
      " json_obj       : '{' (<kv_item> ','?)* '}'             ; \n"
      " json_value     : <json_obj> | <json_array>                 "
      "                | <json_boolean>                            "
      "                | <json_string> | <number>              ; \n"
      " json           : /^/ <json_value> /$/                  ; \n",
      Number, Key,
      Json_String, Json_Boolean,
      KV_Item,
      Json_Array, Json_Object,
      Json_Value,
      JSON, NULL);

  if (mpc_parse_contents(file_path, JSON, &r)) {
    //mpc_ast_print(r.output);
    eval_set_npc(r.output);
    mpc_ast_delete(r.output);
  } else {
    mpc_err_print(r.error);
    mpc_err_delete(r.error);
  }

  mpc_cleanup(9,
      Number, Key,
      Json_String, Json_Boolean,
      KV_Item,
      Json_Array, Json_Object,
      Json_Value,
      JSON);
}

#ifdef LINUX_VERSION_FUNCTION
void setup_npc_by_dir() {
  DIR *d;
  struct dirent *dir;
  char *file_name = NULL;

  String_Pool_start_stack(npc_string_pool, 1000);
  size_t path_len = strlen(CHARACTER_CONFIG_PATH);

  d = opendir(CHARACTER_CONFIG_PATH);
  if (d) {
    while ((dir = readdir(d)) != NULL) {
      if (check_json(dir->d_name)) {
        size_t filename_len = strlen(dir->d_name);

        file_name = MAIN_NPC_STRING_malloc(path_len + filename_len);
        strcpy(file_name, CHARACTER_CONFIG_PATH);
        strcat(file_name, dir->d_name);

        setup_npc(file_name);
      }
    }

    npc_string_pool->stop(npc_string_pool);
    closedir(d);
  }
}
#endif

Status_Access use_npc(char *race, char *name) {
  Status_Access origin_npc = NULL;
  Status_Access npc = NULL;

  bool result = character_pool.find(character_prepare_pool, &origin_npc, race);

  if(result) {
    npc = character_pool.malloc(character_use_pool);
    character.copy(npc, origin_npc);

    size_t name_size = strlen(name);
    char *character_name = MAIN_STRING_malloc(name_size);
    strcpy(character_name, name);
    character.set_name(npc, character_name);
  }
  return npc;
}
