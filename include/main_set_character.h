#define MAIN_NPC_STRING_malloc(len) string_pool.malloc(npc_string_pool, len);

Character_Pool_Access character_prepare_pool = NULL;
Character_Pool_Access character_use_pool = NULL;

String_Pool_Access npc_string_pool = NULL;

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

void set_name(Status_Access npc, const char *name) {
  size_t name_size = strlen(name);

  char *character_name = MAIN_STRING_malloc(name_size);
  strcpy(character_name, name);
  character.set_name(npc, character_name);
}

void set_mark(Status_Access npc, const char *mark) {
  size_t len  = strlen(mark);
  String key = MAIN_STRING_malloc(len);
  strcpy(key, mark);
  character.set_mark(npc, key);
}

void set_race(Status_Access npc, const char *race) {
  size_t race_size = strlen(race);

  char *character_race = MAIN_STRING_malloc(race_size);
  strcpy(character_race, race);

  character.set_race(npc, character_race);
  character.set_name(npc, character_race);
}

void set_relation(char *key, Status_Access npc, Relation_Type relation) {
  character.set_relation_enemy(npc);
}


Execute_Result setup_npc(char *file_path) {
  Execute_Result result = EXECUTE_FAILED;
  config_t cfg;
  config_setting_t *setting;
  const char *str;

  config_init(&cfg);

  /* Read the file. If there is an error, report it and exit. */
  if(! config_read_file(&cfg, file_path)) {
    fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
	    config_error_line(&cfg), config_error_text(&cfg));

    goto DONE;
  }

  /* find npc list */
  setting = config_lookup(&cfg, "npc");

  if (setting != NULL) {
    int total_counter = config_setting_length(setting);
    int counter;
    const char *value;

    /* setup npc status with list */
    for (counter = 0; counter < total_counter; counter++) {
      config_setting_t *npc_setting = config_setting_get_elem(setting, counter);

      Status_Access npc = character_pool.malloc(character_prepare_pool);
      Style_Access style_access = Style_Pool_Interface.malloc(style_pool);

      character.set_style(npc, style_access);

      config_setting_lookup_string(npc_setting, "name", &value);
      set_name(npc, value);

      config_setting_lookup_string(npc_setting, "mark", &value);
      set_mark(npc, value);

      config_setting_lookup_string(npc_setting, "race", &value);
      set_race(npc, value);
    }
  }
  else {
    goto DONE;
  }

  result = EXECUTE_SUCCESS;
  DONE:
  config_destroy(&cfg);
  return result;;
}


Status_Access use_npc(char *race, char *name, Map_Access map) {
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

    character.set_random_position(npc, map->end.x, map->end.y);
    character.set_random_relation(npc);
  }
  return npc;
}
