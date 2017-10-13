Character_Pool *character_pool = NULL;

NSString *CHARACTER_CONFIG_PATH = NULL;


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

  CHARACTER_CONFIG_PATH = [NSString stringWithFormat: @"%s%s",
                        CONFIG_VALUE,
                        CONFIG_NAME];
}

void set_relation(char *key, Status_Access npc, Relation_Type relation) {
  character.set_relation_enemy(npc);
}


Execute_Result setup_npc(const char *file_path) {
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
    const char *c_value;
    NSString *value;

    /* setup npc status with list */
    for (counter = 0; counter < total_counter; counter++) {
      config_setting_t *npc_setting = config_setting_get_elem(setting, counter);

      Status_Access npc = [character_pool sign_in];
      Style_Access style_access = Style_Pool_Interface.malloc(style_pool);

      character.set_style(npc, style_access);

      config_setting_lookup_string(npc_setting, "name", &c_value);
      value = [NSString  stringWithUTF8String: c_value];
      character.set_name(npc, value);

      config_setting_lookup_string(npc_setting, "mark", &c_value);
      value = [NSString  stringWithUTF8String: c_value];
      character.set_mark(npc, value);

      config_setting_lookup_string(npc_setting, "race", &c_value);
      value = [NSString  stringWithUTF8String: c_value];
      character.set_race(npc, value);
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
