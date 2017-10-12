#define MAIN_STRING_malloc(len) string_pool.malloc(config_pool, len);

const char *CONF_PATH = NULL;

int WIDTH = 800;
int HEIGHT = 600;
char *FONT_FAMILY = NULL;
char *GAME_TITLE = NULL;


Style_Pool_Access style_pool = NULL;
String_Pool_Access config_pool = NULL;


char *use_value(const char *CONFIG_VALUE) {
  char *result = NULL;
  size_t len = strlen(CONFIG_VALUE);

  result = string_pool.malloc(config_pool, len);
  strcpy(result, CONFIG_VALUE);
  return result;
}

char *use_path(const char *CONFIG_NAME) {
  char *result = NULL;
  size_t path_len = strlen(CONF_PATH);
  size_t file_len = strlen(CONFIG_NAME);

  result = string_pool.malloc(config_pool, path_len + file_len);
  strcpy(result, CONF_PATH);
  strcat(result, CONFIG_NAME);
  return result;
}

void setup_window(config_setting_t **setting) {
  const char *font_family;
  const char *title;

  config_setting_lookup_int(*setting, "width", &WIDTH);
  config_setting_lookup_int(*setting, "height", &HEIGHT);

  config_setting_lookup_string(*setting, "font-family", &font_family);
  FONT_FAMILY = use_path(font_family);

  config_setting_lookup_string(*setting, "title", &title);
  GAME_TITLE = use_value(title);
}


// Setup player and dead mark for now.
void setup_mark(config_setting_t **setting) {
  const char *player;
  const char *dead;
  String key = NULL;
  String item = NULL;
  Style_Access style_access = NULL;
  size_t len;

  style_access = Style_Pool_Interface.malloc(style_pool);
  len = strlen("player");
  key = string_pool.malloc(config_pool, len);
  strcpy(key, "player");
  style_access->name = key;

  config_setting_lookup_string(*setting, "player", &player);
  len = strlen(player);
  item = string_pool.malloc(config_pool, len);
  strcpy(item, player);
  style_access->mark = item;

  style_access = Style_Pool_Interface.malloc(style_pool);
  len = strlen("dead");
  key = string_pool.malloc(config_pool, len);
  strcpy(key, "dead");
  style_access->name = key;

  config_setting_lookup_string(*setting, "dead", &dead);
  len = strlen(dead);
  item = string_pool.malloc(config_pool, len);
  strcpy(item, dead);
  style_access->mark = item;
}

// Default Execute_Result value is EXECUTE_FAILED.
// Only run success will change the variable.
Execute_Result setup_style(const char *file_path) {
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


  /* check and setup window */
  setting = config_lookup(&cfg, "window");

  if (setting != NULL) {
    setup_window(&setting);
  }
  else {
    goto DONE;
  }


  /* check and setup mark */
  setting = config_lookup(&cfg, "mark");

  if (setting != NULL) {
    setup_mark(&setting);
  }
  else {
    goto DONE;
  }

  result = EXECUTE_SUCCESS;
DONE:
  config_destroy(&cfg);
  return result;;
}
