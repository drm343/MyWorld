const char *CONF_PATH = NULL;

int WIDTH = 800;
int HEIGHT = 600;

NSString *OBJC_FONT_FAMILY = NULL;
const char *FONT_FAMILY = NULL;
NSString *OBJC_GAME_TITLE = NULL;
const char *GAME_TITLE = NULL;


Style_Pool_Access style_pool = NULL;


void setup_window(config_setting_t **setting) {
  const char *font_family;
  const char *title;

  config_setting_lookup_int(*setting, "width", &WIDTH);
  config_setting_lookup_int(*setting, "height", &HEIGHT);

  config_setting_lookup_string(*setting, "font-family", &font_family);
  OBJC_FONT_FAMILY = [NSString stringWithFormat: @"%s%s", CONF_PATH, font_family];
  FONT_FAMILY = [OBJC_FONT_FAMILY UTF8String];

  config_setting_lookup_string(*setting, "title", &title);
  OBJC_GAME_TITLE = [NSString stringWithUTF8String: title];
  GAME_TITLE = [OBJC_GAME_TITLE UTF8String];
}


// Setup player and dead mark for now.
void setup_mark(config_setting_t **setting) {
  const char *player;
  const char *dead;
  NSString *key = NULL;
  NSString *item = NULL;
  Style_Access style_access = NULL;
  size_t len;

  style_access = Style_Pool_Interface.malloc(style_pool);
  style_access->name = @"player";

  config_setting_lookup_string(*setting, "player", &player);
  style_access->mark = [NSString stringWithUTF8String: player];

  style_access = Style_Pool_Interface.malloc(style_pool);
  style_access->name = @"dead";

  config_setting_lookup_string(*setting, "dead", &dead);
  style_access->mark = [NSString stringWithUTF8String: dead];
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
