const char *CONF_PATH = NULL;

int WIDTH = 800;
int HEIGHT = 600;

const char *FONT_FAMILY = NULL;
const char *GAME_TITLE = NULL;


Style_Pool_Access style_pool = NULL;


void setup_window(config_setting_t ** setting)
{
    const char *tmp;

    config_setting_lookup_int(*setting, "width", &WIDTH);
    config_setting_lookup_int(*setting, "height", &HEIGHT);

    config_setting_lookup_string(*setting, "font-family", &tmp);
    int counter = snprintf(NULL, 0, "%s%s", CONF_PATH, tmp);
    char font_family[counter];
    snprintf(font_family, counter + 1, "%s%s", CONF_PATH, tmp);
    FONT_FAMILY = String_Repo_sign_in(font_family);

    config_setting_lookup_string(*setting, "title", &tmp);
    GAME_TITLE = String_Repo_sign_in(tmp);
}


// Setup player and dead mark for now.
void setup_mark(config_setting_t ** setting)
{
    int total_counter = config_setting_length(*setting);
    const char *value;

    for (int counter = 0; counter < total_counter; counter++) {
        config_setting_t *style_setting =
            config_setting_get_elem(*setting, counter);

        const char *key = NULL;

        Style_Access style_access = STYLE_P(malloc) (style_pool);
        config_setting_lookup_string(style_setting, "key", &key);
        style_access->name = String_Repo_sign_in(key);

        config_setting_lookup_string(style_setting, "mark", &key);
        style_access->mark = String_Repo_sign_in(key);

        int is_attackable = 0;
        config_setting_lookup_bool(style_setting, "attackable",
                                   &is_attackable);
        style_access->attackable = is_attackable;

        int is_crossable = 0;
        config_setting_lookup_bool(style_setting, "crossable",
                                   &is_crossable);
        style_access->crossable = is_crossable;
    }
}

// Default Execute_Result value is EXECUTE_FAILED.
// Only run success will change the variable.
Execute_Result setup_style(const char *file_path)
{
    Execute_Result result = EXECUTE_FAILED;
    config_t cfg;
    config_setting_t *setting;
    const char *str;

    config_init(&cfg);

    /* Read the file. If there is an error, report it and exit. */
    if (!config_read_file(&cfg, file_path)) {
        fprintf(stderr, "%s:%d - %s\n", config_error_file(&cfg),
                config_error_line(&cfg), config_error_text(&cfg));

        goto DONE;
    }


    /* check and setup window */
    setting = config_lookup(&cfg, "window");

    if (setting != NULL) {
        setup_window(&setting);
    } else {
        goto DONE;
    }


    /* check and setup mark */
    setting = config_lookup(&cfg, "style");

    if (setting != NULL) {
        setup_mark(&setting);
    } else {
        goto DONE;
    }

    result = EXECUTE_SUCCESS;
  DONE:
    config_destroy(&cfg);
    return result;;
}
