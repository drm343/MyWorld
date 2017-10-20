#ifndef HEADER_MAIN
#define HEADER_MAIN

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <unistd.h>
#include <libgen.h>

#include <libconfig.h>

#include "helper/strings.h"
#include "graphic-camera.h"


#define SP(name) Style_Pool_##name
#include "main_set_config.h"

#define MAP(name) Map_Type_##name
#define CP(name) Character_Pool_Access_##name
#define CP_SUPER(name) Character_Pool_##name
#endif
