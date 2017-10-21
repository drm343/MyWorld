#ifndef HEADER_MAIN
#define HEADER_MAIN

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
#define CAMERA(name) Graphic_Camera_##name
#endif

/** \mainpage Create your own roguelike in C
 *
 * 本專案使用 C 撰寫，需要安裝 SDL2、SDL2-ttf、libconfig，如果你是 windows 的使用者，請考慮使用 "Bash on Ubuntu"。
 *
 * 下列工具為選擇性的，可根據需求選擇是否安裝。<br>
 * doxygen、graphviz、git、make、indent。
 *
 * @see 開發相關紀錄 https://drm343.github.io
 */
