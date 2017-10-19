#ifndef HEADER_GRAPHIC
#define HEADER_GRAPHIC

#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>


#include "base_type.h"


typedef SDL_Texture *SDL_Texture_Access;

typedef struct {
    SDL_Color *color;
    TTF_Font *font;
    int8_t width;
    int8_t height;
} FontConfig;


typedef struct {
    const char *name;
    const char *mark;
    SDL_Texture_Access access;
} Style;
typedef Style *Style_Access;


typedef struct {
    Style_Access pool;
    uint8_t max_size;
    uint8_t current_size;
} Style_Pool;
typedef Style_Pool *Style_Pool_Access;


typedef struct {
    Style_Access(*malloc) (Style_Pool_Access);
    Style_Access(*find) (Style_Pool_Access, const char *);
    Style_Access(*next) (Style_Pool_Access, uint8_t *);
    void (*gc) (Style_Pool_Access);
     Style_Pool_Access(*start) (int);
    void (*stop) (Style_Pool_Access);
    void (*debug) (Style_Pool_Access);
} STYLE_POOL_INTERFACE;


extern STYLE_POOL_INTERFACE Style_Pool_Interface;
#endif
