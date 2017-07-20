#ifndef HEADER_GRAPHIC_CAMERA
#define HEADER_GRAPHIC_CAMERA

#include "character-pool.h"
#include "map_system.h"
#include "graphic-message.h"


typedef enum {
  CAMERA_UNDEFINE,
  CAMERA_FIX,
  CAMERA_MOVE,
} Camera_Mode;


typedef struct {
  Status_Access player;
  Style_Access dead;
  Point_Type center;
  int64_t max_x; // 25
  int64_t max_y; // 21
  Point_Type start;
  Point_Type end;
  Camera_Mode horizon;
  Camera_Mode vertical;
  Map_Access map;
} Camera_Type;
typedef Camera_Type * Camera_Access;


typedef struct {
  Camera_Access (*start)(void);
  void (*stop)(Camera_Access);

  void (*set_max_x)(Camera_Access, int);
  void (*set_max_y)(Camera_Access, int);
  void (*set_dead_style)(Camera_Access, Style_Access);

  void (*set_player)(Camera_Access, Status_Access);
  void (*set_map)(Camera_Access, Map_Access);

  bool (*take)(Camera_Access, Character_Pool_Access, Message_Box_Access, SDL_Event *);
} Graphic_Camera_API;

extern Graphic_Camera_API camera;
#endif
