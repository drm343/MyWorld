#ifndef HEADER_CHARACTER_SKILL
#define HEADER_CHARACTER_SKILL

#include "character-status.h"
#include "helper_function.h"


typedef struct {
  String name;
  uint8_t efficacy;
  Status_Access owner;
  Use_Type status;
} Skill;
typedef Skill * Skill_Access;

GENERIC_POOL(Skill_Pool, Skill_Access);
typedef Skill_Pool * Skill_Pool_Access;


typedef struct {
  void (*set_name)(Skill_Access, String);
  void (*set_efficacy)(Skill_Access, uint8_t);
  void (*remove)(Skill_Access);
} Skill_API;

extern Skill_API skill;


typedef struct {
  Skill_Pool_Access (*start)(uint8_t);
  void (*stop)(Skill_Pool_Access);
  Skill_Access (*malloc)(Skill_Pool_Access, Status_Access);
  bool (*find)(Skill_Pool_Access, Status_Access, String, Skill_Access *);
} Skill_Pool_API;

extern Skill_Pool_API skill_pool;

#endif
