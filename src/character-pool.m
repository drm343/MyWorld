#include "character-pool.h"

#include <time.h>


Message_Type Point_Type_over_there(Point_Type * self, Point_Type * other)
{
    Message_Type result = DO_NOTHING;

    int32_t diff_x = Point_Type_x(self) - Point_Type_x(other);
    int32_t diff_y = Point_Type_y(self) - Point_Type_y(other);
    int32_t abs_x = abs(diff_x);
    int32_t abs_y = abs(diff_y);

    if (abs_x >= abs_y) {
        if (diff_x >= 0) {
            result = LEFT;
        } else {
            result = RIGHT;
        }
    } else {
        if (diff_y >= 0) {
            result = TOP;
        } else {
            result = DOWN;
        }
    }
    return result;
}

Message_Type Point_Type_near_by(Point_Type * self, Point_Type * other)
{
    Message_Type result = DO_NOTHING;

    int32_t diff_x = Point_Type_x(self) - Point_Type_x(other);
    int32_t diff_y = Point_Type_y(self) - Point_Type_y(other);
    int32_t abs_x = abs(diff_x);
    int32_t abs_y = abs(diff_y);

    if (abs_x >= abs_y) {
        if (diff_x >= 2) {
            result = LEFT;
        } else if (diff_x <= -2) {
            result = RIGHT;
        } else {
            goto RANDOM_POSITION;
        }
    } else {
        if (diff_y >= 2) {
            result = TOP;
        } else if (diff_y <= -2) {
            result = DOWN;
        } else {
            goto RANDOM_POSITION;
        }
    }
    goto DONE;

  RANDOM_POSITION:
    switch (rand() % 4) {
        case 1:
            result = TOP;
            break;
        case 2:
            result = DOWN;
            break;
        case 3:
            result = LEFT;
            break;
        default:
            result = RIGHT;
            break;
    }

  DONE:
    return result;
}


static Status_Pool_Access status_pool_start(uint8_t size)
{
    Status_Access memory = calloc(size, sizeof(Status));
    Status_Pool_Access pool_access = calloc(1, sizeof(Status_Pool));

    pool_access->pool = memory;
    pool_access->max_size = size;
    pool_access->current_size = size;

    return pool_access;
}

static Character_Base_Pool_Access base_pool_start(uint8_t size)
{
    Character_Base_Access memory =
        calloc(size, sizeof(Character_Base_Type));
    Character_Base_Pool_Access pool_access =
        calloc(1, sizeof(Character_Base_Pool));

    pool_access->pool = memory;
    pool_access->max_size = size;
    pool_access->current_size = size;

    return pool_access;
}

static Character_Pool_Type_Access pool_start(uint8_t size)
{
    Status_Pool_Access status = status_pool_start(size);
    Character_Base_Pool_Access base = base_pool_start(size);

    Character_Pool_Type_Access pool_access =
        calloc(1, sizeof(Character_Pool_Type_Access));

    pool_access->status = status;
    pool_access->base = base;
    return pool_access;
}


static void status_pool_stop(Status_Pool_Access pool_access)
{
    free(pool_access->pool);
    free(pool_access);
}


static void base_pool_stop(Character_Base_Pool_Access pool_access)
{
    free(pool_access->pool);
    free(pool_access);
}


static void pool_stop(Character_Pool_Type_Access pool_access)
{
    status_pool_stop(pool_access->status);
    base_pool_stop(pool_access->base);
    free(pool_access);
}


static Status_Access status_pool_malloc(Status_Pool_Access pool_access)
{
    uint8_t start = pool_access->max_size - pool_access->current_size;
    Status_Access result = &(pool_access->pool[start]);

    pool_access->current_size -= 1;
    return result;
}

static Character_Base_Access base_pool_malloc(Character_Base_Pool_Access
                                              pool_access)
{
    uint8_t start = pool_access->max_size - pool_access->current_size;
    Character_Base_Access result = &(pool_access->pool[start]);

    pool_access->current_size -= 1;
    return result;
}


/*
static bool pool_copy(Character_Pool_Type_Access from, Character_Pool_Type_Access to,
    char *name) {
  uint8_t count = 0;
  uint8_t used = from->status->max_size - from->status->current_size;
  Status_Access from_status = NULL;
  Status_Access to_status = NULL;

  for (count; count < used; count++) {
    from_status = &(from->status->pool[count]);

    if ([from_status->base->name isEqualToString: name] == YES) {
      to_status = [Character_Pool malloc: to];
      character.copy(to_status, from_status);
      character.set_name(to_status, name);
      return true;
    }
  }
  return false;
}
*/


static bool pool_all_copy(Character_Pool_Type_Access from,
                          Character_Pool_Type_Access to)
{
    uint8_t count = 0;
    uint8_t used = from->status->max_size - from->status->current_size;
    Status_Access from_status = NULL;
    Status_Access to_status = NULL;

    for (count; count < used; count++) {
        from_status = &(from->status->pool[count]);

        to_status = Character_Pool_Type_malloc(to);
        character.copy(to_status, from_status);
    }
    return false;
}


static Found_Result pool_find(Character_Pool_Type_Access access,
                              Status_Access * npc, const char *race)
{
    uint8_t count = 0;
    uint8_t used = access->status->max_size - access->status->current_size;
    race = String_Repo_search(race);

    for (count; count < used; count++) {
        *npc = &(access->status->pool[count]);

        if ((*npc)->race == race) {
            return FOUND;
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}


static Found_Result pool_find_by_position(Character_Pool_Type_Access
                                          access, Status_Access * npc,
                                          Point_Access point)
{
    uint8_t count = 0;
    uint8_t used = access->status->max_size - access->status->current_size;

    for (count; count < used; count++) {
        *npc = &(access->status->pool[count]);

        Faction_Type faction = character.get_relation(*npc);
        Point_Access npc_position = character.get_position(*npc);

        if (Point_Type_eq(point, (*npc)->base->Real_Position)) {
            if ((*npc)->base->crossable == NO) {
                return FOUND;
            }
        }
    }
    *npc = NULL;
    return NOT_FOUND;
}


static void reset_graph_position(Character_Pool_Type_Access access,
                                 Rectangle_Access rectangle)
{
    Rectangle_Access_change(rectangle);
    Point_Access start_point = Rectangle_Access_top_left_point();
    Point_Access end_point = Rectangle_Access_down_right_point();

    Point_Access_change(start_point);
    int64_t x = Point_Access_x();
    int64_t y = Point_Access_y();

    Point_Access_change(end_point);
    int64_t max_x = Point_Access_x();
    int64_t max_y = Point_Access_y();

    uint8_t count = 0;
    uint8_t used = access->status->max_size - access->status->current_size;
    Status_Access npc = NULL;
    int64_t counter_x = 0;
    int64_t counter_y = 0;

    for (count; count < used; count++) {
        npc = &(access->status->pool[count]);

        counter_x = npc->base->Real_Position->x - x;
        counter_y = npc->base->Real_Position->y - y;

        if (((counter_x >= 0) || (counter_y >= 0)) &&
            ((counter_x < max_x) && (counter_y < max_y - 1))) {
            Point_Access_change(npc->base->Graph_Position);
            Point_Access_set_x(counter_x);
            Point_Access_set_y(counter_y);
        } else {
            Point_Access_change(npc->base->Graph_Position);
            Point_Access_set_x(-1);
            Point_Access_set_y(-1);
        }
    }
}


static Message_Type player_reaction(bool is_alive)
{
    Message_Type result = DO_NOTHING;
    SDL_Event event;

    if (SDL_PollEvent(&event)) {
        switch (event.type) {
            case SDL_QUIT:
                result = QUIT;
                break;
            case SDL_KEYDOWN:
                if (is_alive == YES) {
                    goto CHECK_KEYDOWN;
                } else {
                    result = QUIT;
                }
                break;
            default:
                break;
        }
    }
    goto DONE;

  CHECK_KEYDOWN:
    switch (event.key.keysym.sym) {
        case SDLK_UP:
            result = TOP;
            break;
        case SDLK_DOWN:
            result = DOWN;
            break;
        case SDLK_LEFT:
            result = LEFT;
            break;
        case SDLK_RIGHT:
            result = RIGHT;
            break;
        case SDLK_q:
            result = QUIT;
            break;
        default:
            break;
    }
  DONE:
    return result;
}


static Message_Type faction_neutral_reaction(Point_Access self,
                                             Point_Access player)
{
    return Point_Type_near_by(self, player);
}


static Message_Type npc_reaction(Status * self, Status_List * enemy_group)
{
    uint8_t used = enemy_group->instance_counter;

    if (used <= 0) {
        return DO_NOTHING;
    }

    uint8_t target_number = rand() % used;
    Status *target = Status_List_get_by_index(enemy_group, target_number);
    Point_Access target_position = character.get_position(target);
    Point_Access self_position = character.get_position(self);
    return Point_Type_over_there(self_position, target_position);
}


Status_Access Character_Pool_Type_malloc(Character_Pool_Type * access)
{
    Status_Access status = status_pool_malloc(access->status);
    Character_Base_Access base = base_pool_malloc(access->base);

    status->base = base;
    character.init(status);
    return status;
}
