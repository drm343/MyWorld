#include "stdio.h"

#include "strings.h"

int main(void) {
  char buffer[20];

  struct strings *repo = strings_new();
  uint32_t a = strings_intern(repo, "hello");
  uint32_t b = strings_intern(repo, "world");
  uint32_t c = strings_intern(repo, "hello");

  const char *hello = strings_lookup_id(repo, a);
  printf("%s\n", hello);
  hello = strings_lookup_id(repo, b);
  printf("%s\n", hello);
  sprintf(buffer, "%s %s", strings_lookup_id(repo, a), strings_lookup_id(repo, b));
  printf("%s\n", buffer);
  strings_free(repo);
}
