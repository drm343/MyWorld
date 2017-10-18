#include "stdio.h"

#include "strings.h"

int main(void) {
  char buffer[20];
  const char *a_s = NULL;
  const char *b_s = NULL;

  struct strings *repo = strings_new();
  uint32_t a = strings_intern(repo, "hello");
  uint32_t b = strings_intern(repo, "world");
  uint32_t c = strings_intern(repo, "hello");
  printf("%d %d %d %d\n", a, b, c, strings_count(repo));

  const char *hello = strings_lookup_id(repo, a);
  printf("%s\n", hello);
  hello = strings_lookup_id(repo, b);
  printf("%s\n", hello);
  a_s = strings_lookup_id(repo, a);
  b_s = strings_lookup_id(repo, b);
  sprintf(buffer, "%s %s", a_s, b_s);
  printf("%s\n", buffer);
  strings_free(repo);
}
