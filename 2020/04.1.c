#include <stdio.h>
#include <string.h>

int main() {
  char c;
  char field[4];
  signed char idx = 0;
  char prev = -1;
  char other = 0;
  int valid = 0;

  while ((c = getchar()) != EOF) {
    switch (c) {
    case ':':
      field[3] = '\0';
      if (strncmp("cid", field, 3) != 0) {
        ++other;
      }
      idx = -1;
      break;
    case ' ':
      idx = 0;
      break;
    case '\n':
      if (prev == '\n') {
        if (other == 7) { ++valid; };
        other = 0;
      }
      idx = 0;
      break;
    default:
      if (idx >= 0) {
        field[idx] = c;
        ++idx;
      }
    }
    prev = c;
  }

  if (other == 7) { ++valid; };
  printf("%d\n", valid);
}
