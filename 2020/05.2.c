#include <stdio.h>

int main() {
  char c;
  int seat, idx;
  seat = idx = 0;

  while ((c = getchar()) != EOF) {
    if (c != '\n') {
      seat |= ((c == 'B' || c == 'R')) << (9 - idx);
      ++idx;
    } else {
      printf("%d\n", seat);
      seat = idx = 0;
    }
  }
}
