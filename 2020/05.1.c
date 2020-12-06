#include <stdio.h>

int main() {
  char c;
  int seat, max, idx;
  seat = max = idx = 0;

  while ((c = getchar()) != EOF) {
    if (c != '\n') {
      seat |= ((c == 'B' || c == 'R')) << (9 - idx);
      ++idx;
    } else {
      if (seat > max) { max = seat; }
      seat = idx = 0;
    }
  }

  printf("%d\n", max);
}
