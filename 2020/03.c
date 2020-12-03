#include <stdio.h>

#define SLOPE 3

int main(int argc, char *argv[])
{
  int c, idx, cursor, length, trees, right, down, line;

  if (argc == 1) {
    right = 3;
    down = 1;
  } else {
    sscanf(argv[1], "%d", &right);
    sscanf(argv[2], "%d", &down);
  }

  idx = trees = line = 0;
  length = -1;
  for (;;) {
    cursor = 0;
    while ((c = getchar()) != '\n') {
      if (c == EOF) { printf("%d\n", trees); return 0; }
      if (idx == cursor && ((line % down) == 0) && (c == '#')) { ++trees; }
      ++cursor;
    }

    length = cursor;
    if ((line % down) == 0) { idx = (idx + right) % length; }
    ++line;
  }
}
