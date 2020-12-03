#include <stdio.h>

int main(int argc, char *argv[])
{
  int c, idx, cursor, length, treesFound, right, down, line;

  if (argc == 1) {
    right = 3;
    down = 1;
  } else {
    sscanf(argv[1], "%d", &right);
    sscanf(argv[2], "%d", &down);
  }

  idx = treesFound = line = 0;
  length = -1;
  for (;;) {
    cursor = 0;
    while ((c = getchar()) != '\n') {
      if (c == EOF) { printf("%d\n", treesFound); return 0; }
      if (idx == cursor && ((line % down) == 0) && (c == '#')) { ++treesFound; }
      ++cursor;
    }

    length = cursor;
    if ((line % down) == 0) { idx = (idx + right) % length; }
    ++line;
  }
}
