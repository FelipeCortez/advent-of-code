#include <stdio.h>

int main() {
  char c, prev;
  int answers[26] = { 0 };
  int i, sum, peopleInGroup;
  sum = peopleInGroup = 0;
  prev = -1;

  while ((c = getchar()) != EOF) {
    if (c == '\n') {
      if (prev == '\n') {
        for (i = 0; i < 26; ++i) {
          if (answers[i] == peopleInGroup) { ;++sum; }
          answers[i] = 0;
        }
        peopleInGroup = 0;
      } else {
        ++peopleInGroup;
      }
    } else {
      ++answers[c - 'a'];
    }

    prev = c;
  }

  for (i = 0; i < 26; ++i) {
    if (answers[i] == peopleInGroup) { ++sum; }
  }

  printf("%d\n", sum);
}
