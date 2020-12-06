#include <stdio.h>

int main() {
  char c, prev;
  int answers[26] = { 0 };
  int i, sum;
  sum = 0;
  prev = -1;

  while ((c = getchar()) != EOF) {
    if (c == '\n' && prev == '\n') {
      for (i = 0; i < 26; ++i) {
        sum += answers[i];
        answers[i] = 0;
      }
    } else {
      answers[c - 'a'] = 1;
    }

    prev = c;
  }

  for (i = 0; i < 26; ++i) { sum += answers[i]; }

  printf("%d\n", sum);
}
