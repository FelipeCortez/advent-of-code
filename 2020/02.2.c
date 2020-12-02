#include <stdio.h>

int main()
{
  int c, pos1, pos2, chrIdx, validPasswords;
  char chr, found;

  validPasswords = 0;
  while(scanf("%d-%d %c: ", &pos1, &pos2, &chr) != EOF) {
    chrIdx = 1;

    found = 0;
    while ((c = getchar()) != '\n') {
      if (((pos1 == chrIdx) || (pos2 == chrIdx)) && (c == chr)) {
        ++found;
      }

      ++chrIdx;
    }

    if (found == 1) { ++validPasswords; }
  }

  printf("%d\n", validPasswords);
}
