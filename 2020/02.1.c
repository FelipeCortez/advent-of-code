#include <stdio.h>

int main()
{
  int c, min, max, nChr, validPasswords;
  char chr;

  validPasswords = 0;
  while(scanf("%d-%d %c: ", &min, &max, &chr) != EOF) {
    nChr = 0;

    while ((c = getchar()) != '\n') {
      if (c == chr) { ++nChr; };
    }

    if (min <= nChr && nChr <= max) { ++validPasswords; }
  }

  printf("%d\n", validPasswords);
}
