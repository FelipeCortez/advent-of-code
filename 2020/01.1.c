#include <stdio.h>

int main()
{
  int numbers[500];
  int i, numbersCount;

  numbersCount = 0;
  while (scanf("%d", &numbers[numbersCount]) != EOF) {
    for (i = 0; i < numbersCount; ++i) {
      if (numbers[numbersCount] + numbers[i] == 2020) {
        printf("%d\n", numbers[numbersCount] * numbers[i]);
      }
    }
    ++numbersCount;
  }

  return 0;
}
