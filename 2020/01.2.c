#include <stdio.h>

int main()
{
  int numbers[500];
  int i, j, numbersCount;

  numbersCount = 0;
  while (scanf("%d", &numbers[numbersCount]) != EOF) {
    for (i = 0; i < numbersCount; ++i) {
      for (j = i + 1; j < numbersCount; ++j) {
        if (numbers[numbersCount] + numbers[i] + numbers[j] == 2020) {
          printf("%d\n", numbers[numbersCount] * numbers[i] * numbers[j]);
        }
      }
    }
    ++numbersCount;
  }

  return 0;
}
