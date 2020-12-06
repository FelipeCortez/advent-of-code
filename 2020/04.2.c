#include <stdio.h>
#include <string.h>
#include <assert.h>

#define READING_FIELD 0
#define READING_VAL 1

int
validateYears(char *s, int from, int to) {
  int year = 0;
  if (sscanf(s, "%d", &year) == EOF) { return 0; }
  if (year >= from && year <= to) { return 1; }
  return 0;
}

int
validateHeight(char *s) {
  int height;
  char unit[3] = "--";
  if (sscanf(s, "%d%s", &height, unit) == EOF) { return 0; }
  if        (strncmp(unit, "cm", 2) == 0) {
    return (height >= 150 && height <= 193);
  } else if (strncmp(unit, "in", 2) == 0) {
    return (height >= 59 && height <= 76);
  }

  return 0;
}

int
validateHair(char *s) {
  int i;
  char value[10] = "---------";
  if (sscanf(s, "#%s", value) == EOF) { return 0; }
  for (i = 0; i < 10; ++i) {
    if (value[i] == '\0') { return 1; }
    if (!((value[i] >= 'a' && value[i] <= 'f') || (value[i] >= '0' && value[i] <= '9'))) { return 0; }
  }

  return 0;
}

int
validateEye(char *s) {
  unsigned int i;
  const char *validColors[] = { "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
  for (i = 0; i < sizeof(validColors) / sizeof(const char *); ++i) {
    if (strncmp(s, validColors[i], 3) == 0) { return 1; }
  }

  return 0;
}

int validatePid(char *s) {
  if (strlen(s) != 9) { return 0; }
  int i;
  for (i = 0; i < 9; ++i) {
    if (!(s[i] >= '0' && s[i] <= '9')) { return 0; };
  }
  return 1;
}

int main() {
  assert(validateYears("2000", 1990, 2005) == 1);
  assert(validateYears("2000", 2000, 2005) == 1);
  assert(validateYears("2000", 1990, 2000) == 1);
  assert(validateYears("2000", 1999, 1999) == 0);
  assert(validateYears("not a year", 1990, 2000) == 0);

  assert(validateHeight("60in") == 1);
  assert(validateHeight("190cm") == 1);
  assert(validateHeight("190in") == 0);
  assert(validateHeight("190") == 0);

  assert(validateHair("#123abc") == 1);
  assert(validateHair("#123abcz") == 0);
  assert(validateHair("123abc") == 0);

  assert(validateEye("brn") == 1);
  assert(validateEye("hzl") == 1);
  assert(validateEye("wat") == 0);

  assert(validatePid("000000001") == 1);
  assert(validatePid("093154719") == 1);
  assert(validatePid("0123456789") == 0);

  char c;
  char field[4];
  char value[30];
  signed char idx = 0;
  char prev = -1;
  char other = 0;
  int valid = 0;
  int state = READING_FIELD;

  while ((c = getchar()) != EOF) {
    switch (c) {
    case ':':
      field[3] = '\0';
      state = READING_VAL;
      idx = 0;
      break;
    case ' ':
    case '\n':
      value[idx] = '\0';

      if (prev == '\n') {
        if (other == 7) { ++valid; };
        other = 0;
      } else {
        if        (strncmp("byr", field, 3) == 0) {
          other += validateYears(value, 1920, 2002);
        } else if (strncmp("iyr", field, 3) == 0) {
          other += validateYears(value, 2010, 2020);
        } else if (strncmp("eyr", field, 3) == 0) {
          other += validateYears(value, 2020, 2030);
        } else if (strncmp("hgt", field, 3) == 0) {
          other += validateHeight(value);
        } else if (strncmp("hcl", field, 3) == 0) {
          other += validateHair(value);
        } else if (strncmp("ecl", field, 3) == 0) {
          other += validateEye(value);
        } else if (strncmp("pid", field, 3) == 0) {
          other += validatePid(value);
        }
      }

      idx = 0;
      state = READING_FIELD;
      break;
    default:
      if (state == READING_FIELD) {
        field[idx] = c;
      } else if (state == READING_VAL) {
        value[idx] = c;
      }
      ++idx;
    }
    prev = c;
  }

  if (other == 7) { ++valid; };
  printf("%d\n", valid);
}
