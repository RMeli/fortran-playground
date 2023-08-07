#include <stdio.h>

void foo1(char a) {
  printf("C\t| char = '%c'\n", a);
}

void foo2(char* a) {
  printf("C\t| char = '%c'\n", a[0]);
}
