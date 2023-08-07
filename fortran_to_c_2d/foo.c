#include <stdio.h>

void foo(double *b, int n) {
  printf("C:\t");
  for (int i = 0; i < n; ++i) {
    printf("%E\t", b[i]);
  }
  printf("\n");
}
