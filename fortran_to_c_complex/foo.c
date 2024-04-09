#include <stdio.h>
#include <complex.h>

void foo(double complex *b, int n) {
  printf("C:\t");
  for (int i = 0; i < n; ++i) {
    printf("(%E, %E)\t", creal(b[i]), cimag(b[i]));
  }
  printf("\n");
}
