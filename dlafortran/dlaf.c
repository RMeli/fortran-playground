#include <stdio.h>
#include <complex.h>

#include <mpi.h>

#include "dlaf_c/grid.h"
#include "dlaf_c/utils.h"
#include "dlaf_c/eigensolver/eigensolver.h"

int dlaf_create_grid_f2c(MPI_Fint* f_comm, int nprow, int npcol, char order){
  MPI_Comm c_comm = MPI_Comm_f2c(f_comm);

  printf(">>> DEBUG <<< ~~~ Calling DLAF_CREATE_GRID ~~~\n");
  return dlaf_create_grid(c_comm, nprow, npcol, order);
}

void dlaf_pzheevd_c(const char uplo, const int n, complex double* a, const int ia,
                                const int ja, const int desca[9], double* w, complex double* z,
                                const int iz, const int jz, const int descz[9], int* info){
  int context = desca[1];
  struct DLAF_descriptor desc = make_dlaf_descriptor(n, n, 1, 1, desca);

  printf(">>> DEBUG <<< ~~~ Calling DLAF_HERMITIAN_EIGENSOLVER_Z ~~~\n");

  dlaf_hermitian_eigensolver_z(context, uplo, a, desc, w, z, desc);
}
