#include <mpi.h>
#include "dlaf_c/grid.h"
#include <stdio.h>

int dlaf_create_grid_f2c(MPI_Fint* f_comm, int nprow, int npcol, char order){
  printf("!!! WTF !!!");
  MPI_Comm c_comm = MPI_Comm_f2c(f_comm);
  return dlaf_create_grid(c_comm, nprow, npcol, order);
}
