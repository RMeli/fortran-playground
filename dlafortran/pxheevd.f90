program pzheevd_test

    call pzheevd()

contains

module pxheevd_tests
   use iso_fortran_env, only: error_unit, sp=>real32, dp=>real64
   use dlafort, only: dlaf_initialize, dlaf_finalize, dlaf_create_grid, dlaf_free_grid
   use dlafort, only: dlaf_pssyevd
   use dlafort, only: dlaf_pcheevd
   use dlafort, only: dlaf_pdsyevd
   use dlafort, only: dlaf_pzheevd

   use testutils, only: allclose, terminate, setup_mpi, teardown_mpi, bcast_check, set_random_matrix, init_desc

   implicit none

   external blacs_pinfo
   external blacs_get
   external blacs_gridinit
   external blas_gridingo
   external blacs_gridexit
   external blacs_exit
   integer, external :: numroc

   subroutine pzheevd

      integer, parameter :: n = 10

      integer:: nprow, npcol
      integer:: i, j

      logical :: failed
      integer :: rank, numprocs, myrow, mycol
      integer :: ictxt, ictxt_0
      integer :: info, lld, nb, ma, na
      integer :: desca(9), desca_local_dlaf(9) !, desca_local_scalapack(9)
      integer :: descz_local_dlaf(9) !, descz_local_scalapack(9)
      integer :: descz_dlaf(9), descz_scalapack(9)
      complex(kind=dp), dimension(:,:), allocatable :: A, A_local_dlaf, A_local_scalapack
      complex(kind=dp), dimension(:,:), allocatable ::Z_local_dlaf!, Z_local_scalapack
      complex(kind=dp), dimension(:,:), allocatable :: Z_dlaf !, Z_scalapack
      real(kind=dp), dimension(:), allocatable :: W_dlaf!, W_scalapack
      real(kind=dp), parameter :: abstol =  1e-8_dp 

      nprow = 2
      npcol = 3
      nb = 2

      call setup_mpi(nprow, npcol, rank, numprocs)

      ! Setup BLACS
      call blacs_get(0, 0, ictxt)
      ictxt_0 = ictxt
      call blacs_gridinit(ictxt, 'R', nprow, npcol)
      call blacs_gridinit(ictxt_0, 'R', 1, 1)
      call blacs_pinfo(rank, numprocs)
      call blacs_gridinfo(ictxt, nprow, npcol, myrow, mycol)

      ! Setup full matrices on rank 0
      call init_desc(desca)
      call init_desc(descz_dlaf)
      if(rank == 0) then
         allocate(A(n, n))
         allocate(Z_dlaf(n, n))

         call descinit(desca, n, n, n, n, 0, 0, ictxt_0, n, info)
         call descinit(descz_dlaf, n, n, n, n, 0, 0, ictxt_0, n, info)

         call set_random_matrix(A)
      end if

      ! Allocate local matrices
      ma = numroc(n, nb, myrow, 0, nprow)
      na = numroc(n, nb, mycol, 0, npcol)
      lld = max(1, ma)
      allocate(A_local_dlaf(ma, na))!, A_local_scalapack(ma, na))
      allocate(Z_local_dlaf(ma, na))!, Z_local_scalapack(ma, na))
      allocate(W_dlaf(n))!, W_scalapack(n))

      call descinit(desca_local_dlaf, n, n, nb, nb, 0, 0, ictxt, lld, info)
      call descinit(descz_local_dlaf, n, n, nb, nb, 0, 0, ictxt, lld, info)
      call pzgemr2d(n, n, a, 1, 1, desca, A_local_dlaf, 1, 1, desca_local_dlaf, ictxt)

      ! Solve with DLAF
      call dlaf_initialize()
      call dlaf_create_grid(ictxt)
      call dlaf_pzheevd(&
         'L', &
         n, A_local_dlaf, 1, 1, desca_local_dlaf, &
         W_dlaf, Z_local_dlaf, 1, 1, descz_local_dlaf, &
         info &
         )
      call dlaf_free_grid(ictxt)
      call dlaf_finalize()
      if(info /= 0) then
         write(error_unit,*) 'ERROR: dlaf_pzheevd returned info = ', info
         call terminate(ictxt)
      end if

      call pzgemr2d(n, n, Z_local_dlaf, 1, 1, descz_local_dlaf, Z_dlaf, 1, 1, descz_dlaf, ictxt)

      if(rank == 0) then
         write(error_unit,*) "DLAF: ", W_dlaf
      end if

      if(rank == 0) then
         if(allocated(A)) deallocate(A)
         if(allocated(Z_dlaf)) deallocate(Z_dlaf)
      end if
      if(allocated(A_local_dlaf)) deallocate(A_local_dlaf)
      if(allocated(Z_local_dlaf)) deallocate(Z_local_dlaf)
      if(allocated(W_dlaf)) deallocate(W_dlaf)

      call blacs_gridexit(ictxt)
      call blacs_exit(1)
      call teardown_mpi()
   end subroutine pzheevd

end program pxheevd_test
