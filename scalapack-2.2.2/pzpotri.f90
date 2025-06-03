!
! Distributed Linear Algebra with Future (DLAF)
!
! Copyright (c) ETH Zurich
! All rights reserved.
!
! Please, refer to the LICENSE file in the root directory.
! SPDX-License-Identifier: BSD-3-Clause
!

module pzpotri_tests
   use iso_fortran_env, only: error_unit, sp => real32, dp => real64
   use mpi

   implicit none

   external blacs_pinfo
   external blacs_get
   external blacs_gridinit
   external blas_gridinfo
   external blacs_gridexit
   external blacs_exit
   integer, external :: numroc

   public :: pzpotri_test

contains
   subroutine terminate(ictxt)
      integer, intent(in), optional :: ictxt
      integer :: ierr

      if (present(ictxt)) call blacs_gridexit(ictxt)
      call mpi_finalize(ierr)
      stop - 1
   end subroutine terminate

   subroutine setup_mpi(nprow, npcol, rank, nprocs)
      integer, intent(in) :: nprow, npcol
      integer, intent(out) :: rank, nprocs
      integer:: ierr, threading_provided

      call mpi_init_thread(MPI_THREAD_MULTIPLE, threading_provided, ierr)

      if (threading_provided /= MPI_THREAD_MULTIPLE) then
         write (error_unit, *) 'ERROR: The MPI library does not support MPI_THREAD_MULTIPLE'
         call terminate()
      end if

      call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
      call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)

      if (nprocs /= nprow*npcol) then
         if (rank == 0) then
            write (error_unit, *) 'ERROR: The test suite needs to run with exactly ', nprow*npcol, ' processes'
            write (error_unit, *) 'ERROR: Got ', nprocs, ' processes'
         end if
         call terminate()
      end if
   end subroutine setup_mpi

   subroutine teardown_mpi()
      integer:: ierr

      call mpi_finalize(ierr)

   end subroutine teardown_mpi

   subroutine set_random_matrix_z(a, s)

      complex(kind=dp), dimension(:, :), intent(out) :: a

      real(kind=dp), dimension(:, :), allocatable :: rand
      integer :: n, i

      integer, allocatable:: seed(:)
      integer :: nseed
      integer, intent(in), optional :: s

      call random_seed(size=nseed)
      allocate (seed(nseed))
      if (present(s)) then
         seed(:) = s
      else
         seed(:) = 0
      end if
      call random_seed(put=seed)
      deallocate (seed)

      if (size(a, 1) /= size(a, 2)) then
         write (error_unit, *) 'ERROR: Matrix must be square'
         call terminate()
      end if

      n = size(a, 1)

      allocate (rand(n, n))

      call random_number(rand)
      a%re = rand

      call random_number(rand)
      a%im = rand

      deallocate (rand)

      ! Make hermitian
      a = a*conjg(transpose(a))

      ! Make positive definite
      do i = 1, n
         a(i, i) = a(i, i) + n
      end do

   end subroutine set_random_matrix_z

   subroutine init_desc(desc)
      integer, intent(out), dimension(9) :: desc

      desc(:) = 0
      desc(2) = -1
   end subroutine init_desc

   subroutine pzpotri_test

      integer, parameter :: n = 4

      integer :: j
      integer:: nprow, npcol
      logical :: failed
      integer :: rank, numprocs, myrow, mycol
      integer :: ictxt, ictxt_0
      integer :: info, lld, nb, ma, na
      integer :: desca(9), desca_local_scalapack(9)
      integer :: descr_scalapack(9)
      complex(kind=dp), dimension(:, :), allocatable :: A, A_local_scalapack
      complex(kind=dp), dimension(:, :), allocatable :: R_scalapack
      character :: uplo = 'L'

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
      call init_desc(descr_scalapack)
      if (rank == 0) then
         allocate (A(n, n))
         allocate (R_scalapack(n, n))

         call descinit(desca, n, n, n, n, 0, 0, ictxt_0, n, info)
         call descinit(descr_scalapack, n, n, n, n, 0, 0, ictxt_0, n, info)

         call set_random_matrix_z(A)
      end if

      ! Allocate local matrices
      ma = numroc(n, nb, myrow, 0, nprow)
      na = numroc(n, nb, mycol, 0, npcol)
      lld = max(1, ma)
      allocate (A_local_scalapack(ma, na))

      ! + --------- +
      ! | ScaLAPACK |
      ! + --------- +

      call descinit(desca_local_scalapack, n, n, nb, nb, 0, 0, ictxt, lld, info)
      call pzgemr2d(n, n, A, 1, 1, desca, A_local_scalapack, 1, 1, desca_local_scalapack, ictxt)

      ! ScaLAPACK
      call pzpotrf(uplo, n, A_local_scalapack, 1, 1, desca_local_scalapack, info)
      if (info /= 0) then
         write (error_unit, *) 'ERROR: pzpotrf returned info = ', info
         call terminate(ictxt)
      end if
      call pzpotri(uplo, n, A_local_scalapack, 1, 1, desca_local_scalapack, info)
      if (info /= 0) then
         write (error_unit, *) 'ERROR: pzpotri returned info = ', info
         call terminate(ictxt)
      end if

      call pzgemr2d(n, n, A_local_scalapack, 1, 1, desca_local_scalapack, R_scalapack, 1, 1, descr_scalapack, ictxt)

      ! Check results
      ! Results are checked only on rank 0

      failed = .false.
      if (rank == 0) then
         write (*, *) "A="
         do j = 1, n
            write (*, "(*('('sf6.2xspf6.2x'i)':x))") A(:, j)
         end do
         write (*, *) "ScaLAPACK="
         do j = 1, n
            write (*, "(*('('sf6.2xspf6.2x'i)':x))") R_scalapack(:, j)
         end do
         call zpotrf(uplo, n, A, n, info)
         call zpotri(uplo, n, A, n, info)
         write (*, *) "LAPACK="
         do j = 1, n
            write (*, "(*('('sf6.2xspf6.2x'i)':x))") A(:, j)
         end do
      end if

      if (rank == 0) then
         if (allocated(A)) deallocate (A)
         if (allocated(R_scalapack)) deallocate (R_scalapack)
      end if
      if (allocated(A_local_scalapack)) deallocate (A_local_scalapack)

      call blacs_gridexit(ictxt)
      call blacs_exit(1)
      call teardown_mpi()
   end subroutine pzpotri_test

end module pzpotri_tests

program test_pzpotri
   use pzpotri_tests, only: pzpotri_test

   implicit none

   call pzpotri_test()

end program test_pzpotri
