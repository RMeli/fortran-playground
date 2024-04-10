program pzheevd_test
    use iso_fortran_env, only: error_unit, sp=>real32, dp=>real64
    use iso_c_binding, only: &
      c_char,&
      c_double,&
      c_int,&
      c_loc,&
      c_ptr,&
      c_signed_char, &
      c_null_char
    use mpi
    implicit none

    external blacs_pinfo
    external blacs_get
    external blacs_gridinit
    external blas_gridingo
    external blacs_gridexit
    external blacs_exit
    integer, external :: numroc

    call pzheevd()

contains

   subroutine dlaf_initialize()
      integer, parameter :: dlaf_argc = 1, pika_argc = 1

      character(len=5, kind=c_char), allocatable, target :: dlaf_argv(:), pika_argv(:)
      type(c_ptr), allocatable, dimension(:) :: dlaf_argv_ptr, pika_argv_ptr

      interface
         subroutine dlaf_init(pika_argc_, pika_argv_, dlaf_argc_, dlaf_argv_) bind(C, name='dlaf_initialize')
            import :: c_ptr, c_int
            type(c_ptr), dimension(*) :: pika_argv_
            type(c_ptr), dimension(*) :: dlaf_argv_
            integer(kind=c_int), value :: pika_argc_
            integer(kind=c_int), value :: dlaf_argc_
         end subroutine dlaf_init
      end interface

      allocate(pika_argv(pika_argc))
      pika_argv(1) = "dlaf"//c_null_char
      allocate(dlaf_argv(dlaf_argc))
      dlaf_argv(1) = "dlaf"//c_null_char

      allocate(pika_argv_ptr(pika_argc))
      pika_argv_ptr(1) = c_loc(pika_argv(1))
      allocate (dlaf_argv_ptr(dlaf_argc))
      dlaf_argv_ptr(1) = c_loc(dlaf_argv(1))

      call dlaf_init(pika_argc, pika_argv_ptr, dlaf_argc, dlaf_argv_ptr)

   end subroutine dlaf_initialize

   subroutine dlaf_finalize()
      interface
         subroutine finalize_dlaf() bind(C, name='dlaf_finalize')
         end subroutine finalize_dlaf
      end interface

      call finalize_dlaf()

   end subroutine dlaf_finalize

      subroutine dlaf_create_grid(blacs_context)
      integer, intent(in) :: blacs_context

      interface
         subroutine create_grid_dlaf(blacs_contxt) bind(C, name='dlaf_create_grid_from_blacs')
            import :: c_int
            integer(kind=c_int), value :: blacs_contxt
         end subroutine
      end interface

      call create_grid_dlaf(blacs_context)

   end subroutine dlaf_create_grid

   subroutine dlaf_free_grid(blacs_context)
      integer, intent(in) :: blacs_context

      interface
         subroutine free_grid_dlaf(blacs_contxt) bind(C, name='dlaf_free_grid')
            import :: c_int
            integer(kind=c_int), value :: blacs_contxt
         end subroutine
      end interface

      call free_grid_dlaf(blacs_context)

   end subroutine dlaf_free_grid

   subroutine dlaf_pzheevd(uplo, n, a, ia, ja, desca, w, z, iz, jz, descz, info)
      character, intent(in) :: uplo
      integer, intent(in) :: n, ia, ja, iz, jz
      integer, dimension(9), intent(in) :: desca, descz
      integer, target, intent(out) :: info
      complex(kind=dp), dimension(:, :), target, intent(inout) :: a, z
      real(kind=dp), dimension(:), target, intent(out) :: w

      interface
         subroutine pzheevd_dlaf(uplo_, n_, a_, ia_, ja_, desca_, w_, z_, iz_, jz_, descz_, info_) &
            bind(C, name='dlaf_pzheevd')

            import :: c_int, c_ptr, c_signed_char

            integer(kind=c_signed_char), value :: uplo_
            integer(kind=c_int), value :: n_, ia_, ja_, iz_, jz_
            type(c_ptr), value :: a_, w_, z_
            integer(kind=c_int), dimension(9) :: desca_, descz_
            type(c_ptr), value :: info_
         end subroutine pzheevd_dlaf
      end interface

      info = -1

      call pzheevd_dlaf(iachar(uplo, c_signed_char), n, &
         c_loc(a(1, 1)), ia, ja, desca, &
         c_loc(w(1)), &
         c_loc(z(1, 1)), iz, jz, descz, &
         c_loc(info) &
         )

   end subroutine dlaf_pzheevd

   subroutine init_desc(desc)
         integer, intent(out), dimension(9) :: desc

         desc(:) = 0
         desc(2) = -1
   end subroutine init_desc

   subroutine set_random_matrix_z(a, s)

      complex(kind=dp), dimension(:,:), intent(out) :: a

      real(kind=dp), dimension(:,:), allocatable :: rand
      integer :: n, i

      integer, allocatable:: seed(:)
      integer :: nseed
      integer, intent(in), optional :: s

      call random_seed(size=nseed)
      allocate(seed(nseed))
      if(present(s)) then
          seed(:) = s
      else
          seed(:) = 0
      end if
      call random_seed(put=seed)
      deallocate(seed)

      if(size(a, 1) /= size(a, 2)) then
         write(error_unit, *) 'ERROR: Matrix must be square'
         stop -1
      end if

      n = size(a, 1)

      allocate(rand(n, n))

      call random_number(rand)
      a%re = rand

      call random_number(rand)
      a%im = rand

      deallocate(rand)

      ! Make hermitian
      a = a * conjg(transpose(a))

      ! Make positive definite
      do i = 1, n
         a(i, i) = a(i, i) + n
      end do


      end subroutine set_random_matrix_z

   subroutine pzheevd

      integer, parameter :: n = 4
      integer :: i, j
      integer :: ierr
      
      integer :: rank, nprocs
      integer:: nprow, npcol, myrow, mycol
      integer:: threading_provided

      integer :: ictxt, ictxt_0
      integer :: info, lld, nb, ma, na
      integer :: desca(9), desca_local_dlaf(9)
      integer :: descz_local_dlaf(9)
      integer :: descz_dlaf(9), descz_scalapack(9)
      complex(kind=dp), dimension(:,:), allocatable :: A, A_local_dlaf, A_local_scalapack
      complex(kind=dp), dimension(:,:), allocatable ::Z_local_dlaf
      complex(kind=dp), dimension(:,:), allocatable :: Z_dlaf
      real(kind=dp), dimension(:), allocatable :: W_dlaf

      nprow = 1
      npcol = 1
      nb = 2

      call mpi_init_thread(MPI_THREAD_MULTIPLE, threading_provided, ierr)

      if (threading_provided /= MPI_THREAD_MULTIPLE) then
         write(error_unit, *) 'ERROR: The MPI library does not support MPI_THREAD_MULTIPLE'
         stop -1
      end if

      call mpi_comm_rank(MPI_COMM_WORLD, rank, ierr)
      call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)

      if(nprocs /= nprow * npcol) then
         if(rank == 0) then
            write(error_unit,*) 'ERROR: The test suite needs to run with exactly ', nprow * npcol, ' processes'
            write(error_unit,*) 'ERROR: Got ', nprocs, ' processes'
         end if
         stop -1
      end if

      ! Setup BLACS
      call blacs_get(0, 0, ictxt)
      ictxt_0 = ictxt
      call blacs_gridinit(ictxt, 'R', nprow, npcol)
      call blacs_gridinit(ictxt_0, 'R', 1, 1)
      call blacs_pinfo(rank, nprocs)
      call blacs_gridinfo(ictxt, nprow, npcol, myrow, mycol)

      ! Setup full matrices on rank 0
      call init_desc(desca)
      call init_desc(descz_dlaf)
      if(rank == 0) then
         allocate(A(n, n))
         allocate(Z_dlaf(n, n))

         call descinit(desca, n, n, n, n, 0, 0, ictxt_0, n, info)
         call descinit(descz_dlaf, n, n, n, n, 0, 0, ictxt_0, n, info)

         call set_random_matrix_z(A)
         write(*,*) A
      end if

      ! Allocate local matrices
      ma = numroc(n, nb, myrow, 0, nprow)
      na = numroc(n, nb, mycol, 0, npcol)
      lld = max(1, ma)
      allocate(A_local_dlaf(ma, na))
      allocate(Z_local_dlaf(ma, na))
      allocate(W_dlaf(n))

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
      call mpi_finalize(ierr)

   end subroutine pzheevd

end program pzheevd_test
