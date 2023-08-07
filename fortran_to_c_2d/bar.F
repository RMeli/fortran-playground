program bar

   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, dp => real64
   use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr, c_loc

   implicit none

   integer, parameter :: n = 4

   real(kind=dp), dimension(:, :), allocatable :: a
   real(kind=dp), dimension(:, :), allocatable, target :: b

   allocate (a(2, 2))

   a(1, 1) = 1.1_dp
   a(1, 2) = 1.2_dp
   a(2, 1) = 2.1_dp
   a(2, 2) = 2.2_dp

   allocate (b(2, 2))
   b(:, :) = a(:, :)

   if (.not. is_contiguous(a)) then
      write (error_unit, *) "Array B not contiguous!"
      call abort()
   end if
   if (.not. is_contiguous(b)) then
      write (error_unit, *) "Array C not contiguous!"
      call abort()
   end if

   write (output_unit, *) "Fortran: ", a(1, 1), a(1, 2), a(2, 1), a(2, 2)

   call foo_array_1d(a, n)
   call foo_pointer_wrong(b, n)
   call foo_pointer(b, n)

   deallocate (a)
   deallocate (b)

contains

   subroutine foo_array_1d(aa, nn)
      real(kind=dp), dimension(:, :) :: aa
      integer :: nn

      interface
         subroutine foo_c(aaa, nnn) bind(C, name="foo")
            import:: c_int, c_double
            real(c_double), dimension(*) :: aaa
            integer(c_int), value :: nnn
         end subroutine
      end interface

      call foo_c(aa, nn)
   end subroutine

   subroutine foo_pointer_wrong(aa, nn)
      real(kind=dp), dimension(:, :), target :: aa
      integer :: nn

      interface
         subroutine foo_c(aaa, nnn) bind(C, name="foo")
            import :: c_int, c_ptr
            ! Pointer without VALUE attribute
            type(c_ptr) :: aaa
            integer(c_int), value :: nnn
         end subroutine
      end interface

      call foo_c(c_loc(aa(1, 1)), nn)
   end subroutine

   subroutine foo_pointer(aa, nn)
      real(kind=dp), dimension(:, :), target :: aa
      integer :: nn

      interface
         subroutine foo_c(aaa, nnn) bind(C, name="foo")
            import :: c_int, c_ptr
            ! Pointer with VALUE attribute
            type(c_ptr), value :: aaa
            integer(c_int), value :: nnn
         end subroutine
      end interface

      call foo_c(c_loc(aa(1, 1)), nn)
   end subroutine

end program
