program bar

   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, dp => real64
   use, intrinsic :: iso_c_binding, only: c_int, c_double, c_ptr, c_loc

   implicit none

   integer, parameter :: n = 4

   complex(kind=dp), dimension(:, :), allocatable, target :: a

   allocate (a(2, 2))

   a(1, 1) = (1.1_dp, -1.1_dp)
   a(1, 2) = (2.2_dp, -2.2_dp)
   a(2, 1) = (3.3_dp, -3.3_dp)
   a(2, 2) = (4.4_dp, -4.4_dp)

   write (output_unit, *) "Fortran: ", a(1, 1), a(1, 2), a(2, 1), a(2, 2)

   call foo_pointer(a, n)

   deallocate (a)

contains

   subroutine foo_pointer(aa, nn)
      complex(kind=dp), dimension(:, :), target :: aa
      integer :: nn

      interface
         subroutine foo_c(aaa, nnn) bind(C, name="foo")
            import :: c_int, c_ptr
            type(c_ptr), value :: aaa
            integer(c_int), value :: nnn
         end subroutine
      end interface

      call foo_c(c_loc(aa(1, 1)), nn)
   end subroutine

end program
