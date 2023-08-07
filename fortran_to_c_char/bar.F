program bar

   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
   use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_loc, c_null_char

   implicit none

   character :: a
   character(len=2), target :: b

   a = "R"
   b = "M"

   write (output_unit, *) "Fortran | char = '", a, "'"
   write (output_unit, *) "Fortran | char = '", b, "'"

   call foo_char_rubbish(a)
   call foo_pointer(b // c_null_char)

contains

   ! Passing a single character does not work as expected
   subroutine foo_char_rubbish(aa)
      character :: aa

      interface
         subroutine foo1_c(aaa) bind(C, name="foo1")
            import:: c_char
            character(c_char), value :: aaa
         end subroutine
      end interface

      call foo1_c(aa)
   end subroutine

   subroutine foo_pointer(aa)
      character(len=*), target :: aa

      interface
         subroutine foo2_c(aaa) bind(C, name="foo2")
            import :: c_ptr
            type(c_ptr), value :: aaa
         end subroutine
      end interface

      call foo2_c(c_loc(aa))
   end subroutine

end program
