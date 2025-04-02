program bar

   use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, dp => real64

   implicit none

   real(kind=dp) :: wait_time, t_start, t_end

   wait_time = 3.0_dp

   call cpu_time(t_start)
   call busywait(wait_time)
   call cpu_time(t_end)

   write(output_unit, '(A, F0.9, A)') 'Waited for: ', wait_time, ' seconds'
   write(output_unit, '(A, F0.9, A)') 'Elapsed time: ', t_end - t_start, ' seconds'


contains

   subroutine busywait(wait_time)
      real(kind=dp), intent(in) :: wait_time
      
      real(kind=dp) :: t_start, t_now, elapsed_time

      elapsed_time = 0.0_dp

      call cpu_time(t_start)

      do while (elapsed_time < wait_time)
         call cpu_time(t_now)
         elapsed_time = t_now - t_start
      end do

   end subroutine

end program
