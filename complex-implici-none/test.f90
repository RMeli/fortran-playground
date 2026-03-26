subroutine test_correct()
    ! The correct test declares zdotc/cdotc as external routines
    ! and explicitly defines their types.
    ! implicit none ensures that no implicit types are used.
    implicit none
    complex*16 :: zdotc
    complex :: cdotc
    external :: zdotc, cdotc

    integer :: n, incx, incy
    complex*16 :: zx(1), zy(1)
    complex :: cx(1), cy(1)

    real*8 :: result_dble_8
    real :: result_dble_4

    n = 1
    incx = 1
    incy = 1

    result_dble_8 = dble(zdotc(n, zx, incx, zy, incy))
    result_dble_4 = real(cdotc(n, cx, incx, cy, incy))

    write(*,*) '=== CORRECT ==='
    write(*,*) 'DBLE(zdotc):', result_dble_8
    write(*,*) 'REAL(cdotc):', result_dble_4
    write(*,*) 'Expected   : 3.14'
end subroutine test_correct

subroutine test_buggy()
    ! no implicit none declaration
    ! the type of zdotc/cdotc is not explicitly declared,
    ! therefore it is inferred by the name (Fortran convention)
    ! zdotc is implicitly typed as REAL*4
    ! cdotc is implicitly typed as REAL*4

    integer :: n, incx, incy
    complex*16 :: zx(1), zy(1)
    complex :: cx(1), cy(1)

    real*8 :: result_dble_8
    real :: result_dble_4

    complex*16 :: complex_16
    complex :: complex_8

    n = 1
    incx = 1
    incy = 1

    result_dble_8 = dble(zdotc(n, zx, incx, zy, incy))
    result_dble_4 = real(cdotc(n, cx, incx, cy, incy))

    write(*,*) '=== BUGGY ==='
    write(*,*) 'DBLE(zdotc):', result_dble_8
    write(*,*) 'REAL(cdotc):', result_dble_4
    write(*,*) 'Expected   : 3.14'
end subroutine test_buggy

program test_implicit_typing
    implicit none

    call test_correct()
    write(*,*)
    call test_buggy()
end program test_implicit_typing
