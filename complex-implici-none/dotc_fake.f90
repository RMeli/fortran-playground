! The implicit type for both Z(dotc) and C(dotc) is real*4

! Fake implementation of zdotc
! Mimiks the correct signature
complex*16 function zdotc(n, zx, incx, zy, incy)
    implicit none
    integer :: n, incx, incy
    complex*16 :: zx(*), zy(*)
    zdotc = (3.14d0, 2.72d0)
end function zdotc

! Fake implementation of cdotc
! Mimiks the correct signature
complex function cdotc(n, cx, incx, cy, incy)
    implicit none
    integer :: n, incx, incy
    complex :: cx(*), cy(*)
    cdotc = (3.14, 2.72)
end function cdotc
