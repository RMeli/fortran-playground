# Reference ScaLAPACK: bug with 2.2.2

With `netlib-scalapack@2.2.2`, `pzpotri` returns a different results than LAPACK
(and DLA-Future).

The issue appears on the diagonal elements of the inverse matrix,
and it seems to be the first element of the block.

`netlib-scalapack@2.2.0` doesn't suffer of the same issue.

Bisection points to the following PR as the one introducing the problem:

* [scalapack/#51](https://github.com/Reference-ScaLAPACK/scalapack/pull/51)
