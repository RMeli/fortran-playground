# Reference ScaLAPACK: bug with 2.2.2

With `netlib-scalapack@2.2.2`, `pzpotri` returns a different results than LAPACK
(and DLA-Future).

The issue appears on the diagonal elements of the inverse matrix.

`netlib-scalapack@2.2.0` doesn't suffer of the same issue.

Bisection points to the following PR as the one introducing the problem:

* [scalapack/#51]

## Observations

* The problem presents itself only with `uplo='L'`
* The problem presents itself also with larger matrices and block sizes
* Reverting changes to the `pzlauu2.f` before [scalapack/#51] file circumvents the issue

[scalapack/#51]: https://github.com/Reference-ScaLAPACK/scalapack/pull/51
