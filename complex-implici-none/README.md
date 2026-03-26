# ScaLAPACK 2.2.2 Bug

ScaLAPACK 2.2.2 is buggy, and produces wrong results for `pzpotri`.

The source of the issue is the following PR:
* https://github.com/Reference-ScaLAPACK/scalapack/pull/51/

ScaLAPACK 2.2.3 solves the issue with the following PR:
* https://github.com/Reference-ScaLAPACK/scalapack/pull/147

## Issue

The issue is that the PR introducing the bug did not declare the function type
explicitly, therefore resulting in inferred types because `implicit none` was missing.

The PR fixing the issue introduces `implicit none`,
and forces `ZDOTC` to be declared as `external` and `complex*16`.

The same issue is present with `CDOTC`, but it does not create problem becase
the return register is accidentally read correctly 
(the `real*4` implicit return type matches the real part of `complex*8` in the register).
