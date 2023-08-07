cmake_minimum_required(VERSION 3.18)

project(fortran_2_c_2d LANGUAGES Fortran C)

set(CMAKE_C_FLAGS "-std=c99")
set(CMAKE_Fortran_FLAGS
    "-mtune=native -ffree-line-length-512 -ffree-form -fimplicit-none -fallow-argument-mismatch -std=f2008"
)

include(FortranCInterface)
FortranCInterface_VERIFY()

add_executable(bar bar.F)
target_sources(bar PRIVATE foo.c)
