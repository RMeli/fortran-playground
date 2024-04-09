cmake_minimum_required(VERSION 3.18)

project(fortran_2_c_complex LANGUAGES C CXX Fortran)

set(CMAKE_C_FLAGS "-std=c99 -Wall")
set(CMAKE_CXX_FLAGS "-std=c++17 -Wall")
set(CMAKE_Fortran_FLAGS "-std=f2008 -Wall")

include(FortranCInterface)
FortranCInterface_VERIFY()

add_executable(bar bar.f90)
target_sources(bar PRIVATE foo.c)
