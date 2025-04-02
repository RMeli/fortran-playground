cmake_minimum_required(VERSION 3.18)

project(busywait LANGUAGES Fortran)

set(CMAKE_Fortran_FLAGS "-std=f2008 -Wall")

add_executable(busywait busywait.f90)
