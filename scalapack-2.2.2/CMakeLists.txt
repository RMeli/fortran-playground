cmake_minimum_required(VERSION 3.22)

project(pzporti LANGUAGES Fortran C)

find_package(MPI REQUIRED COMPONENTS Fortran C)
find_package(scalapack CONFIG REQUIRED)

add_executable(pzpotri pzpotri.f90)
target_compile_options(pzpotri PRIVATE "-Wall" "-Wpedantic" "-Wextra")
target_link_libraries(pzpotri PRIVATE MPI::MPI_Fortran MPI::MPI_C scalapack)
