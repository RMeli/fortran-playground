cmake_minimum_required(VERSION 3.22)

project(DLAFortran LANGUAGES Fortran C CXX)

include(CMakeDependentOption)
include(GNUInstallDirs)

option(DLAFORT_WITH_MKL "Use MKL" OFF)
#option(DLAFORT_WITH_MPI_F08 "Use MPI F08 interface")


if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE STRING "Build Type" FORCE)
endif()

# if(CMAKE_BUILD_TYPE STREQUAL "Debug")
#   #set(DLAFORT_SANITIZER_FLAGS "-fno-omit-frame-pointer -fsanitize=address -fsanitize=leak -fsanitize=undefined")
# set(DLAFORT_SANITIZER_FLAGS "-fno-omit-frame-pointer -fsanitize=leak -fsanitize=undefined")
# set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${DLAFORT_SANITIZER_FLAGS}")
# set (CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} ${DLAFORT_SANITIZER_FLAGS}")
# set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} ${DLAFORT_SANITIZER_FLAGS}")
# set(CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} ${DLAFORT_SANITIZER_FLAGS}")
# endif()

if(NOT CMAKE_Fortran_MODULE_DIRECTORY)
    set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/ftnmods)
endif ()

find_package(DLAF REQUIRED)
find_package(MPI REQUIRED COMPONENTS Fortran)
if(DLAFORT_WITH_MKL)
  set(MKL_INTERFACE "lp64" CACHE STRING "")
  set(MKL_THREADING "sequential" CACHE STRING "")
  set(MKL_MPI "mpich" CACHE STRING "")
  find_package(MKL REQUIRED)
else()
  find_package(ScaLAPACK REQUIRED)
endif()

add_executable(dlaf dlaf.f90)
target_compile_options(dlaf PRIVATE "-Wall" "-Wpedantic" "-Wextra")
target_link_libraries(dlaf PRIVATE DLAF::DLAF MPI::MPI_Fortran)
if(DLAFORT_WITH_MKL)
	target_link_libraries(dlaf PRIVATE MKL::MKL)
endif()
