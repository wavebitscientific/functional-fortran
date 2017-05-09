# cmake version, project name, language
cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(functional-fortran)
enable_language(Fortran)

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# if build type not specified, default to release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()

# compiler flags for gfortran
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# compiler flags for ifort
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume realloc_lhs -heap-arrays")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# compiler flags for Cray ftn
if(CMAKE_Fortran_COMPILER_ID MATCHES Cray)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -h noomp")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# library to archive (libfunctional.a)
add_library(functional src/lib/mod_functional.f90 src/lib/mod_interfaces.f90)
add_library(testing src/tests/mod_testing.f90)

# tests
enable_testing()
foreach(execid arange complement empty filter foldl foldr foldt head init insert intersection iterfold last limit map reverse set sort split subscript tail unfold union)
  add_executable(test_${execid} src/tests/test_${execid}.f90)
  target_link_libraries(test_${execid} functional testing)
  add_test(test_${execid} bin/test_${execid})
endforeach()

install(DIRECTORY "${CMAKE_Fortran_MODULE_DIRECTORY}" DESTINATION "${CMAKE_INSTALL_PREFIX}")
install(DIRECTORY "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}" DESTINATION "${CMAKE_INSTALL_PREFIX}")
