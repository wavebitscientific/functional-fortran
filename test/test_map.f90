module mod_map_functions
use iso_fortran_env, only:int8, int16, int32, int64, real32, real64, real128

implicit none

public

contains

pure integer(int8) function xpowx_i1(x) result(res)
  integer(int8), intent(in) :: x
  res = x**x
end function xpowx_i1
  
pure integer(int16) function xpowx_i2(x) result(res)
  integer(int16), intent(in) :: x
  res = x**x
end function xpowx_i2
  
pure integer(int32) function xpowx_i4(x) result(res)
  integer(int32), intent(in) :: x
  res = x**x
end function xpowx_i4
  
pure integer(int64) function xpowx_i8(x) result(res)
  integer(int64), intent(in) :: x
  res = x**x
end function xpowx_i8
 
pure real(real32) function xpowx_r4(x) result(res)
  real(real32), intent(in) :: x
  res = x**x
end function xpowx_r4
 
pure real(real64) function xpowx_r8(x) result(res)
  real(real64), intent(in) :: x
  res = x**x
end function xpowx_r8
 
pure real(real128) function xpowx_r16(x) result(res)
  real(real128), intent(in) :: x
  res = x**x
end function xpowx_r16

pure complex(real32) function xpowx_c4(x) result(res)
  complex(real32), intent(in) :: x
  res = x**x
end function xpowx_c4

pure complex(real64) function xpowx_c8(x) result(res)
  complex(real64), intent(in) :: x
  res = x**x
end function xpowx_c8

pure complex(real128) function xpowx_c16(x) result(res)
  complex(real128), intent(in) :: x
  res = x**x
end function xpowx_c16
  
end module mod_map_functions

program test_map
use iso_fortran_env, only:int8, int16, int32, int64, real32, real64, real128, compiler_version, compiler_options
use testing, only:assert, initialize_tests, report_tests
use functional
use mod_map_functions

implicit none

logical, dimension(:), allocatable :: tests
logical :: test_failed
integer :: n, ntests

complex(real32), dimension(:), allocatable :: c4
complex(real64), dimension(:), allocatable :: c8
complex(real128), dimension(:), allocatable :: c16

character(len=100) :: s_compiler_version
logical :: compiler_has_O3
complex(real32), dimension(:), allocatable :: c4res
logical :: c4rescheck

n = 1
ntests = 10
call initialize_tests(tests, ntests)

tests(n) = assert(all(map(xpowx_i1, [1_int8, 2_int8, 3_int8])&
  == [1_int8, 4_int8, 27_int8]), 'map,  int8')
n = n + 1

tests(n) = assert(all(map(xpowx_i2, [1_int16, 2_int16, 3_int16])&
  == [1_int16, 4_int16, 27_int16]), 'map,  int16')
n = n + 1

tests(n) = assert(all(map(xpowx_i4, [1_int32, 2_int32, 3_int32])&
  == [1_int32, 4_int32, 27_int32]), 'map,  int32')
n = n + 1

tests(n) = assert(all(map(xpowx_i8, [1_int64, 2_int64, 3_int64])&
  == [1_int64, 4_int64, 27_int64]), 'map,  int64')
n = n + 1

tests(n) = assert(all(map(xpowx_r4, [1._real32, 2._real32, 3._real32])&
  == [1._real32, 4._real32, 27._real32]), 'map,  real32')
n = n + 1

tests(n) = assert(all(map(xpowx_r8, [1._real64, 2._real64, 3._real64])&
  == [1._real64, 4._real64, 27._real64]), 'map,  real64')
n = n + 1

tests(n) = assert(all(map(xpowx_r16, [1._real128, 2._real128, 3._real128])&
  == [1._real128, 4._real128, 27._real128]), 'map,  real128')
n = n + 1

c4 = [cmplx(1., 0.), cmplx(2., 0.), cmplx(3., 0.)]
c8 = [cmplx(1._real64, 0._real64), &
      cmplx(2._real64, 0._real64), &
      cmplx(3._real64, 0._real64)]
c16 = [cmplx(1._real128, 0._real128), &
       cmplx(2._real128, 0._real128), &
       cmplx(3._real128, 0._real128)]

! Special case for gfortran-10 with -O3 if detected
c4rescheck = all(map(xpowx_c4, c4) == c4**c4)  ! the default
s_compiler_version = compiler_version()  ! e.g., `GCC version {major}.{minor}.{patch}`
compiler_has_O3 = index(compiler_options(), '-O3') /= 0
if ( &
  s_compiler_version(1:3) == 'GCC' &
  .and. s_compiler_version(13:14) == '10' &
  .and. compiler_has_O3 &
) then
  print *, 'using special check for gfortran-10 -O3 for complex real32'
  ! Note: `x = map(xpowx_c4, c4) == c4**c4` is T T F (via `print *, x`) even though `map(xpowx_c4, c4) == c4**c4` is T T T
  c4res = map(xpowx_c4, c4)
  c4rescheck = all(c4res == c4**c4)  ! by assigning `c4res` first we are able to get T T T in the comparison
end if
tests(n) = assert(c4rescheck, 'map,  complex real32')
n = n + 1

tests(n) = assert(all(map(xpowx_c8, c8) == c8**c8), 'map,  complex real64')
n = n + 1

tests(n) = assert(all(map(xpowx_c16, c16) == c16**c16), 'map,  complex real128')
n = n + 1

test_failed = .false.
call report_tests(tests, test_failed)
if(test_failed)stop 1

end program test_map

