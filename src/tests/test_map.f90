module mod_map_functions
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128

implicit none

public

contains

pure integer(kind=int8) function xpowx_i1(x) result(res)
  integer(kind=int8),intent(in) :: x
  res = x**x
endfunction xpowx_i1
  
pure integer(kind=int16) function xpowx_i2(x) result(res)
  integer(kind=int16),intent(in) :: x
  res = x**x
endfunction xpowx_i2
  
pure integer(kind=int32) function xpowx_i4(x) result(res)
  integer(kind=int32),intent(in) :: x
  res = x**x
endfunction xpowx_i4
  
pure integer(kind=int64) function xpowx_i8(x) result(res)
  integer(kind=int64),intent(in) :: x
  res = x**x
endfunction xpowx_i8
 
pure real(kind=real32) function xpowx_r4(x) result(res)
  real(kind=real32),intent(in) :: x
  res = x**x
endfunction xpowx_r4
 
pure real(kind=real64) function xpowx_r8(x) result(res)
  real(kind=real64),intent(in) :: x
  res = x**x
endfunction xpowx_r8
 
pure real(kind=real128) function xpowx_r16(x) result(res)
  real(kind=real128),intent(in) :: x
  res = x**x
endfunction xpowx_r16

pure complex(kind=real32) function xpowx_c4(x) result(res)
  complex(kind=real32),intent(in) :: x
  res = x**x
endfunction xpowx_c4

pure complex(kind=real64) function xpowx_c8(x) result(res)
  complex(kind=real64),intent(in) :: x
  res = x**x
endfunction xpowx_c8

pure complex(kind=real128) function xpowx_c16(x) result(res)
  complex(kind=real128),intent(in) :: x
  res = x**x
endfunction xpowx_c16
  
endmodule mod_map_functions

program test_map
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional
use mod_map_functions

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

complex(kind=real32),dimension(:),allocatable :: c4
complex(kind=real64),dimension(:),allocatable :: c8
complex(kind=real128),dimension(:),allocatable :: c16

n = 1
ntests = 10
call initialize_tests(tests,ntests)

tests(n) = assert(all(map(xpowx_i1,[1_int8,2_int8,3_int8])&
  == [1_int8,4_int8,27_int8]),'map, int8')
n = n + 1

tests(n) = assert(all(map(xpowx_i2,[1_int16,2_int16,3_int16])&
  == [1_int16,4_int16,27_int16]),'map, int16')
n = n + 1

tests(n) = assert(all(map(xpowx_i4,[1_int32,2_int32,3_int32])&
  == [1_int32,4_int32,27_int32]),'map, int32')
n = n + 1

tests(n) = assert(all(map(xpowx_i8,[1_int64,2_int64,3_int64])&
  == [1_int64,4_int64,27_int64]),'map, int64')
n = n + 1

tests(n) = assert(all(map(xpowx_r4,[1._real32,2._real32,3._real32])&
  == [1._real32,4._real32,27._real32]),'map, real32')
n = n + 1

tests(n) = assert(all(map(xpowx_r8,[1._real64,2._real64,3._real64])&
  == [1._real64,4._real64,27._real64]),'map, real64')
n = n + 1

tests(n) = assert(all(map(xpowx_r16,[1._real128,2._real128,3._real128])&
  == [1._real128,4._real128,27._real128]),'map, real128')
n = n + 1

c4 = [cmplx(1.,0.),cmplx(2.,0.),cmplx(3.,0.)]
c8 = [cmplx(1._real64,0._real64),&
      cmplx(2._real64,0._real64),&
      cmplx(3._real64,0._real64)]
c16 = [cmplx(1._real128,0._real128),&
       cmplx(2._real128,0._real128),&
       cmplx(3._real128,0._real128)]

tests(n) = assert(all(map(xpowx_c4,c4) == c4**c4),'map, complex real32')
n = n + 1

tests(n) = assert(all(map(xpowx_c8,c8) == c8**c8),'map, complex real64')
n = n + 1

tests(n) = assert(all(map(xpowx_c16,c16) == c16**c16),'map, complex real128')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_map

