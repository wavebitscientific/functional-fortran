module mod_unfold_functions

implicit none

public

contains

pure integer(kind=int8) function addone_i1(x) result(res)
  use iso_fortran_env,only:int8
  integer(kind=int8),intent(in) :: x
  res = x+1
endfunction addone_i1

pure integer(kind=int16) function addone_i2(x) result(res)
  use iso_fortran_env,only:int16
  integer(kind=int16),intent(in) :: x
  res = x+1
endfunction addone_i2
  
pure integer(kind=int32) function addone_i4(x) result(res)
  use iso_fortran_env,only:int32
  integer(kind=int32),intent(in) :: x
  res = x+1
endfunction addone_i4
  
pure integer(kind=int64) function addone_i8(x) result(res)
  use iso_fortran_env,only:int64
  integer(kind=int64),intent(in) :: x
  res = x+1
endfunction addone_i8
  
pure real(kind=real32) function addone_r4(x) result(res)
  use iso_fortran_env,only:real32
  real(kind=real32),intent(in) :: x
  res = x+1
endfunction addone_r4
  
pure real(kind=real64) function addone_r8(x) result(res)
  use iso_fortran_env,only:real64
  real(kind=real64),intent(in) :: x
  res = x+1
endfunction addone_r8
  
pure real(kind=real128) function addone_r16(x) result(res)
  use iso_fortran_env,only:real128
  real(kind=real128),intent(in) :: x
  res = x+1
endfunction addone_r16

pure complex(kind=real32) function addone_c4(x) result(res)
  use iso_fortran_env,only:real32
  complex(kind=real32),intent(in) :: x
  res = x+1
endfunction addone_c4

pure complex(kind=real64) function addone_c8(x) result(res)
  use iso_fortran_env,only:real64
  complex(kind=real64),intent(in) :: x
  res = x+1
endfunction addone_c8

pure complex(kind=real128) function addone_c16(x) result(res)
  use iso_fortran_env,only:real128
  complex(kind=real128),intent(in) :: x
  res = x+1
endfunction addone_c16
  
endmodule mod_unfold_functions

program test_unfold
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional
use mod_unfold_functions

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

complex(kind=real64),dimension(:),allocatable :: c8
complex(kind=real128),dimension(:),allocatable :: c16

n = 1
ntests = 10
call initialize_tests(tests,ntests)

tests(n) = assert(all(unfold(addone_i1,[1_int8],3_int8) == [1,2,3]),&
                  'unfold, int8')
n = n + 1

tests(n) = assert(all(unfold(addone_i2,[1_int16],3_int16) == [1,2,3]),&
                  'unfold, int16')
n = n + 1

tests(n) = assert(all(unfold(addone_i4,[1_int32],3_int32) == [1,2,3]),&
                  'unfold, int32')
n = n + 1

tests(n) = assert(all(unfold(addone_i8,[1_int64],3_int64) == [1,2,3]),&
                  'unfold, int64')
n = n + 1

tests(n) = assert(all(unfold(addone_r4,[1._real32],3_int32) == [1,2,3]),&
                  'unfold, real32')
n = n + 1

tests(n) = assert(all(unfold(addone_r8,[1._real64],3_int32) == [1,2,3]),&
                  'unfold, real64')
n = n + 1

tests(n) = assert(all(unfold(addone_r16,[1._real128],3_int32) == [1,2,3]),&
                  'unfold, real128')
n = n + 1

tests(n) = assert(all(unfold(addone_c4,[cmplx(1._real32,0._real32)],3)& 
                       == arange(cmplx(1._real32,0._real32),&
                                 cmplx(3._real32,0._real32))),&
                  'unfold, complex real32')
n = n + 1

c8 = [cmplx(1._real64,0._real64)]
tests(n) = assert(all(unfold(addone_c8,c8,3)& 
                       == arange(cmplx(1._real64,0._real64),&
                                 cmplx(3._real64,0._real64))),&
                  'unfold, complex real64')
n = n + 1

c16 = [cmplx(1._real128,0._real128)]
tests(n) = assert(all(unfold(addone_c16,c16,3)& 
                       == arange(cmplx(1._real128,0._real128),&
                                 cmplx(3._real128,0._real128))),&
                  'unfold, complex real128')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_unfold
