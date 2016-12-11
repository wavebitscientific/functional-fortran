module mod_filter_functions

implicit none

public

contains

pure logical function gt3lt5_i1(x) result(res)
  use iso_fortran_env,only:int8
  integer(kind=int8),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_i1
  
pure logical function gt3lt5_i2(x) result(res)
  use iso_fortran_env,only:int16
  integer(kind=int16),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_i2
  
pure logical function gt3lt5_i4(x) result(res)
  use iso_fortran_env,only:int32
  integer(kind=int32),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_i4
  
pure logical function gt3lt5_i8(x) result(res)
  use iso_fortran_env,only:int64
  integer(kind=int64),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_i8
  
pure logical function gt3lt5_r4(x) result(res)
  use iso_fortran_env,only:real32
  real(kind=real32),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_r4
  
pure logical function gt3lt5_r8(x) result(res)
  use iso_fortran_env,only:real64
  real(kind=real64),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_r8
  
pure logical function gt3lt5_r16(x) result(res)
  use iso_fortran_env,only:real128
  real(kind=real128),intent(in) :: x
  res = x > 3 .and. x < 5
endfunction gt3lt5_r16

pure logical function gt3lt5_c4(x) result(res)
  use iso_fortran_env,only:real32
  complex(kind=real32),intent(in) :: x
  res = real(x) > 3 .and. real(x) < 5
endfunction gt3lt5_c4

pure logical function gt3lt5_c8(x) result(res)
  use iso_fortran_env,only:real64
  complex(kind=real64),intent(in) :: x
  res = real(x) > 3 .and. real(x) < 5
endfunction gt3lt5_c8

pure logical function gt3lt5_c16(x) result(res)
  use iso_fortran_env,only:real128
  complex(kind=real128),intent(in) :: x
  res = real(x) > 3 .and. real(x) < 5
endfunction gt3lt5_c16
  
endmodule mod_filter_functions

program test_filter
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional
use mod_filter_functions

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

complex(kind=real64),dimension(:),allocatable :: c8
complex(kind=real128),dimension(:),allocatable :: c16

n = 1
ntests = 11
call initialize_tests(tests,ntests)

tests(n) = assert(all(filter(gt3lt5_i1,[3_int8,4_int8,5_int8]) == [4]),&
                  'filter, int8')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_i2,[3_int16,4_int16,5_int16]) == [4]),&
                  'filter, int16')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_i4,[3,4,5]) == [4]),&
                  'filter, int32')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_i8,[3_int64,4_int64,5_int64]) == [4]),&
                  'filter, int64')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_r4,[3.,4.,5.]) == [4]),&
                  'filter, real32')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_r8,[3._real64,4._real64,5._real64]) == [4]),&
                  'filter, real64')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_r16,[3._real128,4._real128,5._real128]) == [4]),&
                  'filter, real128')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_c4,&
  [cmplx(3.,0.),cmplx(4.,0.),cmplx(5.,0.)]) == [cmplx(4.,0.)]),&
  'filter, complex real32')
n = n + 1

! Need to assign to a variable first because cmplx() by default
! returns single-precision complex number which breaks the generic
! interface
c8 = [cmplx(3._real64,0._real64),&
      cmplx(4._real64,0._real64),&
      cmplx(5._real64,0._real64)]
c16 = [cmplx(3._real128,0._real128),&
       cmplx(4._real128,0._real128),&
       cmplx(5._real128,0._real128)]

tests(n) = assert(all(filter(gt3lt5_c8,c8) == [cmplx(4.,0.)]),&
                  'filter, complex real64')
n = n + 1

tests(n) = assert(all(filter(gt3lt5_c16,c16) == [cmplx(4.,0.)]),&
                  'filter, complex real128')
n = n + 1

tests(n) = assert(size(filter(gt3lt5_i4,[1,2,3,5,6])) == 0,&
                  'filter returns empty array')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_filter

