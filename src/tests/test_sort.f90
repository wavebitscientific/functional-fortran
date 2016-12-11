program test_sort
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6
real(kind=real32),dimension(1000) :: x

n = 1
ntests = 12
call initialize_tests(tests,ntests)

tests(n) = assert(all(sort([3_int8,2_int8,1_int8]) == [1,2,3]),&
                  'sort, int8')
n = n + 1

tests(n) = assert(all(sort([3_int16,2_int16,1_int16]) == [1,2,3]),&
                  'sort, int16')
n = n + 1

tests(n) = assert(all(sort([3_int32,2_int32,1_int32]) == [1,2,3]),&
                  'sort, int32')
n = n + 1

tests(n) = assert(all(sort([3_int64,2_int64,1_int64]) == [1,2,3]),&
                  'sort, int8')
n = n + 1

tests(n) = assert(all(sort([3._real32,2._real32,1._real32]) == [1,2,3]),&
                  'sort, real32')
n = n + 1

tests(n) = assert(all(sort([3._real64,2._real64,1._real64]) == [1,2,3]),&
                  'sort, real64')
n = n + 1

tests(n) = assert(all(sort([3._real128,2._real128,1._real128]) == [1,2,3]),&
                  'sort, real128')
n = n + 1

tests(n) = assert(all(sort(arange(cmplx(3._real32,0._real32),&
                                  cmplx(1._real32,0._real32),&
                                  cmplx(-1._real32,0._real32)))& 
                        == arange(cmplx(1._real32,0._real32),&
                                  cmplx(3._real32,0._real32))),&
                  'sort, complex real32')
n = n + 1

tests(n) = assert(all(sort(arange(cmplx(3._real64,0._real64),&
                                  cmplx(1._real64,0._real64),&
                                  cmplx(-1._real64,0._real64)))&
                        == arange(cmplx(1._real64,0._real64),&
                                  cmplx(3._real64,0._real64))),&
                  'sort, complex real64')
n = n + 1

tests(n) = assert(all(sort(arange(cmplx(3._real128,0._real128),&
                                  cmplx(1._real128,0._real128),&
                                  cmplx(-1._real128,0._real128)))& 
                        == arange(cmplx(1._real128,0._real128),&
                                  cmplx(3._real128,0._real128))),&
                  'sort, complex real128')
n = n + 1

call random_number(x)
tests(n) = assert(all(tail(sort(x)) >= init(sort(x))),&
                  'all(tail(sort(x)) >= init(sort(x))')
n = n + 1

tests(n) = assert(all(sort(x) == .sort.x),&
                  'sort operator, .sort.x')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_sort
