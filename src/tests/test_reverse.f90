program test_reverse
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 11
call initialize_tests(tests,ntests)

tests(n) = assert(all(reverse(arange(1_int8,3_int8)) == [3,2,1]),&
                  'reverse, int8')
n = n + 1

tests(n) = assert(all(reverse(arange(1_int16,3_int16)) == [3,2,1]),&
                  'reverse, int16')
n = n + 1

tests(n) = assert(all(reverse(arange(1_int32,3_int32)) == [3,2,1]),&
                  'reverse, int32')
n = n + 1

tests(n) = assert(all(reverse(arange(1_int64,3_int64)) == [3,2,1]),&
                  'reverse, int64')
n = n + 1

tests(n) = assert(all(reverse(arange(1._real32,3._real32)) == [3,2,1]),&
                  'reverse, real32')
n = n + 1

tests(n) = assert(all(reverse(arange(1._real64,3._real64)) == [3,2,1]),&
                  'reverse, real64')
n = n + 1

tests(n) = assert(all(reverse(arange(1._real128,3._real128)) == [3,2,1]),&
                  'reverse, real128')
n = n + 1

tests(n) = assert(all(reverse(arange(cmplx(1._real32,0._real32),&
                                     cmplx(3._real32,0._real32)))&
                       == arange(cmplx(3._real32,0._real32),&
                                 cmplx(1._real32,0._real32),&
                                 cmplx(-1._real32,0._real32))),& 
                  'reverse, complex real32')
n = n + 1

tests(n) = assert(all(reverse(arange(cmplx(1._real64,0._real64),&
                                     cmplx(3._real64,0._real64)))&
                       == arange(cmplx(3._real64,0._real64),&
                                 cmplx(1._real64,0._real64),&
                                 cmplx(-1._real64,0._real64))),& 
                  'reverse, complex real64')
n = n + 1

tests(n) = assert(all(reverse(arange(cmplx(1._real128,0._real128),&
                                     cmplx(3._real128,0._real128)))&
                       == arange(cmplx(3._real128,0._real128),&
                                 cmplx(1._real128,0._real128),&
                                 cmplx(-1._real128,0._real128))),& 
                  'reverse, complex real128')
n = n + 1

tests(n) = assert(all(reverse([1,2,3]) == .reverse.[1,2,3]),&
                  'reverse operator, .reverse.x')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_reverse
