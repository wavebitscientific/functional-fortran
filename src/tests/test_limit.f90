program test_limit
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional,only:limit,arange

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 11
call initialize_tests(tests,ntests)

tests(n) = assert(limit(2_int8,1_int8,3_int8) == 2_int8,&
                  'limit, int8')
n = n + 1

tests(n) = assert(limit(2_int16,1_int16,3_int16) == 2_int16,&
                  'limit, int16')
n = n + 1

tests(n) = assert(limit(2_int32,1_int32,3_int32) == 2_int32,&
                  'limit, int32')
n = n + 1

tests(n) = assert(limit(2_int64,1_int64,3_int64) == 2_int64,&
                  'limit, int64')
n = n + 1

tests(n) = assert(limit(2._real32,1._real32,3._real32) == 2._real32,&
                  'limit, real32')
n = n + 1

tests(n) = assert(limit(2._real64,1._real64,3._real64) == 2._real64,&
                  'limit, real32')
n = n + 1

tests(n) = assert(limit(2._real128,1._real128,3._real128) == 2._real128,&
                  'limit, real128')
n = n + 1

tests(n) = assert(limit(cmplx(-0.5,1.5),cmplx(0,0),cmplx(1,1)) == cmplx(0,1),&
                  'limit, complex real32')
n = n + 1

tests(n) = assert(limit(cmplx(-0.5_real64,1.5_real64),cmplx(0._real64,0._real64),&
                        cmplx(1._real64,1._real64)) == cmplx(0._real64,1._real64),&
                  'limit, complex real64')
n = n + 1

tests(n) = assert(limit(cmplx(-0.5_real128,1.5_real128),cmplx(0._real128,0._real128),&
                        cmplx(1._real128,1._real128)) == cmplx(0._real128,1._real128),&
                  'limit, complex real128')
n = n + 1

tests(n) = assert(all(limit(arange(1,3),2,2) == [2,2,2]),&
                  'limit works on arrays')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_limit
