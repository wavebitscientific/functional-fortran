program test_arange
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional,only:arange

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 19
call initialize_tests(tests,ntests)

tests(n) = assert(all(arange(1_int8,3_int8) == [1_int8,2_int8,3_int8]),&
                  'arange, int8')
n = n + 1

tests(n) = assert(all(arange(1_int16,3_int16) == [1_int16,2_int16,3_int16]),&
                  'arange, int16')
n = n + 1

tests(n) = assert(all(arange(1_int32,3_int32) == [1_int32,2_int32,3_int32]),&
                  'arange, int32')
n = n + 1

tests(n) = assert(all(arange(1_int64,3_int64) == [1_int64,2_int64,3_int64]),&
                  'arange, int64')
n = n + 1

tests(n) = assert(all(arange(1._real32,3._real32) == [1._real32,2._real32,3._real32]),&
                  'arange, real32')
n = n + 1

tests(n) = assert(all(arange(1._real64,3._real64) == [1._real64,2._real64,3._real64]),&
                  'arange, real32')
n = n + 1

tests(n) = assert(all(arange(1._real128,3._real128) == [1._real128,2._real128,3._real128]),&
                  'arange, real128')
n = n + 1

tests(n) = assert(all(arange(1._real128,3._real128) == [1._real128,2._real128,3._real128]),&
                  'arange, real128')
n = n + 1

tests(n) = assert(all(arange(cmplx(1._real32,0._real32),&
                             cmplx(3._real32,0._real32),&
                             cmplx(1._real32,0._real32))&
                  == [cmplx(1._real32,0._real32),&
                      cmplx(2._real32,0._real32),&
                      cmplx(3._real32,0._real32)]),&
                  'arange, complex real32')
n = n + 1

tests(n) = assert(all(arange(cmplx(1._real64,0._real64),&
                             cmplx(3._real64,0._real64),&
                             cmplx(1._real64,0._real64))&
                  == [cmplx(1._real64,0._real64),&
                      cmplx(2._real64,0._real64),&
                      cmplx(3._real64,0._real64)]),&
                  'arange, complex real64')
n = n + 1

tests(n) = assert(all(arange(cmplx(1._real128,0._real128),&
                             cmplx(3._real128,0._real128),&
                             cmplx(1._real128,0._real128))&
                  == [cmplx(1._real128,0._real128),&
                      cmplx(2._real128,0._real128),&
                      cmplx(3._real128,0._real128)]),&
                  'arange, complex real128')
n = n + 1

tests(n) = assert(all(arange(cmplx(1,1),cmplx(3,5),cmplx(1,2))&
                  == [cmplx(1,1),cmplx(2,3),cmplx(3,5)]),&
                  'arange, incrementing both parts of complex numbers')
n = n + 1

tests(n) = assert(all(arange(1,10) == arange(1,10,1)),&
                  'arange increment equals 1 when ommited')
n = n + 1

tests(n) = assert(all(arange(1.,10.) == real(arange(1,10,1))),&
                  'integer and real arange variants produce same values')
n = n + 1

tests(n) = assert(all(arange(0.,2.4,0.8) == [0.,0.8,1.6,2.4]),&
                  'custom increment value')
n = n + 1

tests(n) = assert(all(arange(3,-1,-1) == [3,2,1,0,-1]),&
                  'negative increment value')
n = n + 1

tests(n) = assert(size(arange(1.0,1.4,0.1)) == 5,&
                  'real32-typed arange returns array of expected size')
n = n + 1

tests(n) = assert(size(arange(1.0_real64,1.4_real64,0.1_real64)) == 5,&
                  'real64-typed arange returns array of expected size')
n = n + 1

tests(n) = assert(size(arange(1.0_real128,1.4_real128,0.1_real128)) == 5,&
                  'real128-typed arange returns array of expected size')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_arange
