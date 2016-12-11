program test_insert
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 12
call initialize_tests(tests,ntests)

tests(n) = assert(all(insert(2_int8,2,[1_int8,3_int8]) == [1,2,3]),&
                  'insert, int8')
n = n + 1

tests(n) = assert(all(insert(2_int16,2,[1_int16,3_int16]) == [1,2,3]),&
                  'insert, int16')
n = n + 1

tests(n) = assert(all(insert(2_int32,2,[1_int32,3_int32]) == [1,2,3]),&
                  'insert, int32')
n = n + 1

tests(n) = assert(all(insert(2_int64,2,[1_int64,3_int64]) == [1,2,3]),&
                  'insert, int64')
n = n + 1

tests(n) = assert(all(insert(2._real32,2,[1._real32,3._real32]) == [1,2,3]),&
                  'insert, real32')
n = n + 1

tests(n) = assert(all(insert(2._real64,2,[1._real64,3._real64]) == [1,2,3]),&
                  'insert, real64')
n = n + 1

tests(n) = assert(all(insert(2._real128,2,[1._real128,3._real128]) == [1,2,3]),&
                  'insert, real128')
n = n + 1

tests(n) = assert(all(insert(cmplx(2._real32,0._real32),2,&
                             [cmplx(1._real32,0._real32),cmplx(3._real32,0._real32)])&
                       == arange(cmplx(1._real32,0._real32),cmplx(3._real32,0._real32))),&
                  'insert, real32')
n = n + 1

tests(n) = assert(all(insert(cmplx(2._real64,0._real64),2,&
                             [cmplx(1._real64,0._real64),cmplx(3._real64,0._real64)])&
                       == arange(cmplx(1._real64,0._real64),cmplx(3._real64,0._real64))),&
                  'insert, real64')
n = n + 1

tests(n) = assert(all(insert(cmplx(2._real128,0._real128),2,&
                             [cmplx(1._real128,0._real128),cmplx(3._real128,0._real128)])&
                       == arange(cmplx(1._real128,0._real128),cmplx(3._real128,0._real128))),&
                  'insert, real128')
n = n + 1

tests(n) = assert(all(insert(1,1,arange(1,0)) == [1]),&
                  'insert into empty array')
n = n + 1

tests(n) = assert(all(insert(2,2,[1]) == [1,2]),&
                  'insert out of bounds')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_insert
