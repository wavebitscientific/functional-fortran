program test_subscript
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

tests(n) = assert(all(subscript([1_int8,2_int8,3_int8],[2_int8]) == [2_int8]),&
                  'subscript, int8')
n = n + 1

tests(n) = assert(all(subscript([1_int16,2_int16,3_int16],[2_int16]) == [2_int16]),&
                  'subscript, int16')
n = n + 1

tests(n) = assert(all(subscript([1_int32,2_int32,3_int32],[2_int32]) == [2_int32]),&
                  'subscript, int32')
n = n + 1

tests(n) = assert(all(subscript([1_int64,2_int64,3_int64],[2_int64]) == [2_int64]),&
                  'subscript, int64')
n = n + 1

tests(n) = assert(all(subscript([1._real32,2._real32,3._real32],[2]) == [2._real32]),&
                 'subscript, real32')
n = n + 1

tests(n) = assert(all(subscript([1._real64,2._real64,3._real64],[2]) == [2._real64]),&
                  'subscript, real64')
n = n + 1

tests(n) = assert(all(subscript([1._real128,2._real128,3._real128],[2]) == [2._real128]),&
                  'subscript, real128')
n = n + 1

tests(n) = assert(size(subscript([1,2,3],[0])) == 0,&
                  'subscript out of bounds returns empty array')
n = n + 1

tests(n) = assert(all(subscript(arange(cmplx(1._real32,0._real32),&
                                       cmplx(3._real32,0._real32)),[2])&
                       == [cmplx(2._real32,0._real32)]),&
                  'subscript, complex real32')
n = n + 1

tests(n) = assert(all(subscript(arange(cmplx(1._real64,0._real64),&
                                       cmplx(3._real64,0._real64)),[2])&
                       == [cmplx(2._real64,0._real64)]),&
                  'subscript, complex real64')
n = n + 1

tests(n) = assert(all(subscript(arange(cmplx(1._real128,0._real128),&
                                       cmplx(3._real128,0._real128)),[2])&
                       == [cmplx(2._real128,0._real128)]),&
                  'subscript, complex real128')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_subscript
