program test_complement
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

tests(n) = assert(all(complement([1_int8,2_int8],[2_int8]) == [1]),&
                  'complement, int8')
n = n + 1

tests(n) = assert(all(complement([1_int16,2_int16],[2_int16]) == [1]),&
                  'complement, int16')
n = n + 1

tests(n) = assert(all(complement([1_int32,2_int32],[2_int32]) == [1]),&
                  'complement, int32')
n = n + 1

tests(n) = assert(all(complement([1_int64,2_int64],[2_int64]) == [1]),&
                  'complement, int64')
n = n + 1

tests(n) = assert(all(complement([1._real32,2._real32],[2._real32]) == [1]),&
                  'complement, real32')
n = n + 1

tests(n) = assert(all(complement([1._real64,2._real64],[2._real64]) == [1]),&
                  'complement, real64')
n = n + 1

tests(n) = assert(all(complement([1._real128,2._real128],[2._real128]) == [1]),&
                  'complement, real128')
n = n + 1

tests(n) = assert(all(complement([cmplx(1._real32,0._real32),&
                                  cmplx(2._real32,0._real32)],&
                                 [cmplx(2._real32,0._real32)])&
                  == [cmplx(1._real32,0._real32)]),'complement, complex real32')
n = n + 1

tests(n) = assert(all(complement([cmplx(1._real64,0._real64),&
                                  cmplx(2._real64,0._real64)],&
                                 [cmplx(2._real64,0._real64)])&
                  == [cmplx(1._real64,0._real64)]),'complement, complex real64')
n = n + 1

tests(n) = assert(all(complement([cmplx(1._real64,0._real64),&
                                  cmplx(2._real64,0._real64)],&
                                 [cmplx(2._real64,0._real64)])&
                  == [cmplx(1._real64,0._real64)]),'complement, complex real64')
n = n + 1

tests(n) = assert(all(complement([1,2],[2]) == ([1,2].complement.[2])),&
                  'complement operator, x.complement.y')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_complement
