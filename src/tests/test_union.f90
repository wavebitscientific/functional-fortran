program test_union
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 14
call initialize_tests(tests,ntests)

tests(n) = assert(all(union([1_int8,2_int8],[2_int8,3_int8]) == [1,2,3]),&
                  'union, int8')
n = n + 1

tests(n) = assert(all(union([1_int16,2_int16],[2_int16,3_int16]) == [1,2,3]),&
                  'union, int16')
n = n + 1

tests(n) = assert(all(union([1_int32,2_int32],[2_int32,3_int32]) == [1,2,3]),&
                  'union, int32')
n = n + 1

tests(n) = assert(all(union([1_int64,2_int64],[2_int64,3_int64]) == [1,2,3]),&
                  'union, int64')
n = n + 1

tests(n) = assert(all(union([1._real32,2._real32],[2._real32,3._real32]) == [1,2,3]),&
                  'union, real32')
n = n + 1

tests(n) = assert(all(union([1._real64,2._real64],[2._real64,3._real64]) == [1,2,3]),&
                  'union, real64')
n = n + 1

tests(n) = assert(all(union([1._real128,2._real128],[2._real128,3._real128]) == [1,2,3]),&
                  'union, real128')
n = n + 1

tests(n) = assert(all(union([cmplx(1._real32,0._real32),cmplx(2._real32,0._real32)],&
                            [cmplx(2._real32,0._real32),cmplx(3._real32,0._real32)])&
                      == [cmplx(1._real32,0._real32),&
                          cmplx(2._real32,0._real32),&
                          cmplx(3._real32,0._real32)]),&
                  'union, complex real32')
n = n + 1

tests(n) = assert(all(union([cmplx(1._real64,0._real64),cmplx(2._real64,0._real64)],&
                            [cmplx(2._real64,0._real64),cmplx(3._real64,0._real64)])&
                      == [cmplx(1._real64,0._real64),&
                          cmplx(2._real64,0._real64),&
                          cmplx(3._real64,0._real64)]),&
                  'union, complex real64')
n = n + 1

tests(n) = assert(all(union([cmplx(1._real128,0._real128),cmplx(2._real128,0._real128)],&
                            [cmplx(2._real128,0._real128),cmplx(3._real128,0._real128)])&
                      == [cmplx(1._real128,0._real128),&
                          cmplx(2._real128,0._real128),&
                          cmplx(3._real128,0._real128)]),&
                  'union, complex real128')
n = n + 1

tests(n) = assert(all(union(arange(1,0),arange(1,0)) == arange(1,0)),&
                  'union of empty arrays is an empty array')
n = n + 1

tests(n) = assert(all(union([1,2,2,3],arange(1,0)) == set([1,2,2,3])),&
                  'union(x,[]) == set(x)')
n = n + 1

tests(n) = assert(all(union(arange(1,0),[1,2,2,3]) == set([1,2,2,3])),&
                  'union([],x) == set(x)')
n = n + 1

tests(n) = assert(all(union([1,2],[3,4]) == ([1,2].union.[3,4])),&
                  'union operator, a.union.b')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_union
