program test_split
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional,only:arange,split

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 16
call initialize_tests(tests,ntests)

tests(n) = assert(all(split(arange(1_int8,10_int8),1) == arange(1,5)),&
                  'split(x,1), int8')
n = n + 1

tests(n) = assert(all(split(arange(1_int8,10_int8),2) == arange(6,10)),&
                  'split(x,2), int8')
n = n + 1

tests(n) = assert(all(split(arange(1_int16,10_int16),1) == arange(1,5)),&
                  'split(x,1), int16')
n = n + 1

tests(n) = assert(all(split(arange(1_int16,10_int16),2) == arange(6,10)),&
                  'split(x,2), int16')
n = n + 1

tests(n) = assert(all(split(arange(1_int32,10_int32),1) == arange(1,5)),&
                  'split(x,1), int32')
n = n + 1

tests(n) = assert(all(split(arange(1_int32,10_int32),2) == arange(6,10)),&
                  'split(x,2), int32')
n = n + 1

tests(n) = assert(all(split(arange(1_int64,10_int64),1) == arange(1,5)),&
                  'split(x,1), int64')
n = n + 1

tests(n) = assert(all(split(arange(1_int64,10_int64),2) == arange(6,10)),&
                  'split(x,2), int64')
n = n + 1

tests(n) = assert(all(split(arange(1._real32,10._real32),1) == arange(1,5)),&
                  'split(x,1), real32')
n = n + 1

tests(n) = assert(all(split(arange(1._real32,10._real32),2) == arange(6,10)),&
                  'split(x,2), real32')
n = n + 1

tests(n) = assert(all(split(arange(1._real64,10._real64),1) == arange(1,5)),&
                  'split(x,1), real64')
n = n + 1

tests(n) = assert(all(split(arange(1._real64,10._real64),2) == arange(6,10)),&
                  'split(x,2), real64')
n = n + 1

tests(n) = assert(all(split(arange(1._real128,10._real128),1) == arange(1,5)),&
                  'split(x,1), real128')
n = n + 1

tests(n) = assert(all(split(arange(1._real128,10._real128),2) == arange(6,10)),&
                  'split(x,2), real128')
n = n + 1

tests(n) = assert(all(split([1],1) == arange(1,0)),'split([1],1) returns an empty array')
n = n + 1

tests(n) = assert(all(split([1],2) == [1]),'split([1],2) returns [1]')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_split
