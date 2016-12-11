program test_last
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

complex(kind=real32),dimension(:),allocatable :: c_r4
complex(kind=real64),dimension(:),allocatable :: c_r8
complex(kind=real128),dimension(:),allocatable :: c_r16

c_r4 = [(1,2),(2,4)]
c_r8 = c_r4
c_r16 = c_r4

n = 1
ntests = 11
call initialize_tests(tests,ntests)

tests(n) = assert(last([1_int8,2_int8]) == 2_int8,'last, int8')
n = n + 1

tests(n) = assert(last([1_int16,2_int16]) == 2_int16,'last, int16')
n = n + 1

tests(n) = assert(last([1_int32,2_int32]) == 2_int32,'last, int32')
n = n + 1

tests(n) = assert(last([1_int64,2_int64]) == 2_int64,'last, int64')
n = n + 1

tests(n) = assert(last([1._real32,2._real32]) == 2._real32,'last, real32')
n = n + 1

tests(n) = assert(last([1._real64,2._real64]) == 2._real64,'last, real64')
n = n + 1

tests(n) = assert(last([1._real128,2._real128]) == 2._real128,'last, real128')
n = n + 1

tests(n) = assert(last(c_r4) == c_r4(2),'last, complex real32')
n = n + 1

tests(n) = assert(last(c_r8) == c_r8(2),'last, complex real64')
n = n + 1

tests(n) = assert(last(c_r16) == c_r16(2),'last, complex real128')
n = n + 1

tests(n) = assert(last([1,2]) == .last.[1,2],'last operator, .last.x')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_last
