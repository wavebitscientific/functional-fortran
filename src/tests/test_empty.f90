program test_empty
use iso_fortran_env,only:int8,int16,int32,int64,real32,real64,real128
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional,only:empty

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 10
call initialize_tests(tests,ntests)

tests(n) = assert(size(empty(1_int8)) == 0,'empty, int8')
n = n + 1

tests(n) = assert(size(empty(1_int16)) == 0,'empty, int16')
n = n + 1

tests(n) = assert(size(empty(1_int32)) == 0,'empty, int32')
n = n + 1

tests(n) = assert(size(empty(1_int64)) == 0,'empty, int64')
n = n + 1

tests(n) = assert(size(empty(1._real32)) == 0,'empty, real32')
n = n + 1

tests(n) = assert(size(empty(1._real64)) == 0,'empty, real64')
n = n + 1

tests(n) = assert(size(empty(1._real128)) == 0,'empty, real128')
n = n + 1

tests(n) = assert(size(empty(cmplx(1._real32,0._real32))) == 0,'empty, complex32')
n = n + 1

tests(n) = assert(size(empty(cmplx(1._real64,0._real64))) == 0,'empty, complex64')
n = n + 1

tests(n) = assert(size(empty(cmplx(1._real128,0._real128))) == 0,'empty, complex128')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_empty
