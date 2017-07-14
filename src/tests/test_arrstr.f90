program test_arrstr
use mod_testing,only:assert,initialize_tests,report_tests
use mod_functional,only:arrstr,empty

implicit none

logical,dimension(:),allocatable :: tests
logical :: test_failed
integer :: n,norder,ntests
integer,parameter :: stdout = 6

n = 1
ntests = 2
call initialize_tests(tests,ntests)

tests(n) = assert(arrstr(['h','e','l','l','o']) == 'hello',&
                  'arrstr converts to string')
n = n + 1

tests(n) = assert(arrstr(empty(' ')) == '',&
                  'arrstr converts empty array to ""')
n = n + 1

test_failed = .false.
call report_tests(tests,test_failed)
if(test_failed)stop 1

endprogram test_arrstr
