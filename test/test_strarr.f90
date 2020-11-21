program test_strarr
use testing, only:assert, initialize_tests, report_tests
use functional, only:arrstr, strarr, empty

implicit none

logical, dimension(:), allocatable :: tests
logical :: test_failed
integer :: n, ntests

n = 1
ntests = 3
call initialize_tests(tests, ntests)

tests(n) = assert(all(strarr('hello') == ['h', 'e', 'l', 'l', 'o']), &
                  'strarr converts to array')
n = n + 1

tests(n) = assert(all(strarr('') == empty(' ')), &
                  'strarr converts empty string to []')
n = n + 1

tests(n) = assert(arrstr(strarr('hello')) == 'hello', &
                  'arrstr(strarr(string)) == string')
n = n + 1

test_failed = .false.
call report_tests(tests, test_failed)
if(test_failed)stop 1

end program test_strarr
