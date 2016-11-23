module mod_testing

implicit none

private

public :: assert
public :: initialize_tests
public :: report_tests

integer,parameter :: stdout = 6
integer,parameter :: stderr = 0

contains


logical function assert(condition,test_name)
  logical,         intent(in) :: condition
  character(len=*),intent(in) :: test_name
  character(len=69) :: output_test_name
  assert = condition
  output_test_name = test_name
  if(assert)then
    write(unit=stdout,fmt='(a)')'test '//output_test_name//': '//&
      char(27)//'[32mPASS'//char(27)//'[0m'
  else
    write(unit=stdout,fmt='(a)')'test '//output_test_name//': '//&
      char(27)//'[31mFAIL'//char(27)//'[0m'
  endif
endfunction assert


subroutine initialize_tests(tests,ntests)
  logical,dimension(:),allocatable,intent(inout) :: tests
  integer,intent(in) :: ntests
  if(allocated(tests))then
    deallocate(tests)
  endif
  allocate(tests(ntests))
endsubroutine initialize_tests



subroutine report_tests(tests,test_failed)
  logical,dimension(:),intent(in) :: tests
  logical,optional,intent(out)    :: test_failed
  integer :: n,ntests,nsuccess,nfailure
  ntests = size(tests)
  nsuccess = 0
  nfailure = 0
  do n = 1,ntests
    if(tests(n))then
      nsuccess = nsuccess + 1
    else
      nfailure = nfailure + 1
    endif
  enddo
  write(unit=stdout,fmt='(a,i3,a)')'Ran a total of ',ntests,' tests.'
  write(unit=stdout,fmt='(i3,a,i3,a)')nsuccess,' tests PASSED, ',&
                                      nfailure,' tests FAILED.'
  if(present(test_failed)) then
    test_failed = .false.
    if(.not. nfailure == 0 )test_failed = .true.
  endif
endsubroutine report_tests

endmodule mod_testing
