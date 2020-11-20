module testing

  ! Simple unit testing module.

  implicit none

  private
  public :: assert, initialize_tests, report_tests

contains


  logical function assert(condition, test_name)
    ! Prints the result of the test to standard output.
    logical, intent(in) :: condition
    character(len=*), intent(in) :: test_name
    character(len=69) :: output_test_name
    assert = condition
    output_test_name = test_name
    if (assert) then
      write(*, '(a)') 'test ' // output_test_name // ': ' // &
        char(27) // '[32mPASS' // char(27) // '[0m'
    else
      write(*, '(a)') 'test ' // output_test_name // ': ' // &
        char(27) // '[31mFAIL' // char(27) // '[0m'
    end if
  end function assert


  subroutine initialize_tests(tests, ntests)
    ! Initialize the test suite.
    logical, allocatable, intent(in out) :: tests(:)
    integer, intent(in) :: ntests
    if (allocated(tests)) deallocate(tests)
    allocate(tests(ntests))
  end subroutine initialize_tests


  subroutine report_tests(tests, test_failed)
    ! Print the test suite report to standard output.
    logical, intent(in) :: tests(:)
    logical, optional, intent(out)    :: test_failed
    integer :: n, ntests, nsuccess, nfailure
    ntests = size(tests)
    nsuccess = 0
    nfailure = 0
    do n = 1, ntests
      if (tests(n)) then
        nsuccess = nsuccess + 1
      else
        nfailure = nfailure + 1
      end if
    end do
    write(*, '(a, i3, a)') 'Ran a total of ', ntests, ' tests.'
    write(*, '(i3, a, i3, a)') nsuccess, ' tests PASSED,  ', &
                               nfailure, ' tests FAILED.'
    if (present(test_failed)) then
      test_failed = .false.
      if (.not. nfailure == 0 ) test_failed = .true.
    end if
  end subroutine report_tests

end module testing
