!-------------------------------------------------------------------------------
!
! report_error subroutine
!
!-------------------------------------------------------------------------------
subroutine report_error(line, str, status)
  implicit none
  
  !-- Transferred variables
  integer,   intent(in) :: line
  character(len=*), intent(in) :: str
  integer,   intent(in) :: status
  
  !-- Local variables
  character(len=10000), parameter :: file = __FILE__
  
  !-- print the error message
  print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', status
  
end subroutine report_error
