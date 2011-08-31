!-------------------------------------------------------------------------------
!
! report_error subroutine
!
!-------------------------------------------------------------------------------
subroutine report_error(line, str, status)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer,          intent(in) :: line
  character(len=*), intent(in) :: str
  integer,          intent(in) :: status
  
  !-- Local variables
  character(len=10000), parameter :: file = __FILE__
  character(len=KEY_CHAR_LENGTH)  :: message; pointer(pmessage,message)

  pmessage = kim_api_status_msg_f(status)
  !-- print the error message
  print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', &
          message(1:(index(message,char(0))-1))

  return
  
end subroutine report_error
