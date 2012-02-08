!-------------------------------------------------------------------------------
!
! free_KIM_API_object : Deallocate storage and destroy KIM API object
!
!-------------------------------------------------------------------------------
subroutine free_KIM_API_object(pkim)
  use KIM_API
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim

  !-- Local variables
  integer ier, idum

  ! call the model destroy function
  !
  call kim_api_model_destroy_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_destroy_f", ier)
     stop
  endif

  ! free all KIM API object storage
  !
  call kim_api_free_f(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_free_f", ier)
     stop
  endif

  return

end subroutine free_KIM_API_object
