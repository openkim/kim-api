!-------------------------------------------------------------------------------
!
! free_KIM_API_object : Deallocate storage and destroy KIM API object
!
!-------------------------------------------------------------------------------
subroutine free_KIM_API_object(pkim)
  use KIMservice
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim

  !-- Local variables
  integer ier

  ! call the model destroy function
  !
  call kim_api_model_destroy(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_destroy", ier)
     stop
  endif

  ! free all KIM API object storage
  !
  call kim_api_free(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_free", ier)
     stop
  endif

  return

end subroutine free_KIM_API_object
