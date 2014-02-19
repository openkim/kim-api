!-------------------------------------------------------------------------------
!
! free_KIM_API_object : Deallocate storage and destroy KIM API object
!
!-------------------------------------------------------------------------------
subroutine free_KIM_API_object(pkim)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr), intent(in)  :: pkim

  !-- Local variables
  integer(c_int) ier, idum

  ! call the model destroy function
  !
  ier = kim_api_model_destroy(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_destroy", ier)
     stop
  endif

  ! free all KIM API object storage
  !
  call kim_api_free(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_free", ier)
     stop
  endif

  return

end subroutine free_KIM_API_object
