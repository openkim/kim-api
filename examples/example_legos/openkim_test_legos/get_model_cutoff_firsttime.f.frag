!-------------------------------------------------------------------------------
!
! get_model_cutoff_firsttime: get cutoff radius of a KIM model *prior* to the
!                             existence of the KIM API object
!
! NOTE: This routine creates a temporary KIM API object and goes through the
!       initialization process in order to read the cutoff radius from the
!       API object.
!
!       This procedure needs to be improved in the future.
!
!-------------------------------------------------------------------------------
real(c_double) function get_model_cutoff_firsttime(testkimfile, modelname)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  character(len=KIM_KEY_STRING_LENGTH),  intent(in)  :: testkimfile
  character(len=KIM_KEY_STRING_LENGTH),  intent(in)  :: modelname

  !-- Local variables
  integer(c_int), parameter :: ASpecies = 1  ! hard-wired to one atomic species
  integer(c_int) ier, idum
  type(c_ptr) pkim_temp
  integer(c_int) N
  real(c_double), pointer :: cutoff; type(c_ptr) :: pcutoff

  ! Initialize the KIM object
  ier = kim_api_file_init(pkim_temp, testkimfile, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_file_init", ier)
     stop
  endif

  ! To get the `cutoff', we use 1 particle to allocate memory via the KIM system
  !
  N = 1
  call kim_api_allocate(pkim_temp, N, ASpecies, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_allocate", ier)
     stop
  endif

  ! call model's init routine to put cutoff in KIM API object
  !
  ier = kim_api_model_init(pkim_temp)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_init", ier)
     stop
  endif

  ! access the `cutoff' argument
  !
  pcutoff = kim_api_get_data(pkim_temp, "cutoff", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pcutoff, cutoff)
  get_model_cutoff_firsttime = cutoff

  ! tear it all down
  !
  ier = kim_api_model_destroy(pkim_temp)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_destroy", ier)
     stop
  endif
  call kim_api_free(pkim_temp, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_free", ier)
     stop
  endif

  return

end function get_model_cutoff_firsttime
