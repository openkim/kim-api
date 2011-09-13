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
double precision function get_model_cutoff_firsttime(testname, modelname)
  use KIMservice
  implicit none

  !-- Transferred variables
  character(len=80),  intent(in)  :: testname
  character(len=80),  intent(in)  :: modelname

  !-- Local variables
  integer, parameter :: ATypes = 1  ! hard-wired to one atomic type
  integer ier
  integer(kind=kim_intptr) pkim_temp
  integer N
  real*8 cutoff; pointer(pcutoff,cutoff)

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim_temp, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_init_f", ier)
     stop
  endif

  ! To get the `cutoff', we use 1 atom to allocate memory via the KIM system
  !
  N = 1
  call kim_api_allocate_f(pkim_temp, N, ATypes, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! call model's init routine to put cutoff in KIM API object
  !
  ier = kim_api_model_init_f(pkim_temp)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_init_f", ier)
     stop
  endif

  ! access the `cutoff' argument
  !
  pcutoff = kim_api_get_data_f(pkim_temp, "cutoff", ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_f", ier)
     stop
  endif
  get_model_cutoff_firsttime = cutoff

  ! tear it all down
  !
  call kim_api_model_destroy_f(pkim_temp, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_destroy_f", ier)
     stop
  endif
  call kim_api_free_f(pkim_temp, ier)
  if (ier.lt.KIM_STATUS_OK) then
     call kim_api_report_error_f(__LINE__, __FILE__, "kim_api_free_f", ier)
     stop
  endif

  return

end function get_model_cutoff_firsttime
