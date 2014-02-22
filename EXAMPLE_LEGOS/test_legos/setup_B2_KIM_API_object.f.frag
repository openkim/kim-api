!-------------------------------------------------------------------------------
!
! setup_B2_KIM_API_object : Create KIM API object,
!                           allocate memory and set known values.
!
!-------------------------------------------------------------------------------
subroutine setup_B2_KIM_API_object(pkim, testname, modelname, specname1, &
                                   specname2)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr),                          intent(out) :: pkim
  character(len=KIM_KEY_STRING_LENGTH), intent(in)  :: testname
  character(len=KIM_KEY_STRING_LENGTH), intent(in)  :: modelname
  character(len=2),                     intent(in)  :: specname1
  character(len=2),                     intent(in)  :: specname2

  !-- Local variables
  integer(c_int)            :: N = 2 ! hard-wired to two atoms
  integer(c_int), parameter :: ATypes = 2  ! hard-wired to two atomic types
  integer(c_int) ier, idum
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnAtoms
  integer(c_int), pointer :: numberParticleTypes; type(c_ptr) :: pnparticleTypes
  integer(c_int), pointer :: particleTypes(:);    type(c_ptr) :: pparticleTypes

  ! Initialize KIM API object
  !
  ier = kim_api_init(pkim, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_init", ier)
     stop
  endif
  call kim_api_allocate(pkim, N, ATypes, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_allocate", ier)
     stop
  endif

  ! call model's init routine
  !
  ier = kim_api_model_init(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_init", ier)
     stop
  endif

  ! Unpack data from KIM object whose values need to be set
  !
  call kim_api_getm_data(pkim, ier, &
       "numberOfParticles",   pnAtoms,           1, &
       "numberParticleTypes", pnparticleTypes,   1, &
       "particleTypes",       pparticleTypes,    1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(pnAtoms,         numberOfParticles)
  call c_f_pointer(pnparticleTypes, numberParticleTypes)
  call c_f_pointer(pparticleTypes,  particleTypes, [N])

  ! Set values
  !
  numberOfParticles   = N
  numberParticleTypes = ATypes
  particleTypes(1)    = kim_api_get_partcl_type_code(pkim, specname1, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_partcl_type_code", ier)
     stop
  endif
  particleTypes(2)    = kim_api_get_partcl_type_code(pkim, specname2, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_partcl_type_code", ier)
     stop
  endif

  return

end subroutine setup_B2_KIM_API_object
