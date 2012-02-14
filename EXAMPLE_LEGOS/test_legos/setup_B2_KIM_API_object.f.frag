!-------------------------------------------------------------------------------
!
! setup_B2_KIM_API_object : Create KIM API object, 
!                           allocate memory and set known values.
!
!-------------------------------------------------------------------------------
subroutine setup_B2_KIM_API_object(pkim, testname, modelname, specname1, specname2)
  use KIM_API
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim
  character(len=80),        intent(in)  :: testname
  character(len=80),        intent(in)  :: modelname
  character(len=2),         intent(in)  :: specname1
  character(len=2),         intent(in)  :: specname2

  !-- Local variables
  integer            :: N = 2 ! hard-wired to two atoms
  integer, parameter :: ATypes = 2  ! hard-wired to two atomic types
  integer ier, idum
  integer numberOfParticles;   pointer(pnAtoms,numberOfParticles)
  integer numberParticleTypes; pointer(pnparticleTypes,numberParticleTypes)
  integer atomTypesdum(1);     pointer(patomTypesdum,atomTypesdum)
  integer, pointer :: atomTypes(:)

  ! Initialize KIM API object
  !
  ier = kim_api_init_f(pkim, testname, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_init_f", ier)
     stop
  endif
  call kim_api_allocate_f(pkim, N, ATypes, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! call model's init routine
  !
  ier = kim_api_model_init_f(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_model_init_f", ier)
     stop
  endif

  ! Unpack data from KIM object whose values need to be set
  !
  call kim_api_getm_data_f(pkim, ier, &
       "numberOfParticles",   pnAtoms,         1, &
       "numberParticleTypes", pnparticleTypes, 1, &
       "atomTypes",           patomTypesdum,   1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
     stop
  endif

  call KIM_to_F90_int_array_1d(atomTypesdum, atomTypes, N)

  ! Set values
  !
  numberOfParticles   = N
  numberParticleTypes = ATypes
  atomTypes(1)    = kim_api_get_partcl_type_code_f(pkim, specname1, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_partcl_type_code_f", ier)
     stop
  endif
  atomTypes(2)    = kim_api_get_partcl_type_code_f(pkim, specname2, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_partcl_type_code_f", ier)
     stop
  endif

  return

end subroutine setup_B2_KIM_API_object
