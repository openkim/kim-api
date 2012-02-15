!-------------------------------------------------------------------------------
!
! setup_KIM_API_object : Create KIM API object, 
!                        allocate memory and set known values.
!
!-------------------------------------------------------------------------------
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))
subroutine setup_KIM_API_object(pkim, testname, modelname, N, specname, SupportHalf)
  use KIM_API
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim
  character(len=80),        intent(in)  :: testname
  character(len=80),        intent(in)  :: modelname
  integer,                  intent(in)  :: N
  character(len=2),         intent(in)  :: specname
  integer,                  intent(in)  :: SupportHalf

  !-- Local variables
  integer, parameter :: ATypes = 1  ! hard-wired to one atomic type
  integer ier, idum
  integer numberOfParticles;   pointer(pnAtoms,numberOfParticles)
  integer numContrib;          pointer(pnumContrib,numContrib)
  integer numberParticleTypes; pointer(pnparticleTypes,numberParticleTypes)
  integer particleTypesdum(1); pointer(pparticleTypesdum,particleTypesdum)
  integer, pointer :: particleTypes(:)

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
       "numberOfParticles",           pnAtoms,           1,                           &
       "numberContributingParticles", pnumContrib,       TRUEFALSE(SupportHalf.eq.1), & 
       "numberParticleTypes",         pnparticleTypes,   1,                           &
       "particleTypes",               pparticleTypesdum, 1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_getm_data_f", ier)
     stop
  endif

  call KIM_to_F90_int_array_1d(particleTypesdum, particleTypes, N)

  ! Set values
  !
  numberOfParticles   = N
  if (SupportHalf.eq.1) numContrib = 1
  numberParticleTypes = ATypes
  particleTypes(:)        = kim_api_get_partcl_type_code_f(pkim, specname, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_partcl_type_code_f", ier)
     stop
  endif

  return

end subroutine setup_KIM_API_object
