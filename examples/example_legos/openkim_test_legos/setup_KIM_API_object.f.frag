!-------------------------------------------------------------------------------
!
! setup_KIM_API_object : Create KIM API object,
!                        allocate memory and set known values.
!
!-------------------------------------------------------------------------------
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))
subroutine setup_KIM_API_object(pkim, testkimfile, modelname, N, specname, &
                                SupportHalf)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  implicit none

  !-- Transferred variables
  type(c_ptr),                          intent(out) :: pkim
  character(len=KIM_KEY_STRING_LENGTH), intent(in)  :: testkimfile
  character(len=KIM_KEY_STRING_LENGTH), intent(in)  :: modelname
  integer(c_int),                       intent(in)  :: N
  character(len=2),                     intent(in)  :: specname
  integer(c_int),                       intent(in)  :: SupportHalf

  !-- Local variables
  integer(c_int), parameter :: ASpecies = 1  ! hard-wired to 1 particle species
  integer(c_int) ier, idum
  integer(c_int) isHalf
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnParts
  integer(c_int), pointer :: numContrib;          type(c_ptr) :: pnumContrib
  integer(c_int), pointer :: numberOfSpecies; type(c_ptr) :: pnOfSpecies
  integer(c_int), pointer :: particleSpecies(:);  type(c_ptr) :: pparticleSpecies

  ! Initialize KIM API object
  !
  ier = kim_api_file_init(pkim, testkimfile, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_file_init", ier)
     stop
  endif
  call kim_api_allocate(pkim, N, ASpecies, ier)
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

  ! determine if a half list is being used
  isHalf = kim_api_is_half_neighbors(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_is_half_neighbors", ier)
     stop
  endif

  ! Unpack data from KIM object whose values need to be set
  !
  call kim_api_getm_data(pkim, ier, &
       "numberOfParticles",           pnParts,           1, &
       "numberContributingParticles", pnumContrib,          &
           TRUEFALSE((SupportHalf.eq.1).and.(isHalf.eq.1)), &
       "numberOfSpecies",             pnOfSpecies,       1, &
       "particleSpecies",             pparticleSpecies,  1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(pnParts, numberOfParticles)
  if ((SupportHalf.eq.1).and.(isHalf.eq.1)) call c_f_pointer(pnumContrib, &
                                                             numContrib)
  call c_f_pointer(pnOfSpecies, numberOfSpecies)
  call c_f_pointer(pparticleSpecies,  particleSpecies, [N])

  ! Set values
  !
  numberOfParticles   = N
  if ((SupportHalf.eq.1).and.(isHalf.eq.1)) numContrib = 1
  numberOfSpecies = ASpecies
  particleSpecies(:)        = kim_api_get_species_code(pkim, specname, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_species_code", ier)
     stop
  endif

  return

end subroutine setup_KIM_API_object
