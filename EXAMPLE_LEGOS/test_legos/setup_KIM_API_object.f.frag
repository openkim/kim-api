!-------------------------------------------------------------------------------
!
! setup_KIM_API_object : Create KIM API object, 
!                        allocate memory and set known values.
!
!-------------------------------------------------------------------------------
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))
subroutine setup_KIM_API_object(pkim, testname, modelname, N, specname, SupportHalf)
  use KIMservice
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
  integer numberOfAtoms;         pointer(pnAtoms,numberOfAtoms)
  integer numContrib;            pointer(pnumContrib,numContrib)
  integer numberAtomTypes;       pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1);       pointer(patomTypesdum,atomTypesdum)
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
  call kim_api_get_data_multiple_f(pkim, ier, &
       "numberOfAtoms",           pnAtoms,       1,                           &
       "numberContributingAtoms", pnumContrib,   TRUEFALSE(SupportHalf.eq.1), & 
       "numberAtomTypes",         pnAtomTypes,   1,                           &
       "atomTypes",               patomTypesdum, 1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_data_multiple_f", ier)
     stop
  endif

  call toIntegerArrayWithDescriptor1d(atomTypesdum, atomTypes, N)

  ! Set values
  !
  numberOfAtoms   = N
  if (SupportHalf.eq.1) numContrib = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, specname, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error_f(__LINE__, __FILE__, "kim_api_get_atypecode_f", ier)
     stop
  endif

  return

end subroutine setup_KIM_API_object
