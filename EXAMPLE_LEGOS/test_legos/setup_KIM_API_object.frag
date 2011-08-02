!-------------------------------------------------------------------------------
!
! setup_KIM_API_object : Create KIM API object, 
!                        allocate memory and set known values.
!
!-------------------------------------------------------------------------------
subroutine setup_KIM_API_object(pkim, testname, modelname, N, specname)
  use KIMservice
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim
  character(len=80),        intent(in)  :: testname
  character(len=80),        intent(in)  :: modelname
  integer(kind=kim_intptr), intent(in)  :: N
  character(len=2),         intent(in)  :: specname

  !-- Local variables
  integer, parameter :: ATypes = 1  ! hard-wired to one atomic type
  integer ier, N4
  integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
  integer numberAtomTypes;       pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1);       pointer(patomTypesdum,atomTypesdum)
  integer, pointer :: atomTypes(:)

  ! Initialize KIM API object
  !
  ier = kim_api_init_f(pkim, testname, modelname)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_init_f", ier)
     stop
  endif
  call kim_api_allocate_f(pkim, N, ATypes, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! call model's init routine
  !
  ier = kim_api_model_init(pkim)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_init", ier)
     stop
  endif

  ! Unpack data from KIM object whose values need to be set
  !
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier);
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif

  pnAtomTypes = kim_api_get_data_f(pkim, "numberAtomTypes", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif

  patomTypesdum = kim_api_get_data_f(pkim, "atomTypes", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  N4 = N
  call toIntegerArrayWithDescriptor1d(atomTypesdum, atomTypes, N4)

  ! Set values
  !
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, specname, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_atypecode_f", ier)
     stop
  endif

  return

end subroutine setup_KIM_API_object
