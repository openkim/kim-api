!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_compute_equilibrium_spacing :
!
!    Use the Golden section search algorithm to find the equilibrium spacing by
!    minimizing the energy of the system with respect to the periodic box size.
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_compute_equilibrium_spacing(pkim, &
             DIM,CellsPerCutoff,MinSpacing,MaxSpacing,  &
             TOL,N,neighObject,                         &
             verbose,RetSpacing,RetEnergy)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  type(c_ptr),            intent(in)    :: pkim
  integer(c_int),         intent(in)    :: DIM
  integer(c_int),         intent(in)    :: CellsPerCutoff
  real(c_double),         intent(in)    :: MinSpacing
  real(c_double),         intent(in)    :: MaxSpacing
  real(c_double),         intent(in)    :: TOL
  integer(c_int),         intent(in)    :: N
  type(neighObject_type), intent(inout) :: neighObject
  logical,                intent(in)    :: verbose
  real(c_double),         intent(out)   :: RetSpacing
  real(c_double),         intent(out)   :: RetEnergy

  !-- Local variables
  real(c_double), parameter :: Golden = (1.0_cd + sqrt(5.0_cd))/2.0_cd
  integer(c_int) ier, idum
  real(c_double) Spacings(4)
  real(c_double) Energies(4)
  real(c_double), pointer :: energy;           type(c_ptr) :: penergy
  real(c_double), pointer :: coords(:,:);      type(c_ptr) :: pcoor
  real(c_double), pointer :: cutoff;           type(c_ptr) :: pcutoff
  real(c_double), parameter :: cutpad = CUTOFF_PADDING_STR ! cutoff radius padding
  logical :: halfflag  ! .true. = half neigh list; .false. = full neigh list
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method;

  ! Unpack data from KIM object
  !
  call kim_api_getm_data(pkim, ier, &
       "energy",      penergy, 1, &
       "coordinates", pcoor,   1, &
       "cutoff",      pcutoff, 1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(penergy, energy)
  call c_f_pointer(pcoor, coords, [DIM,N])
  call c_f_pointer(pcutoff, cutoff)

  ! determine which neighbor list type to use
  !
  ier = kim_api_get_nbc_method(pkim, NBC_Method)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"NEIGH_RVEC_H").eq.1) then
     halfflag = .true.
  elseif (index(NBC_Method,"NEIGH_RVEC_F").eq.1) then
     halfflag = .false.
  else
     ier = KIM_STATUS_FAIL
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", ier)
     return
  endif

  ! Initialize for minimization
  !
  Spacings(1) = MinSpacing
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_RVEC_periodic_FCC_neighborlist(halfflag, CellsPerCutoff,     &
                                            (cutoff+cutpad), Spacings(1), &
                                            N, neighObject, coords)
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
     stop
  endif
  Energies(1) = energy
  if (verbose) &
     print *, "Energy/part = ", Energies(1), "; Spacing = ", Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_RVEC_periodic_FCC_neighborlist(halfflag, CellsPerCutoff,     &
                                            (cutoff+cutpad), Spacings(3), &
                                            N, neighObject, coords)
  ! Call model compute
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
     stop
  endif
  Energies(3) = energy
  if (verbose) &
     print *, "Energy/part = ", Energies(3), "; Spacing = ", Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0_cd - Golden)*(MaxSpacing - MinSpacing)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_RVEC_periodic_FCC_neighborlist(halfflag, CellsPerCutoff,     &
                                            (cutoff+cutpad), Spacings(2), &
                                            N, neighObject, coords)
  ! Call model compute
  ier = kim_api_model_compute(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_compute", ier)
     stop
  endif
  Energies(2) = energy
  if (verbose) &
     print *, "Energy/part = ", Energies(2), "; Spacing = ", Spacings(2)


  ! iterate until convergence.
  !
  do while (abs(Spacings(3) - Spacings(1)) .gt. TOL)
     ! set new spacing
     Spacings(4) = (Spacings(1) + Spacings(3)) - Spacings(2)
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call NEIGH_RVEC_periodic_FCC_neighborlist(halfflag, CellsPerCutoff,     &
                                               (cutoff+cutpad), Spacings(4), &
                                               N, neighObject, coords)
     ! Call model compute
     ier = kim_api_model_compute(pkim)
     if (ier.lt.KIM_STATUS_OK) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "kim_api_model_compute", ier)
        stop
     endif
     Energies(4) = energy
     if (verbose) &
        print *, "Energy/part = ", Energies(4), "; Spacing = ", Spacings(4)

     ! determine the new interval
     if (Energies(4) .lt. Energies(2)) then
        ! We want the right-hand interval
        Spacings(1) = Spacings(2); Energies(1) = Energies(2)
        Spacings(2) = Spacings(4); Energies(2) = Energies(4)
     else
        ! We want the left-hand interval
        Spacings(3) = Spacings(1); Energies(3) = Energies(1)
        Spacings(1) = Spacings(4); Energies(1) = Energies(4)
     endif
  enddo

  ! pull out results and return
  !
  RetSpacing = Spacings(2)
  RetEnergy  = Energies(2)

  return

end subroutine NEIGH_RVEC_compute_equilibrium_spacing
