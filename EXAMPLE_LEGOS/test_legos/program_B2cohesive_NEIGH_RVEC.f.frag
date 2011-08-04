!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic B2 crystal of SPECIES1_NAME_STR and SPECIES2_NAME_STR as a 
!**  function of lattice spacing.
!**
!**  Works with the following NBC methods:
!**        NEIGH-RVEC-F
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
!**  All rights reserved.
!**
!*******************************************************************************


!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program TEST_NAME_STR
  use KIMservice
  implicit none

!============================== VARIABLE DEFINITIONS ==========================

  ! parameters controlling behavior of test
  !
  character(len=80),        parameter :: testname    = "TEST_NAME_STR"
  character(len=2),         parameter :: specname1   = 'SPECIES1_NAME_STR'
  character(len=2),         parameter :: specname2   = 'SPECIES2_NAME_STR'
  double precision,         parameter :: TOL         = 1.0d-8
  double precision,         parameter :: B2spacing   = B2_SPACING_STR
  double precision,         parameter :: MinSpacing  = 0.800d0*B2spacing
  double precision,         parameter :: MaxSpacing  = 1.200d0*B2spacing
  integer,                  parameter :: DIM         = 3

  ! significant local variables
  !
  double precision     :: rcut               ! cutoff radius of the potential

  integer,          allocatable :: NLRvecLocs(:)     ! neighbor list pointers
  integer,          allocatable :: neighborList(:,:) ! neighbor list storage
  double precision, allocatable :: RijList(:,:,:)    ! Rij vector list storage

  integer              :: NNeighbors         ! maximum number of neighbors for an atom

  double precision     :: FinalSpacing       ! crystal lattice parameter

  double precision     :: FinalEnergy        ! energy per atom of crystal 
                                             ! at current spacing

  integer              :: CellsPerRcut       ! number of unit cells along
                                             ! box (of size rcut) side


  ! KIM variables
  !
  character(len=80)         :: modelname     ! KIM-compliant model name

  integer(kind=kim_intptr)  :: pkim          ! pointer to KIM API object

  integer                   :: ier           ! error flag

  real*8 coordum(DIM,1);   pointer(pcoor,coordum)         ! coordinate

  real*8 cutoff; pointer(pcutoff,cutoff)                  ! cutoff

  integer(kind=kim_intptr) :: N                           ! number of atoms


  ! other variables
  !
  double precision, external  ::  get_model_cutoff_firsttime

!========================= END VARIABLE DEFINITIONS ==========================


  ! Read in KIM Model name to use
  !
  print *, "Please enter a valid KIM model name: "
  read(*,*) modelname


  ! We'll use just two atom (one SPECIES1_NAME_STR and one SPECIES2_NAME_STR) for this calculation!
  !
  N = 2


  ! Setup the KIM API object
  !
  call setup_B2_KIM_API_object(pkim, testname, modelname, specname1, specname2)


  ! allocate storage for neighbor lists, compute them for the first time, 
  ! and store necessary pointers in KIM API object
  !
  ! First, access the `cutoff' arguemt
  !
  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  rcut = cutoff
  !
  ! Second, determine how many neighbors we will need
  !
  CellsPerRcut = ceiling(rcut/MinSpacing+ 0.05d0) ! the 0.05 is a saftey factor
  NNeighbors = 2*((2*CellsPerRcut + 1)**3)
  !
  ! allocate memory for the neighbor list and Rij vectors
  !
  allocate(neighborList(NNeighbors+1,N))
  allocate(RijList(3,NNeighbors+1,N))
  allocate(NLRvecLocs(3))
  ! 
  call setup_neighborlist_Rij_KIM_access(pkim, N, NNeighbors, neighborList, &
                                         RijList, NLRvecLocs)


  ! find equilibrium spacing by minimizing coheseive energy with respect
  ! to the periodic box size
  !
  call NEIGH_RVEC_compute_equilibrium_spacing(pkim, &
         DIM,CellsPerRcut,MinSpacing,MaxSpacing, &
         TOL,N,NNeighbors,neighborlist,RijList,  &
         .false.,FinalSpacing,FinalEnergy)

  ! print results to screen
  !
  print '(80(''*''))'
  print *, "Results for KIM Model: ", modelname
  print *,
  print *,"Found minimum energy configuration to within", TOL
  print *
  print *,"Energy/atom = ", FinalEnergy, "; Spacing = ", FinalSpacing
  print '(80(''*''))'


  ! Don't forget to free and/or deallocate
  !
  deallocate(neighborList)
  deallocate(RijList)
  deallocate(NLRvecLocs)
  call free_KIM_API_object(pkim)

  stop

end program TEST_NAME_STR

!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_compute_equilibrium_spacing : 
!
!    Use the Golden section search algorithm to find the equilibrium spacing by 
!    minimizing the energy of the system with respect to the periodic box size.
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_compute_equilibrium_spacing(pkim, &
             DIM,CellsPerRcut,MinSpacing,MaxSpacing, &
             TOL,N,NNeighbors,neighborlist,RijList,  &
             verbose,RetSpacing,RetEnergy)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)  :: pkim
  integer,                  intent(in)  :: DIM
  integer,                  intent(in)  :: CellsPerRcut
  double precision,         intent(in)  :: MinSpacing
  double precision,         intent(in)  :: MaxSpacing
  double precision,         intent(in)  :: TOL
  integer(kind=kim_intptr), intent(in)  :: N
  integer,                  intent(in)  :: NNeighbors
  integer,                  intent(in)  :: neighborList(NNeighbors+1,N)
  double precision,         intent(in)  :: RijList(3,NNeighbors+1,N)
  logical,                  intent(in)  :: verbose
  double precision,         intent(out) :: RetSpacing
  double precision,         intent(out) :: RetEnergy
  
  !-- Local variables
  double precision,         parameter :: Golden      = (1.d0 + sqrt(5.d0))/2.d0
  integer ier
  double precision Spacings(4)
  double precision Energies(4)
  real*8 energy;           pointer(penergy,energy)
  integer N4
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  real*8, pointer :: coords(:,:)
  real*8 cutoff;           pointer(pcutoff,cutoff)

  ! Unpack data from KIM object
  !
  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif

  pcoor = kim_api_get_data_f(pkim, "coordinates", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  N4 = N  ! (Some routines expect N to be integer*4)
  call toRealArrayWithDescriptor2d(coordum, coords, DIM, N4)

  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif


  ! Initialize for minimization
  !
  Spacings(1) = MinSpacing
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_RVEC_F_periodic_B2_neighborlist(CellsPerRcut, (cutoff+0.75), &
                                             Spacings(1), NNeighbors,     &
                                             neighborList, RijList)
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute_f", ier)
     stop
  endif
  Energies(1) = energy
  if (verbose) &
     print *, "Energy/atom = ", Energies(1), "; Spacing = ", Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_RVEC_F_periodic_B2_neighborlist(CellsPerRcut, (cutoff+0.75), &
                                             Spacings(3), NNeighbors,     &
                                             neighborList, RijList)
  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute_f", ier)
     stop
  endif
  Energies(3) = energy
  if (verbose) &
     print *, "Energy/atom = ", Energies(3), "; Spacing = ", Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0 - Golden)*(MaxSpacing - MinSpacing)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_RVEC_F_periodic_B2_neighborlist(CellsPerRcut, (cutoff+0.75), &
                                             Spacings(2), NNeighbors,     &
                                             neighborList, RijList)
  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute_f", ier)
     stop
  endif
  Energies(2) = energy
  if (verbose) &
     print *, "Energy/atom = ", Energies(2), "; Spacing = ", Spacings(2)


  ! iterate until convergence.
  !
  do while (abs(Spacings(3) - Spacings(1)) .gt. TOL)
     ! set new spacing
     Spacings(4) = (Spacings(1) + Spacings(3)) - Spacings(2)
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call NEIGH_RVEC_F_periodic_B2_neighborlist(CellsPerRcut, (cutoff+0.75), &
                                                Spacings(4), NNeighbors,     &
                                                neighborList, RijList)
     ! Call model compute
     call kim_api_model_compute_f(pkim, ier)
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_model_compute_f", ier)
        stop
     endif
     Energies(4) = energy
     if (verbose) &
        print *, "Energy/atom = ", Energies(4), "; Spacing = ", Spacings(4)

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

  stop

end subroutine NEIGH_RVEC_compute_equilibrium_spacing
