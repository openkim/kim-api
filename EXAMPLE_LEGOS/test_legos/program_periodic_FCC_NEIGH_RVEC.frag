!******************************************************************************* 
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic FCC crystal of SPECIES_NAME_STR as a 
!**  function of lattice spacing.
!**
!**  Works with the following NBC scenarios:
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

  integer,                  external  :: get_neigh_Rij
  double precision,         parameter :: TOL         = 1.0d-8
  double precision,         parameter :: Golden      = (1.d0 + sqrt(5.d0))/2.d0
  double precision,         parameter :: FCCspacing  = FCC_SPACING_STR
  double precision,         parameter :: MinSpacing  = 0.800d0*FCCspacing
  double precision,         parameter :: MaxSpacing  = 1.200d0*FCCspacing
  double precision,         parameter :: SpacingIncr = 0.025d0*FCCspacing
  integer,                  parameter :: DIM               = 3
  integer,                  parameter :: ATypes            = 1
  integer(kind=kim_intptr), parameter :: SizeOne           = 1

  ! neighbor list
  integer, allocatable          :: neighborList(:,:)
  integer, allocatable          :: NLRvecLocs(:)
  double precision, allocatable :: RijList(:,:,:)


  !
  ! KIM variables
  !
  character*80              :: testname     = "TEST_NAME_STR"
  character*80              :: modelname
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier
  integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
  integer numberAtomTypes;       pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1);       pointer(patomTypesdum,atomTypesdum)

  real*8 cutoff;           pointer(pcutoff,cutoff)
  real*8 energy;           pointer(penergy,energy)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  integer CellsPerCutoff
  integer N4
  real*8, pointer  :: coords(:,:)
  integer, pointer :: atomTypes(:)
  integer(kind=kim_intptr) :: N
  integer          :: NNeighbors
  double precision :: Spacings(4)
  double precision :: Energies(4)


  ! Get KIM Model name to use
  print *, "Please enter a valid KIM model name: "
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_init_f", ier)
     stop
  endif

  ! Allocate memory via the KIM system
  N = 1; N4 = N ! We'll use just one atom for this calculation!
  call kim_api_allocate_f(pkim, N, ATypes, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! call model's init routine
  ier = kim_api_model_init_f(pkim)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_init", ier)
     stop
  endif


  ! Unpack data from KIM object
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
  call toIntegerArrayWithDescriptor1d(atomTypesdum, atomTypes, N4)

  pcoor = kim_api_get_data_f(pkim, "coordinates", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(coordum, coords, DIM, N4)

  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif

  ! Set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "SPECIES_NAME_STR", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_atypecode_f", ier)
     stop
  endif

  ! set up coords
  coords(:,1) = 0.0d0

  ! set up the periodic atom neighbor list
  !
  ! determine how many neighbors we will need
  CellsPerCutoff = ceiling(cutoff/MinSpacing+ 0.05d0) ! the 0.05 is a saftey factor
  NNeighbors = 4*((2*CellsPerCutoff + 1)**3)
  ! compute Rij neighbor list where everything is an image of atom 1
  allocate(neighborList(NNeighbors+1,N))
  allocate(RijList(3,NNeighbors+1,N))
  ! generate neighbor list
  Spacings(1) = MinSpacing
  call NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(1), NNeighbors, neighborList, RijList)

  ! store pointers to neighbor list object and access function
  allocate(NLRvecLocs(3))
  NLRvecLocs(1) = loc(neighborList)
  NLRvecLocs(2) = loc(RijList)
  NLRvecLocs(3) = NNeighbors+1
  ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(NLRvecLocs))
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data_f", ier)
     stop
  endif

  ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_neigh_Rij))
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data_f", ier)
     stop
  endif 

  ! print results to screen
  print *, "***********************************************************************************************"
  print *, "Results for KIM Model: ", modelname
  print *, "Using NBC: NEIGH_RVEC_F"
  print *, ""

  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif
  Energies(1) = energy
  print *, "Energy/atom = ", Energies(1), "; Spacing = ", Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  call NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(3), NNeighbors, neighborList, RijList)
  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif
  Energies(3) = energy
  print *, "Energy/atom = ", Energies(3), "; Spacing = ", Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0 - Golden)*(MaxSpacing - MinSpacing)
  call NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(2), NNeighbors, neighborList, RijList)
  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif
  Energies(2) = energy
  print *, "Energy/atom = ", Energies(2), "; Spacing = ", Spacings(2)
  
  
  ! iterate until convergence.


  do while (abs(Spacings(3) - Spacings(1)) .gt. TOL)
     ! set new spacing
     Spacings(4) = (Spacings(1) + Spacings(3)) - Spacings(2)
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(4), NNeighbors, neighborList, RijList)
     ! Call model compute
     call kim_api_model_compute_f(pkim, ier)
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_model_compute", ier)
        stop
     endif
     Energies(4) = energy
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

  print *,
  print *,"Found minimum energy configuration to within", TOL
  print *,
  print *,"Energy/atom = ", Energies(2), "; Spacing = ", Spacings(2)

  ! Don't forget to free and/or deallocate
  deallocate(neighborList)
  deallocate(NLRvecLocs)
  deallocate(RijList)

  call kim_api_model_destroy_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_destroy", ier)
     stop
  endif
  call kim_api_free(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_free", ier)
     stop
  endif

  stop
end program TEST_NAME_STR
