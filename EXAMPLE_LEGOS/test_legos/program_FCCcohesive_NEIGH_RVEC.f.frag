!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic FCC crystal (spec="SPECIES_NAME_STR") as a 
!**  function of lattice spacing.
!**
!**  Works with the following NBC methods:
!**        NEIGH-RVEC-F
!**
!**  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
!**
!**  Release: This file is part of the openkim-api.git repository.
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
  character(len=80), parameter :: testname    = "TEST_NAME_STR"
  character(len=2),  parameter :: specname    = 'SPECIES_NAME_STR'
  double precision,  parameter :: TOL         = 1.0d-8
  double precision,  parameter :: FCCspacing  = FCC_SPACING_STR
  double precision,  parameter :: MinSpacing  = 0.800d0*FCCspacing
  double precision,  parameter :: MaxSpacing  = 1.200d0*FCCspacing
  integer,           parameter :: DIM         = 3

  ! significant local variables
  !
  integer(kind=kim_intptr), allocatable :: NLRvecLocs(:)     ! neighbor list pointers
  integer,                  allocatable :: neighborList(:,:) ! neighbor list storage
  double precision,         allocatable :: RijList(:,:,:)    ! Rij vector list storage

  integer              :: NNeighbors         ! maximum number of neighbors for an atom

  double precision     :: FinalSpacing       ! crystal lattice parameter

  double precision     :: FinalEnergy        ! energy per atom of crystal 
                                             ! at current spacing

  integer              :: CellsPerCutoff     ! number of unit cells along
                                             ! box (of size cutoff) side


  ! KIM variables
  !
  character(len=80)         :: modelname     ! KIM-compliant model name

  integer(kind=kim_intptr)  :: pkim          ! pointer to KIM API object

  integer                   :: ier           ! error flag

  real*8 coordum(DIM,1);   pointer(pcoor,coordum)         ! coordinate

  real*8 cutoff; pointer(pcutoff,cutoff)                  ! cutoff radius of Model

  integer(kind=kim_intptr) :: N                           ! number of atoms


  ! other variables
  !
  double precision, external  ::  get_model_cutoff_firsttime

!========================= END VARIABLE DEFINITIONS ==========================


  ! Read in KIM Model name to use
  !
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname


  ! We'll use just one atom for this calculation!
  !
  N = 1


  ! Setup the KIM API object
  !
  call setup_KIM_API_object(pkim, testname, modelname, N, specname)


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
  !
  ! Second, determine how many neighbors we will need
  !
  CellsPerCutoff = ceiling(cutoff/MinSpacing+ 0.05d0) ! the 0.05 is a saftey factor
  NNeighbors = 4*((2*CellsPerCutoff + 1)**3)
  !
  ! allocate memory for the neighbor list and Rij vectors
  !
  allocate(neighborList(NNeighbors+1,N))
  allocate(RijList(3,NNeighbors+1,N))
  allocate(NLRvecLocs(3))
  !
  NLRvecLocs(1) = loc(neighborList)
  NLRvecLocs(2) = loc(RijList)
  NLRvecLocs(3) = NNeighbors
  call setup_neighborlist_Rij_KIM_access(pkim, NLRvecLocs)


  ! find equilibrium spacing by minimizing coheseive energy with respect
  ! to the periodic box size
  !
  call NEIGH_RVEC_compute_equilibrium_spacing(pkim, &
         DIM,CellsPerCutoff,MinSpacing,MaxSpacing,  &
         TOL,N,NNeighbors,neighborlist,RijList,     &
         .false.,FinalSpacing,FinalEnergy)

  ! print results to screen
  !
  print '(80(''-''))'
  print '("This is Test          : ",A)', testname
  print '("Results for KIM Model : ",A)', modelname
  print *
  print '("Found minimum energy configuration to within",E25.15)', TOL
  print *
  print '("Energy/atom = ",E25.15,"; Spacing = ",E25.15)', FinalEnergy, FinalSpacing
  print '(80(''-''))'


  ! Don't forget to free and/or deallocate
  !
  deallocate(neighborList)
  deallocate(RijList)
  deallocate(NLRvecLocs)
  call free_KIM_API_object(pkim)

  stop

end program TEST_NAME_STR
