!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic FCC crystal (spec="SPECIES_NAME_STR") as a 
!**  function of lattice spacing.
!**
!**  Works with the following NBC methods:
!**        NEIGH-PURE-H
!**        NEIGH-PURE-F
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
  double precision     :: rcut               ! cutoff radius of the potential

  integer, allocatable :: neighborList(:,:)  ! neighbor list storage

  double precision     :: FinalSpacing       ! crystal lattice parameter

  double precision     :: FinalEnergy        ! energy per atom of crystal 
                                             ! at current spacing

  integer              :: CellsPerRcut       ! number of unit cells along
                                             ! box (of size rcut) side


  ! KIM variables
  !
  character(len=80)         :: modelname     ! KIM-compliant model name

  integer(kind=kim_intptr)  :: pkim          ! pointer to KIM API object

  real*8 coordum(DIM,1);   pointer(pcoor,coordum)         ! coordinate

  integer                   :: N                          ! number of atoms


  ! other variables
  !
  double precision, external  ::  get_model_cutoff_firsttime

!========================= END VARIABLE DEFINITIONS ==========================


  ! Read in KIM Model name to use
  !
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname


  ! Get model cutoff radius and compute number of atoms needed
  ! (We need 2*cutoff, use 2.125*cutoff for saftey)
  !
  rcut = get_model_cutoff_firsttime(testname, modelname)
  CellsPerRcut = ceiling(rcut/MinSpacing)
  N = 4*((2.d0*CellsPerRcut)**3)


  ! Setup the KIM API object
  !
  call setup_KIM_API_object(pkim, testname, modelname, N, specname)


  ! allocate storage for neighbor lists, compute them for the first time, 
  ! and store necessary pointers in KIM API object
  !
  allocate(neighborList(N+1, N))
  call setup_neighborlist_no_Rij_KIM_access(pkim, N, neighborList)


  ! find equilibrium spacing by minimizing coheseive energy with respect
  ! to the periodic box size
  !
  call NEIGH_PURE_compute_equilibrium_spacing(pkim, &
         DIM,CellsPerRcut,MinSpacing,MaxSpacing, &
         TOL,N,neighborlist,.false.,             &
         FinalSpacing,FinalEnergy)

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
  call free_KIM_API_object(pkim)

  stop

end program TEST_NAME_STR

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_compute_equilibrium_spacing : 
!
!    Use the Golden section search algorithm to find the equilibrium spacing by 
!    minimizing the energy of the system with respect to the periodic box size.
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_compute_equilibrium_spacing(pkim, &
             DIM,CellsPerRcut,MinSpacing,MaxSpacing, &
             TOL,N,neighborlist,verbose,             &
             RetSpacing,RetEnergy)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr), intent(in)     :: pkim
  integer,                  intent(in)     :: DIM
  integer,                  intent(in)     :: CellsPerRcut
  double precision,         intent(in)     :: MinSpacing
  double precision,         intent(in)     :: MaxSpacing
  double precision,         intent(in)     :: TOL
  integer,                  intent(in)     :: N
  integer,                  intent(inout)  :: neighborList(N+1,N)
  logical,                  intent(in)     :: verbose
  double precision,         intent(out)    :: RetSpacing
  double precision,         intent(out)    :: RetEnergy
  
  !-- Local variables
  double precision,         parameter :: Golden      = (1.d0 + sqrt(5.d0))/2.d0
  integer ier
  double precision Spacings(4)
  double precision Energies(4)
  integer MiddleAtomId
  real*8 energy;           pointer(penergy,energy)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  real*8, pointer :: coords(:,:)
  real*8 cutoff;           pointer(pcutoff,cutoff)
  logical :: halfflag  ! .true. = half neighbor list; .false. = full neighbor list
  character(len=64) NBC_Method;  pointer(pNBC_Method,NBC_Method)

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
  call toRealArrayWithDescriptor2d(coordum, coords, DIM, N)

  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif


  ! determine which neighbor list type to use
  !
  pNBC_Method = kim_api_get_nbc_method_f(pkim, ier) ! don't forget to free
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
     halfflag = .true.
  elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
     halfflag = .false.
  else
     ier = 0
     call report_error(__LINE__, "Unknown NBC method", ier)
     return
  endif
  call free(pNBC_Method) ! free the memory

  ! Initialize for minimization
  !
  Spacings(1) = MinSpacing
  call create_FCC_configuration(Spacings(1), 2*CellsPerRcut, .true., coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+0.75), &
                                        MiddleAtomId, neighborList)
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute_f", ier)
     stop
  endif
  if (halfflag) then ! half neighbor list computes twice the energy
     Energies(1) = energy/2.d0
  else
     Energies(1) = energy
  endif
  if (verbose) &
     print '("Energy/atom = ",E25.15,"; Spacing = ",E25.15)', Energies(1), Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  call create_FCC_configuration(Spacings(3), 2*CellsPerRcut, .true., coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+0.75), &
                                        MiddleAtomId, neighborList)
  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute_f", ier)
     stop
  endif
  if (halfflag) then ! half neighbor list computes twice the energy
     Energies(3) = energy/2.d0
  else
     Energies(3) = energy
  endif
  if (verbose) &
     print '("Energy/atom = ",E25.15,"; Spacing = ",E25.15)', Energies(3), Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0 - Golden)*(MaxSpacing - MinSpacing)
  call create_FCC_configuration(Spacings(2), 2*CellsPerRcut, .true., coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+0.75), &
                                        MiddleAtomId, neighborList)
  ! Call model compute
  call kim_api_model_compute_f(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute_f", ier)
     stop
  endif
  if (halfflag) then ! half neighbor list computes twice the energy
     Energies(2) = energy/2.d0
  else
     Energies(2) = energy
  endif
  if (verbose) &
     print '("Energy/atom = ",E25.15,"; Spacing = ",E25.15)', Energies(2), Spacings(2)


  ! iterate until convergence.
  !
  do while (abs(Spacings(3) - Spacings(1)) .gt. TOL)
     ! set new spacing
     Spacings(4) = (Spacings(1) + Spacings(3)) - Spacings(2)
     ! compute new atom coordinates based on new spacing
     call create_FCC_configuration(Spacings(4), 2*CellsPerRcut, .true., coords, MiddleAtomId)
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call NEIGH_PURE_periodic_neighborlist(halfflag, N, coords, (cutoff+0.75), &
                                           MiddleAtomId, neighborList)
     ! Call model compute
     call kim_api_model_compute_f(pkim, ier)
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_model_compute_f", ier)
        stop
     endif
     if (halfflag) then ! half neighbor list computes twice the energy
        Energies(4) = energy/2.d0
     else
        Energies(4) = energy
     endif
     if (verbose) &
        print '("Energy/atom = ",E25.15,"; Spacing = ",E25.15)', Energies(4), Spacings(4)

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

end subroutine NEIGH_PURE_compute_equilibrium_spacing
