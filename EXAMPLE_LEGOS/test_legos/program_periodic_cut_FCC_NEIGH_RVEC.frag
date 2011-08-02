!*******************************************************************************
!**
!**  PROGRAM TEST_NAME_STR
!**
!**  KIM compliant program to compute the energy of one atom in a periodic
!**  FCC crystal of SPECIES_NAME_STR for a range of lattice spacings and model cutoff values.
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
  double precision param_cutoff; pointer(pparam_cutoff,param_cutoff)

  real*8 cutoff;           pointer(pcutoff,cutoff)
  real*8 energy;           pointer(penergy,energy)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  integer CellsPerCutoff
  integer N4
  real*8, pointer  :: coords(:,:)
  integer, pointer :: atomTypes(:)
  integer(kind=kim_intptr) :: N
  integer          :: NNeighbors
  double precision :: CurrentSpacing
  double precision :: MaxCutoff
  integer :: nParams
  integer :: paramIndex
  integer :: i
  character(len=KEY_CHAR_LENGTH) :: listOfParameters(1); pointer(plistOfParameters,listOfParameters)

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

  ! check for PARAM_FREE_cutoff
  plistOfParameters = kim_api_get_listparams_f(pkim, nParams, ier)
  paramIndex = 0
  print *,"The model has defined the following parameters:"
  do i=1,nParams
     print *, i, listOfParameters(i)
     if (index(listOfParameters(i),"PARAM_FREE_cutoff").eq.1) then
        paramIndex = i
     endif
  enddo
  call free(plistOfParameters) ! deallocate memory
  if (paramIndex .gt. 0) then
     print *,"PARAM_FREE_cutoff IS in the list, at index", paramIndex
  else
     print *,"PARAM_FREE_cutoff is NOT in the parameter list."
     stop "exiting..."
  endif

  pparam_cutoff = kim_api_get_data_f(pkim, "PARAM_FREE_cutoff", ier)
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


  ! set max cutoff
  MaxCutoff = cutoff + 2.0d0
  !  set initial cutoff
  param_cutoff = cutoff - 2.0d0
  ier = kim_api_model_reinit_f(pkim)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_reinit", ier)
     stop
  endif
  do while (cutoff .le. MaxCutoff)
     ! set up the periodic_cut atom neighbor list
     !
     ! determine how many neighbors we will need
     CellsPerCutoff = ceiling(cutoff/MinSpacing+ 0.05d0) ! the 0.05 is a saftey factor
     NNeighbors = 4*((2*CellsPerCutoff + 1)**3)
     ! compute Rij neighbor list where everything is an image of atom 1
     allocate(neighborList(NNeighbors+1,N))
     allocate(RijList(3,NNeighbors+1,N))
     ! generate neighbor list
     CurrentSpacing = MinSpacing
     call NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerCutoff, (cutoff+0.75), CurrentSpacing, NNeighbors, neighborList, RijList)
     
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
     
     ! Call model compute
     call kim_api_model_compute_f(pkim, ier)
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_model_compute", ier)
        stop
     endif
     
     ! print results to screen
     print *, "***********************************************************************************************"
     print *, "Results for KIM Model: ", modelname
     print *, "Using NBC: NEIGH_RVEC_F"
     print *, ""
     print *, "Energy/atom = ", energy, "; Spacing = ", CurrentSpacing, "; cutoff = ", cutoff
     
     ! compute for a range of lattice spacings
     
     
     ! set up the periodic_cut atom positions
     do while (CurrentSpacing.lt.MaxSpacing)
        ! set new spacing
        CurrentSpacing = CurrentSpacing + SpacingIncr
        ! compute new neighbor lists (could be done more intelligently, I'm sure)
        call NEIGH_RVEC_F_periodic_FCC_neighborlist(CellsPerCutoff, (cutoff+0.75), CurrentSpacing, NNeighbors, &
                                                    neighborList, RijList)
        
        ! Call model compute
        call kim_api_model_compute_f(pkim, ier)
        if (ier.le.0) then
           call report_error(__LINE__, "kim_api_model_compute", ier)
           stop
        endif
        
        ! report result
        print *, "Energy/atom = ", energy, "; Spacing = ", CurrentSpacing, "; cutoff = ", cutoff
     enddo
     
     ! increment cutoff and repeat
     param_cutoff = cutoff + 1.0d0
     ier = kim_api_model_reinit_f(pkim)
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_model_reinit", ier)
        stop
     endif
     ! Don't forget to free and/or deallocate
     deallocate(neighborList)
     deallocate(NLRvecLocs)
     deallocate(RijList)
  enddo
     

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
