!*******************************************************************************
!**
!**  PROGRAM test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90
!**
!**  KIM compliant program to compute the energy of one two-atom cell in a 
!**  periodic B2 crystal of Ar and Ne for a range of lattice spacings.
!**
!**  Works with the following NBC scenarios:
!**        NEIGH-RVEC-F
!**
!**  Author: Ryan S. Elliott
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
program test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90
  use KIMservice
  implicit none

  integer,                  external  :: get_NEIGH_RVEC_neigh
  double precision,         parameter :: B2spacing  = 5.260d0 ! in angstroms
  double precision,         parameter :: MinSpacing  = 0.900d0*B2spacing
  double precision,         parameter :: MaxSpacing  = 1.100d0*B2spacing
  double precision,         parameter :: SpacingIncr = 0.025d0*B2spacing
  integer,                  parameter :: DIM               = 3
  integer,                  parameter :: ATypes            = 2
  integer(kind=kim_intptr), parameter :: SizeOne           = 1

  ! neighbor list
  integer, allocatable          :: neighborList(:,:)
  integer, allocatable          :: NLRvecLocs(:)
  double precision, allocatable :: RijList(:,:,:)


  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90"
  character*80              :: modelname
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier
  integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
  integer numberAtomTypes;      pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1); pointer(patomTypesdum,atomTypesdum)

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

  ! Get KIM Model name to use
  print *, "Please enter a valid KIM model name: "
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname); if (ier.le.0) stop "The given KIM Model name "&
       //"is not a match for this test."

  ! Allocate memory via the KIM system
  N = 2; N4 = N ! We'll use just two atoms (one Ar and one Ne) for this calculation!
  call kim_api_allocate_f(pkim, N, ATypes, ier); if (ier.le.0) call print_error("allocate", ier)

  ! call model's init routine
  ier = kim_api_model_init(pkim); if (ier.le.0) call print_error("model_init", ier)


  ! Unpack data from KIM object
  !
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier);
  if (ier.le.0) call print_error("numberOfAtoms", ier)

  pnAtomTypes = kim_api_get_data_f(pkim, "numberAtomTypes", ier)
  if (ier.le.0) call print_error("numberAtomTypes", ier)

  patomTypesdum = kim_api_get_data_f(pkim, "atomTypes", ier)
  if (ier.le.0) call print_error("atomTypes", ier)
  call toIntegerArrayWithDescriptor1d(atomTypesdum, atomTypes, N4)

  pcoor = kim_api_get_data_f(pkim, "coordinates", ier)
  if (ier.le.0) call print_error("coordinates", ier)
  call toRealArrayWithDescriptor2d(coordum, coords, DIM, N4)

  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.le.0) call print_error("cutoff", ier)

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.le.0) call print_error("energy", ier)

  ! set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(1)    = kim_api_get_atypecode_f(pkim, "Ar", ier); if (ier.le.0) call print_error("aTypeCode", ier)
  atomTypes(2)    = kim_api_get_atypecode_f(pkim, "Ne", ier); if (ier.le.0) call print_error("aTypeCode", ier)

  ! set up the periodic atom neighbor list
  !
  ! determine how many neighbors we will need
  CellsPerCutoff = ceiling(cutoff/MinSpacing+ 0.05d0) ! the 0.05 is a saftey factor
  NNeighbors = 2*((2*CellsPerCutoff + 1)**3)
  ! compute Rij neighbor list where everything is an image of atom 1
  allocate(neighborList(NNeighbors,N))
  allocate(RijList(3,NNeighbors,N))

  ! generate neighbor list
  !
  CurrentSpacing = MinSpacing
  ! set up coords
  coords(:,1) = 0.0d0
  coords(:,2) = 0.5d0*CurrentSpacing

  call B2_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), CurrentSpacing, NNeighbors, neighborList, RijList)

  ! store pointers to neighbor list object and access function
  allocate(NLRvecLocs(3))
  NLRvecLocs(1) = NNeighbors
  NLRvecLocs(2) = loc(neighborList)
  NLRvecLocs(3) = loc(RijList)
  ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(NLRvecLocs))
  if (ier.le.0) call print_error("neighObject", ier)

  ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_NEIGH_RVEC_neigh))
  if (ier.le.0) call print_error("get_full_heigh", ier)

  ! Call model compute
  call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)

  ! print results to screen
  print *, "***********************************************************************************************"
  print *, "Results for KIM Model: ", modelname
  print *, "Using NBC: NEIGH_RVEC_F"
  print *, ""
  print *, "Energy/cell = ", energy, "; Spacing = ", CurrentSpacing

  ! compute for a range of lattice spacings

  
  ! set up the periodic atom positions
  do while (CurrentSpacing.lt.MaxSpacing)
     ! set new spacing
     CurrentSpacing = CurrentSpacing + SpacingIncr
     ! set up coords
     coords(:,1) = 0.0d0
     coords(:,2) = 0.5d0*CurrentSpacing

     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     call B2_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), CurrentSpacing, NNeighbors, neighborList, RijList)

     ! Call model compute
     call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)

     ! report result
     print *, "Energy/cell = ", energy, "; Spacing = ", CurrentSpacing
  enddo


  ! Don't forget to free and/or deallocate
  deallocate(neighborList)
  deallocate(NLRvecLocs)
  deallocate(RijList)

  call kim_api_model_destroy(pkim, ier); if (ier.le.0) call print_error("model_destroy", ier)
  call kim_api_free(pkim, ier);          if (ier.le.0) call print_error("kim_api_free",  ier)

  stop
end program test_Ar_Ne_periodic_B2_NEIGH_RVEC_f90

!-------------------------------------------------------------------------------
!
! neighbor list functions
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
! B2_NEIGH_RVEC_F_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine B2_NEIGH_RVEC_F_neighborlist(CellsPerHalfSide, cutoff, B2spacing, NN, neighborList, RijList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: CellsPerHalfSide
  double precision,                                    intent(in)  :: cutoff
  double precision,                                    intent(in)  :: B2spacing
  integer,                                             intent(in)  :: NN
  integer, dimension(NN,1),                            intent(out) :: neighborList
  double precision, dimension(3,NN,1),                 intent(out) :: RijList
  
  !-- Local variables
  double precision dx(3)
  double precision r2
  double precision cutoff2
  double precision :: B2shifts(3,2)
  double precision :: latVec(3)
  integer          :: atom, a, i, j, k, m

  cutoff2 = cutoff**2

  ! Cubic B2 cell positions ----------------------------------------------------------------------
  B2shifts(1,1) = 0.d0;           B2shifts(2,1) = 0.d0;           B2shifts(3,1) = 0.d0
  B2shifts(1,2) = 0.5*B2spacing;  B2shifts(2,2) = 0.5*B2spacing;  B2shifts(3,2) = 0.5*B2spacing

  do atom = 1,2
     a = 1
     do i=-CellsPerHalfSide,CellsPerHalfSide
        latVec(1) = i*B2spacing
        do j=-CellsPerHalfSide,CellsPerHalfSide
           latVec(2) = j*B2spacing
           do k=-CellsPerHalfSide,CellsPerHalfSide
              latVec(3) = k*B2spacing
              do m=1,2
                 dx = B2shifts(:,atom) - (latVec + B2shifts(:,m))
                 if (dot_product(dx,dx).lt.cutoff2) then
                    if (.not.( (i.eq.0) .and. (j.eq.0) .and. (k.eq.0) .and. (m.eq.atom) )) then
                       ! we have a neighbor
                       a = a+1
                       neighborList(a,atom) = m
                       RijList(:,a-1,atom)  = dx
                    endif
                 endif
              enddo
           enddo
        enddo
     enddo
     ! atom 1 has a-1 neighbors
     neighborList(1,atom) = a-1
  enddo

end subroutine B2_NEIGH_RVEC_F_neighborlist

!-------------------------------------------------------------------------------
!
! get_NEIGH_RVEC_neigh neighbor list access function 
!
! This function only implements Locator mode
!
!-------------------------------------------------------------------------------
integer function get_NEIGH_RVEC_neigh(pkim,mode,request,atom,numnei,pnei1atom,pRij)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr), intent(in) :: pkim
  integer, intent(in)  :: mode
  integer, intent(in)  :: request
  integer, intent(out) :: atom
  integer, intent(out) :: numnei
  integer :: nei1atom(1); pointer(pnei1atom, nei1atom) ! actual cray pointer associated with nei1atom
  real*8  :: Rij(3,1); pointer(pRij, Rij)
  
  !-- Local variables
  integer   :: NLRvecLocs(1); pointer(pNLRvecLocs,NLRvecLocs)
  integer   :: neighborList(1);  pointer(pneighborList, neighborList)
  double precision :: RijList(1); pointer(pRijList,RijList)
  integer   :: ier
  integer*8 :: numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer   :: N, NN

  ! exit if wrong mode
  if (mode.ne.1) stop "get_NEIGH_RVEC_neigh() only supports locator mode!"
  
  ! unpack neighbor list object
  pNLRVecLocs = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) call print_error("neighObject", ier)
  N             = 2
  NN            = NLRvecLocs(1)
  pneighborList = NLRvecLocs(2)
  pRijList      = NLRvecLocs(3)
  
  ! set the returned atom
  if ( (request.lt.1).or.(request.gt.N)) stop "get_NEIGH_RVEC_heigh() called with invalid request value!"
  atom = request
  
  ! set the returned number of neighbors for the returned atom
  numnei = neighborList( ((atom-1)*NN + 1) )
  
  ! set the location for the returned neighbor list
  pnei1atom = loc(neighborList( ((atom-1)*NN + 2) ))
  
  ! set pointer to Rij to appropriate value
  pRij = loc(RijList( (3*(atom-1)*NN + 1) ))
  
  get_NEIGH_RVEC_neigh = 1
  return
end function get_NEIGH_RVEC_neigh


!-------------------------------------------------------------------------------
!
! print_error subroutine
!
!-------------------------------------------------------------------------------
subroutine print_error(nm, err)
  integer :: err
  character(len=*) :: nm
  if (err.ne.1) then
     print *,"error in: "//nm
     print *,"KIM error code = ",kimerr
     stop
  endif
  return
end subroutine print_error
