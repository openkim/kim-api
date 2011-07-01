!*******************************************************************************
!**
!**  PROGRAM test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90
!**
!**  KIM compliant program to compute the energy of one atom in a periodic
!**  FCC crystal of Ar for a range of lattice spacings and model cutoff values.
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
program test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90
  use KIMservice
  implicit none

  integer,                  external  :: get_NEIGH_RVEC_neigh
  double precision,         parameter :: FCCspacing  = 5.260d0 ! in angstroms
  double precision,         parameter :: MinSpacing  = 0.900d0*FCCspacing
  double precision,         parameter :: MaxSpacing  = 1.100d0*FCCspacing
  double precision,         parameter :: SpacingIncr = 0.025d0*FCCspacing
  integer,                  parameter :: DIM               = 3
  integer,                  parameter :: ATypes            = 1
  integer(kind=kim_intptr), parameter :: SizeOne           = 1

  ! neighbor list
  integer, allocatable          :: neighborList(:)
  integer, allocatable          :: NLRvecLocs(:)
  double precision, allocatable :: RijList(:,:)


  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90"
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

  ! Get KIM Model name to use
  print *, "Please enter a valid KIM model name: "
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname); if (ier.le.0) stop "The given KIM Model name "&
       //"is not a match for this test."

  ! Allocate memory via the KIM system
  N = 1; N4 = N ! We'll use just one atom for this calculation!
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

  pparam_cutoff = kim_api_get_data_f(pkim, "PARAM_FREE_cutoff", ier)
  if (ier.le.0) call print_error("PARAM_FREE_cutoff", ier)

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.le.0) call print_error("energy", ier)

  ! set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "Ar", ier); if (ier.le.0) call print_error("aTypeCode", ier)

  ! set up coords
  coords(:,1) = 0.0d0


  MaxCutoff = cutoff + 4.0d0
  do while (cutoff .le. MaxCutoff)
     ! set up the periodic_cut atom neighbor list
     !
     ! determine how many neighbors we will need
     CellsPerCutoff = ceiling(cutoff/MinSpacing+ 0.05d0) ! the 0.05 is a saftey factor
     NNeighbors = 4*((2*CellsPerCutoff + 1)**3)
     ! compute Rij neighbor list where everything is an image of atom 1
     allocate(neighborList(NNeighbors))
     allocate(RijList(3,NNeighbors))
     ! generate neighbor list
     CurrentSpacing = MinSpacing
     call FCC_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), CurrentSpacing, neighborList, RijList)
     
     ! store pointers to neighbor list object and access function
     allocate(NLRvecLocs(2))
     NLRvecLocs(1) = loc(neighborList)
     NLRvecLocs(2) = loc(RijList)
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
     print *, "Energy/atom = ", energy, "; Spacing = ", CurrentSpacing, "; cutoff = ", cutoff
     
     ! compute for a range of lattice spacings
     
     
     ! set up the periodic_cut atom positions
     do while (CurrentSpacing.lt.MaxSpacing)
        ! set new spacing
        CurrentSpacing = CurrentSpacing + SpacingIncr
        ! compute new neighbor lists (could be done more intelligently, I'm sure)
        call FCC_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), CurrentSpacing, neighborList, RijList)
        
        ! Call model compute
        call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)
        
        ! report result
        print *, "Energy/atom = ", energy, "; Spacing = ", CurrentSpacing, "; cutoff = ", cutoff
     enddo
     
     ! increment cutoff and repeat
     param_cutoff = cutoff + 1.0d0
     ier = kim_api_model_reinit(pkim); if (ier.le.0) call print_error("model_reinit", ier)
     ! Don't forget to free and/or deallocate
     deallocate(neighborList)
     deallocate(NLRvecLocs)
     deallocate(RijList)
  enddo
     

  call kim_api_model_destroy(pkim, ier); if (ier.le.0) call print_error("model_destroy", ier)
  call kim_api_free(pkim, ier);          if (ier.le.0) call print_error("kim_api_free",  ier)

  stop
end program test_Ar_periodic_cut_FCC_NEIGH_RVEC_f90

!-------------------------------------------------------------------------------
!
! neighbor list functions
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
! FCC_NEIGH_RVEC_F_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine FCC_NEIGH_RVEC_F_neighborlist(CellsPerHalfSide, cutoff, FCCspacing, neighborList, RijList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: CellsPerHalfSide
  double precision,                                    intent(in)  :: cutoff
  double precision,                                    intent(in)  :: FCCspacing
  integer, dimension(1),                               intent(out) :: neighborList
  double precision, dimension(3,1),                    intent(out) :: RijList
  
  !-- Local variables
  double precision dx(3)
  double precision r2
  double precision cutoff2
  double precision :: FCCshifts(3,4)
  double precision :: latVec(3)
  integer          :: a, i, j, k, m

  cutoff2 = cutoff**2

  ! Cubic FCC cell positions ----------------------------------------------------------------------
  FCCshifts(1,1) = 0.d0;           FCCshifts(2,1) = 0.d0;           FCCshifts(3,1) = 0.d0
  FCCshifts(1,2) = 0.5*FCCspacing; FCCshifts(2,2) = 0.5*FCCspacing; FCCshifts(3,2) = 0.d0
  FCCshifts(1,3) = 0.5*FCCspacing; FCCshifts(2,3) = 0.d0;           FCCshifts(3,3) = 0.5*FCCspacing
  FCCshifts(1,4) = 0.d0;           FCCshifts(2,4) = 0.5*FCCspacing; FCCshifts(3,4) = 0.5*FCCspacing

  a = 1
  do i=-CellsPerHalfSide,CellsPerHalfSide
     latVec(1) = i*FCCspacing
     do j=-CellsPerHalfSide,CellsPerHalfSide
        latVec(2) = j*FCCspacing
        do k=-CellsPerHalfSide,CellsPerHalfSide
           latVec(3) = k*FCCspacing
           do m=1,4
              dx = -latVec - FCCshifts(:,m)
              if (dot_product(dx,dx).lt.cutoff2) then
                 if (.not.( (i.eq.0) .and. (j.eq.0) .and. (k.eq.0) .and. (m.eq.1) )) then
                    ! we have a neighbor
                    a = a+1
                    neighborList(a) = 1
                    RijList(:,a-1) = dx
                 endif
              endif
           enddo
        enddo
     enddo
  enddo
  ! atom 1 has a-1 neighbors
  neighborList(1) = a-1

end subroutine FCC_NEIGH_RVEC_F_neighborlist

!-------------------------------------------------------------------------------
!
! get_NEIGH_RVEC_neigh neighbor list access function 
!
! This function implements Locator and Iterator mode
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
  integer, save :: iterVal = 0
  integer   :: atomToReturn
  integer   :: NLRvecLocs(1); pointer(pNLRvecLocs,NLRvecLocs)
  integer   :: neighborList(1);  pointer(pneighborList, neighborList)
  double precision :: RijList(1); pointer(pRijList,RijList)
  integer   :: ier
  integer*8 :: numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer   :: N

  ! unpack neighbor list object
  pNLRVecLocs = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) call print_error("neighObject", ier)
  pneighborList = NLRvecLocs(1)
  pRijList      = NLRvecLocs(2)

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_NEIGH_RVEC_neigh = 2
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.1) then
           get_NEIGH_RVEC_neigh = 0
           return
        else
           atomToReturn = iterVal
        endif
     else
        get_NEIGH_RVEC_neigh = -6 ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        get_NEIGH_RVEC_neigh = -1
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     get_NEIGH_RVEC_neigh = -2
     return
  endif
  
  ! set the returned atom
  if (atomToReturn.ne.1) stop "get_NEIGH_RVEC_heigh() called with invalid request value!"
  atom = 1
  
  ! set the returned number of neighbors for the returned atom
  numnei = neighborList(1)
  
  ! set the location for the returned neighbor list
  pnei1atom = loc(neighborList(2))
  
  ! set pointer to Rij to appropriate value
  pRij = loc(RijList(1))
  
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
