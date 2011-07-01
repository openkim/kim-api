!*******************************************************************************
!**
!**  PROGRAM test_Ar_periodic_FCC_NEIGH_PURE_f90
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic FCC crystal of Ar as a 
!**  function of lattice spacing.
!**
!**  Works with the following NBC scenarios:
!**        NEIGH-PURE-H
!**        NEIGH-PURE-F
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
program test_Ar_periodic_FCC_NEIGH_PURE_f90
  use KIMservice
  implicit none

  integer,                  external  :: get_NEIGH_PURE_neigh
  double precision,         parameter :: TOL         = 1.0d-8
  double precision,         parameter :: Golden      = (1.d0 + sqrt(5.d0))/2.d0
  double precision,         parameter :: FCCspacing  = 5.260d0 ! in angstroms
  double precision,         parameter :: MinSpacing  = 0.800d0*FCCspacing
  double precision,         parameter :: MaxSpacing  = 1.200d0*FCCspacing
  double precision,         parameter :: SpacingIncr = 0.025d0*FCCspacing
  integer,                  parameter :: DIM               = 3
  integer,                  parameter :: ATypes            = 1
  integer(kind=kim_intptr), parameter :: SizeOne           = 1

  ! neighbor list
  integer, allocatable          :: neighborList(:,:)

  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_periodic_FCC_NEIGH_PURE_f90"
  character*80              :: modelname
  character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
  integer :: nbc  ! 0- NEIGH-PURE-H, 1- NEIGH-PURE-F
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
  double precision :: Spacings(4)
  double precision :: Energies(4)
  integer          :: MiddleAtomID

  ! Get KIM Model name to use
  print *, "Please enter a valid KIM model name: "
  read(*,*) modelname

  ! Initialize the KIM object
  ier = kim_api_init_f(pkim, testname, modelname); if (ier.le.0) stop "The given KIM Model name "&
       //"is not a match for this test."

  ! Find Model's cutoff  (This procedure needs to be improved in the future)
  !
  ! We need to get `cutoff' before we know how many atoms to use; so here we use 1 atom 
  ! Allocate memory via the KIM system
  N = 1
  call kim_api_allocate_f(pkim, N, ATypes, ier); if (ier.le.0) call print_error("allocate", ier)
  ! call model's init routine
  ier = kim_api_model_init(pkim); if (ier.le.0) call print_error("model_init", ier)
  ! access the `cutoff' argument
  pcutoff = kim_api_get_data_f(pkim, "cutoff", ier)
  if (ier.le.0) call print_error("cutoff", ier)
  ! determine number of atoms needed. (make sure that the middle atom has all its neighbors)
  CellsPerCutoff = ceiling(cutoff/MinSpacing)
  N = 4*((2.d0*CellsPerCutoff)**3);  N4=N
  ! tear it all down so we can put it back up
  call kim_api_model_destroy(pkim, ier); if (ier.le.0) call print_error("model_destroy", ier)
  call kim_api_free(pkim, ier);          if (ier.le.0) call print_error("kim_api_free",  ier)

  ! Now setup for real.
  ier = kim_api_init_f(pkim, testname, modelname); if (ier.le.0) stop "The given KIM Model name "&
       //"is not a match for this test."

  call kim_api_allocate_f(pkim, N, ATypes, ier); if (ier.le.0) call print_error("allocate", ier)
  ! call model's init routine
  ier = kim_api_model_init(pkim); if (ier.le.0) call print_error("model_init", ier)


  ! determine which NBC scenerio to use
  pNBC_Method = kim_api_get_nbc_method(pkim, ier); if (ier.le.0) return ! don't forget to free
  if (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
     nbc = 0
  elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
     nbc = 1
  else
     ier = 0
     return
  endif

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

  ! Set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "Ar", ier); if (ier.le.0) call print_error("aTypeCode", ier)

  ! set up the periodic atom positions
  Spacings(1) = MinSpacing
  call create_FCC_periodic(Spacings(1), 2*CellsPerCutoff, coords, MiddleAtomId)

  ! compute special neighbor list (Only the middle atom will have neighbors)
  allocate(neighborList(N+1, N))
  !
  if (nbc.eq.0) then
     call NEIGH_PURE_H_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
  else
     call NEIGH_PURE_F_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
  endif

  ! store pointers to neighbor list object and access function
  ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(neighborList))
  if (ier.le.0) call print_error("neighObject", ier)

  if (nbc.eq.0) then
     ier = kim_api_set_data_f(pkim, "get_half_neigh", SizeOne, loc(get_NEIGH_PURE_neigh))
     if (ier.le.0) call print_error("get_half_heigh", ier)
  else
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_NEIGH_PURE_neigh))
     if (ier.le.0) call print_error("get_full_heigh", ier)
  endif

  ! print results to screen
  print *, "***********************************************************************************************"
  print *, "Results for KIM Model: ", modelname
  print *, "Using NBC: ", NBC_Method
  print *, ""

  ! Call model compute
  call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)
  if (nbc.eq.0) then ! half neighbor list computes twice the energy
     Energies(1) = energy/2.d0
  else
     Energies(1) = energy
  endif
  print *, "Energy/atom = ", Energies(1), "; Spacing = ", Spacings(2)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  call create_FCC_periodic(Spacings(3), 2*CellsPerCutoff, coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  if (nbc.eq.0) then
     call NEIGH_PURE_H_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
  else
     call NEIGH_PURE_F_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
  endif
  ! Call model compute
  call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)
  if (nbc.eq.0) then ! half neighbor list computes twice the energy
     Energies(3) = energy/2.d0
  else
     Energies(3) = energy
  endif
  print *, "Energy/atom = ", Energies(3), "; Spacing = ", Spacings(2)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0 - Golden)*(MaxSpacing - MinSpacing)
  call create_FCC_periodic(Spacings(2), 2*CellsPerCutoff, coords, MiddleAtomId)
  ! compute new neighbor lists (could be done more intelligently, I'm sure)
  if (nbc.eq.0) then
     call NEIGH_PURE_H_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
  else
     call NEIGH_PURE_F_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
  endif
  ! Call model compute
  call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)
  if (nbc.eq.0) then ! half neighbor list computes twice the energy
     Energies(2) = energy/2.d0
  else
     Energies(2) = energy
  endif
  print *, "Energy/atom = ", Energies(2), "; Spacing = ", Spacings(2)
  

  ! iterate until convergence.


  do while (abs(Spacings(3) - Spacings(1)) .gt. TOL)
     ! set new spacing
     Spacings(4) = (Spacings(1) + Spacings(3)) - Spacings(2)
     ! compute new atom coordinates based on new spacing
     call create_FCC_periodic(Spacings(4), 2*CellsPerCutoff, coords)
     ! compute new neighbor lists (could be done more intelligently, I'm sure)
     if (nbc.eq.0) then
        call NEIGH_PURE_H_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
     else
        call NEIGH_PURE_F_neighborlist(N, coords, (cutoff+0.75), MiddleAtomId, neighborList)
     endif
     ! Call model compute
     call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)
     if (nbc.eq.0) then ! half neighbor list computes twice the energy
        Energies(4) = energy/2.d0
     else
        Energies(4) = energy
     endif
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
  call free(pNBC_Method) 
  deallocate(neighborList)

  call kim_api_model_destroy(pkim, ier); if (ier.le.0) call print_error("model_destroy", ier)
  call kim_api_free(pkim, ier);          if (ier.le.0) call print_error("kim_api_free",  ier)

  stop
end program test_Ar_periodic_FCC_NEIGH_PURE_f90

!-------------------------------------------------------------------------------
!
! neighbor list functions
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
! NEIGH_PURE_H_neighborlist  (only give middle atom neighbors)
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_H_neighborlist(numberOfAtoms, coords, cutoff, MiddleAtomId, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
  integer,                                             intent(in)  :: MiddleAtomId
  integer,   dimension(numberOfAtoms+1,numberOfAtoms), intent(out) :: neighborList ! not memory efficient
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2
  
  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
     do j=i+1,numberOfAtoms
        dx = coords(:, i) - coords(:, j)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           if ((i.eq.MiddleAtomId) .or. (j.eq.MiddleAtomId)) then
              ! atom j is a neighbor of atom i
              a = a+1
              neighborList(a,i) = j
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine NEIGH_PURE_H_neighborlist

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_F_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_F_neighborlist(numberOfAtoms, coords, cutoff, MiddleAtomId, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
  integer,                                             intent(in)  :: MiddleAtomId
  integer,   dimension(numberOfAtoms+1,numberOfAtoms), intent(out) :: neighborList ! not memory efficient
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2
  
  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
     if (i.eq.MiddleAtomId) then ! only the Middle atom gets neighbors
        do j=1,numberOfAtoms
           dx = coords(:, i) - coords(:, j)
           r2 = dot_product(dx, dx)
           if (r2.le.cutoff2) then
              if (i.ne.j) then
                 ! atom j is a neighbor of atom i
                 a = a+1
                 neighborList(a,i) = j
              endif
           endif
        enddo
     endif
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
end subroutine NEIGH_PURE_F_neighborlist

!-------------------------------------------------------------------------------
!
! get_NEIGH_PURE_neigh neighbor list access function 
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
integer function get_NEIGH_PURE_neigh(pkim,mode,request,atom,numnei,pnei1atom,pRij)
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
  integer   :: neighborListdum(1); pointer(pneighborListdum, neighborListdum)
  integer, pointer :: neighborList(:,:)
  integer   :: ier
  integer*8 :: numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer   :: N

  ! unpack neighbor list object
  pneighborListdum = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) call print_error("neighObject", ier)
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier); N = numberOfAtoms
  call toIntegerArrayWithDescriptor2d(neighborListdum, neighborlist, N+1, N)
  
  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_NEIGH_PURE_neigh = 2
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_NEIGH_PURE_neigh = 0
           return
        else
           atomToReturn = iterVal
        endif
     else
        get_NEIGH_PURE_neigh = -6 ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        get_NEIGH_PURE_neigh = -1
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     get_NEIGH_PURE_neigh = -2
     return
  endif

  ! set the returned atom
  atom = atomToReturn
  
  ! set the returned number of neighbors for the returned atom
  numnei = neighborList(1,atom)
  
  ! set the location for the returned neighbor list
  pnei1atom = loc(neighborList(2,atom))
  
  ! set pointer to Rij to NULL
  pRij = 0
  
  get_NEIGH_PURE_neigh = 1
  return
end function get_NEIGH_PURE_neigh


!-------------------------------------------------------------------------------
!
! create_FCC_periodic subroutine
!
!  creates a cubic periodic chunck of FCC atoms with lattice spacing `FCCspacing' and
!  `nCellsPerSide' cells along each direction.  This will result in a total of
!  4*(nCellsPerSide)**3 atoms
!
!-------------------------------------------------------------------------------
subroutine create_FCC_periodic(FCCspacing, nCellsPerSide, coords, MiddleAtomId)
  implicit none

  !-- Transferred variables
  double precision, intent(in)  :: FCCspacing
  integer,          intent(in)  :: nCellsPerSide
  double precision, dimension(3,*), intent(out) :: coords
  integer,          intent(out) :: MiddleAtomId
  !
  ! cluster setup variables
  !
  double precision :: FCCshifts(3,4)
  double precision :: latVec(3)
  integer          :: a, i, j, k, m


  ! Create a cubic FCC cluster of Ar atoms ---------------------------------------------------------
  FCCshifts(1,1) = 0.d0;           FCCshifts(2,1) = 0.d0;           FCCshifts(3,1) = 0.d0
  FCCshifts(1,2) = 0.5*FCCspacing; FCCshifts(2,2) = 0.5*FCCspacing; FCCshifts(3,2) = 0.d0
  FCCshifts(1,3) = 0.5*FCCspacing; FCCshifts(2,3) = 0.d0;           FCCshifts(3,3) = 0.5*FCCspacing
  FCCshifts(1,4) = 0.d0;           FCCshifts(2,4) = 0.5*FCCspacing; FCCshifts(3,4) = 0.5*FCCspacing

  a = 0
  do i=1,nCellsPerSide
     latVec(1) = (i-1)*FCCspacing
     do j=1,nCellsPerSide
        latVec(2) = (j-1)*FCCspacing
        do k=1,nCellsPerSide
           latVec(3) = (k-1)*FCCspacing
           do m=1,4
              a = a+1
              coords(:,a) = latVec + FCCshifts(:,m)
              if ((i.eq.nCellsPerside/2) .and. (j.eq.nCellsPerSide/2) .and. &
                   (k.eq.nCellsPerSide/2) .and. (m.eq.1)) then
                 MiddleAtomId = a
              endif
           enddo
        enddo
     enddo
  enddo

end subroutine create_FCC_periodic
!---------------------------------------------------------------------------------------------------

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
