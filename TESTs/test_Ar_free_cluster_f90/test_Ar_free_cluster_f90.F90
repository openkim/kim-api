!*******************************************************************************
!**
!**  PROGRAM test_Ar_free_cluster_f90
!**
!**  KIM compliant program to compute the energy of and forces on an isolated 
!**  cluster of Ar atoms
!**
!**  Works with the following NBC scenarios:
!**        MI-OPBC-H
!**        MI-OPBC-F
!**        NEIGH-PURE-H
!**        NEIGH-PURE-F
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
program test_Ar_free_cluster_f90
  use KIMservice
  implicit none

  integer,                  external  :: get_MI_PURE_neigh
  integer,                  external  :: get_RVEC_neigh
  double precision,         parameter :: FCCspacing     = 5.26d0 ! in angstroms
  integer,                  parameter :: nCellsPerSide  = 2
  integer,                  parameter :: DIM            = 3
  integer,                  parameter :: ATypes         = 1
  integer(kind=kim_intptr), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(kind=kim_intptr), parameter :: SizeOne        = 1

  ! neighbor list
  integer, allocatable          :: neighborList(:,:)
  integer, allocatable          :: NLRvecLocs(:)
  double precision, allocatable :: RijList(:,:,:)

  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_free_cluster_f90"
  character*80              :: modelname
  character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
  integer :: nbc  ! 0- MI-OPBC-H, 1- MI-OPBC-F, 2- NEIGH-PURE-H, 3- NEIGH-PURE-F, 4- NEIGH-RVCE-F
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier
  integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
  integer numberAtomTypes;      pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1); pointer(patomTypesdum,atomTypesdum)

  real*8 cutoff;           pointer(pcutoff,cutoff)
  real*8 energy;           pointer(penergy,energy)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  real*8 forcesdum(DIM,1); pointer(pforces,forcesdum)
  real*8 boxlength(DIM);   pointer(pboxlength,boxlength)
  integer N4
  real*8, pointer  :: coords(:,:), forces(:,:)
  integer, pointer :: atomTypes(:)
  N4 = N

  
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
  call kim_api_allocate_f(pkim, N, ATypes, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_allocate_f", ier)
     stop
  endif

  ! call model's init routine
  ier = kim_api_model_init(pkim)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_init", ier)
     stop
  endif

  ! determine which NBC scenerio to use
  pNBC_Method = kim_api_get_nbc_method(pkim, ier) ! don't forget to free
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"MI-OPBC-H").eq.1) then
     nbc = 0
  elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
     nbc = 1
  elseif (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
     nbc = 2
  elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
     nbc = 3
  elseif (index(NBC_Method,"NEIGH-RVEC-F").eq.1) then
     nbc = 4
  else
     ier = 0
     call report_error(__LINE__, "Unknown NBC method", ier)
     return
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

  if (nbc.le.1) then
     pboxlength = kim_api_get_data_f(pkim, "boxlength", ier)
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_get_data_f", ier)
        stop
     endif
  endif

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif

  pforces = kim_api_get_data_f(pkim, "forces", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  call toRealArrayWithDescriptor2d(forcesdum, forces, DIM, N4)

  ! Set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "Ar", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_atypecode_f", ier)
     stop
  endif

  ! set up the cluster atom positions
  call create_FCC_cluster(FCCspacing, nCellsPerSide, coords)
  if (nbc.le.1) boxlength(:)  = 600.d0 ! large enough to make the cluster isolated

  ! compute neighbor lists
  allocate(neighborList(N+1, N))
  if (nbc.eq.4) then
     allocate(RijList(3,N,N))
  endif
  !
  if (nbc.eq.0) then
     call MI_OPBC_H_neighborlist(N, coords, (cutoff+0.75), boxlength, neighborList)
  elseif (nbc.eq.1) then
     call MI_OPBC_F_neighborlist(N, coords, (cutoff+0.75), boxlength, neighborList)
  elseif (nbc.eq.2) then
     call NEIGH_PURE_H_neighborlist(N, coords, (cutoff+0.75), neighborList)
  elseif (nbc.eq.3) then
     call NEIGH_PURE_F_neighborlist(N, coords, (cutoff+0.75), neighborList)
  elseif (nbc.eq.4) then
     call NEIGH_RVEC_F_neighborlist(N, coords, (cutoff+0.75), neighborList, RijList)
  endif

  ! store pointers to neighbor list object and access function
  if (nbc.le.3) then
     ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(neighborList))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  else
     allocate(NLRvecLocs(2))
     NLRvecLocs(1) = loc(neighborList)
     NLRvecLocs(2) = loc(RijList)
     ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(NLRvecLocs))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  endif

  if (nbc.eq.0) then
     ier = kim_api_set_data_f(pkim, "get_half_neigh", SizeOne, loc(get_MI_PURE_neigh))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.1) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_MI_PURE_neigh))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.2) then
     ier = kim_api_set_data_f(pkim, "get_half_neigh", SizeOne, loc(get_MI_PURE_neigh))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.3) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_MI_PURE_neigh))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  elseif (nbc.eq.4) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_RVEC_neigh))
     if (ier.le.0) then
        call report_error(__LINE__, "kim_api_set_data_f", ier)
        stop
     endif
  endif

  ! Call model compute
  call kim_api_model_compute(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif

  ! print results to screen
  print *, "***********************************************************************************************"
  print *, "Results for KIM Model: ", modelname
  print *, "Using NBC: ", NBC_Method
  print *, "Forces:"
  print *, "  X                   Y                   Z"
  print 10, forces
10 format(f20.15, f20.15, f20.15)
  print *, ""
  print *, "Energy = ", energy


  ! Don't forget to free and/or deallocate
  call free(pNBC_Method) 
  deallocate(neighborList)
  if (nbc.eq.4) then
     deallocate(NLRvecLocs)
     deallocate(RijList)
  endif

  call kim_api_model_destroy(pkim, ier)
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
end program test_Ar_free_cluster_f90

!-------------------------------------------------------------------------------
!
! neighbor list functions
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
! MI_OPBC_H_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_H_neighborlist(numberOfAtoms, coords, cutoff, boxlength, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
  double precision, dimension(3),                      intent(in)  :: boxlength
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
        where (abs(dx) > 0.5d0*boxlength)  ! apply PBC
           dx = dx - sign(boxlength,dx)
        endwhere
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! atom j is a neighbor of atom i
           a = a+1
           neighborList(a,i) = j
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine MI_OPBC_H_neighborlist

!-------------------------------------------------------------------------------
!
! MI_OPBC_F_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine MI_OPBC_F_neighborlist(numberOfAtoms, coords, cutoff, boxlength, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
  double precision, dimension(3),                      intent(in)  :: boxlength
  integer,   dimension(numberOfAtoms+1,numberOfAtoms), intent(out) :: neighborList ! not memory efficient
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2
  
  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
     do j=1,numberOfAtoms
        dx = coords(:, i) - coords(:, j)
        where (abs(dx) > 0.5d0*boxlength)  ! apply PBC
           dx = dx - sign(boxlength,dx)
        endwhere
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           if (i.ne.j) then
              ! atom j is a neighbor of atom i
              a = a+1
              neighborList(a,i) = j
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine MI_OPBC_F_neighborlist

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_H_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_H_neighborlist(numberOfAtoms, coords, cutoff, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
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
           ! atom j is a neighbor of atom i
           a = a+1
           neighborList(a,i) = j
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
subroutine NEIGH_PURE_F_neighborlist(numberOfAtoms, coords, cutoff, neighborList)
  use KIMservice
  implicit none
  
  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),        intent(in)  :: coords
  double precision,                                    intent(in)  :: cutoff
  integer,   dimension(numberOfAtoms+1,numberOfAtoms), intent(out) :: neighborList ! not memory efficient
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2
  
  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
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
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine NEIGH_PURE_F_neighborlist

!-------------------------------------------------------------------------------
!
! get_MI_PURE_neigh neighbor list access function 
!   (works for MI_OPBC and NEIGH_PURE and both full and half)
!
! This function implements Locator and Iterator modes
!
!-------------------------------------------------------------------------------
integer function get_MI_PURE_neigh(pkim,mode,request,atom,numnei,pnei1atom,pRij)
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
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  N = numberOfAtoms
  call toIntegerArrayWithDescriptor2d(neighborListdum, neighborlist, N+1, N)
  
  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_MI_PURE_neigh = 2
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_MI_PURE_neigh = 0
           return
        else
           atomToReturn = iterVal
        endif
     else
        call report_error(__LINE__, "Invalid request in get_MI_PURE_neigh", -6)
        get_MI_PURE_neigh = -6 ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        call report_error(__LINE__, "Invalid request in get_MI_PURE_neigh", -1)
        get_MI_PURE_neigh = -1
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     call report_error(__LINE__, "Invalid mode in get_MI_PURE_neigh", -2)
     get_MI_PURE_neigh = -2
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
  
  get_MI_PURE_neigh = 1
  return
end function get_MI_PURE_neigh

!-------------------------------------------------------------------------------
!
! NEIGH_RVEC_F_neighborlist 
!
!-------------------------------------------------------------------------------
subroutine NEIGH_RVEC_F_neighborlist(numberOfAtoms, coords, cutoff, neighborList, RijList)
  use KIMservice
  implicit none

  !-- Transferred variables
  integer(kind=kim_intptr),                            intent(in)  :: numberOfAtoms
  double precision, dimension(3,numberOfAtoms),               intent(in)  :: coords
  double precision,                                           intent(in)  :: cutoff
  integer, dimension(numberOfAtoms+1,numberOfAtoms),          intent(out) :: neighborList
  double precision, dimension(3,numberOfAtoms,numberOfAtoms), intent(out) :: RijList
  
  !-- Local variables
  integer i, j, a
  double precision dx(3)
  double precision r2
  double precision cutoff2

  cutoff2 = cutoff**2
  
  do i=1,numberOfAtoms
     a = 1
     do j=1,numberOfAtoms
        dx = coords(:, i) - coords(:, j)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           if (i.ne.j) then
              ! atom j is a neighbor of atom i
              a = a+1
              neighborList(a,i) = j
              RijList(:,a-1,i) = dx
           endif
        endif
     enddo
     ! atom i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo
  
end subroutine NEIGH_RVEC_F_neighborlist

!-------------------------------------------------------------------------------
!
! get_RVEC_neigh neighbor list access function 
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
integer function get_RVEC_neigh(pkim,mode,request,atom,numnei,pnei1atom,pRij)
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
  integer   :: neighborListdum(1);  pointer(pneighborListdum, neighborListdum)
  integer, pointer :: neighborList(:,:)
  double precision :: RijList(1); pointer(pRijList,RijList)
  integer   :: ier
  integer*8 :: numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer   :: N

  ! unpack neighbor list object
  pNLRVecLocs = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  pneighborListdum = NLRvecLocs(1)
  pRijList         = NLRvecLocs(2)
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data_f", ier)
     stop
  endif
  N = numberOfAtoms
  call toIntegerArrayWithDescriptor2d(neighborListdum, neighborlist, N+1, N)

  ! check mode and request
  if (mode.eq.0) then ! iterator mode
     if (request.eq.0) then ! reset iterator
        iterVal = 0
        get_RVEC_neigh = 2
        return
     elseif (request.eq.1) then ! increment iterator
        iterVal = iterVal + 1
        if (iterVal.gt.N) then
           get_RVEC_neigh = 0
           return
        else
           atomToReturn = iterVal
        endif
     else
        call report_error(__LINE__, "Invalid request in get_RVEC_neigh", -6)
        get_RVEC_neigh = -6 ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        call report_error(__LINE__, "Invalid request in get_RVEC_neigh", -1)
        get_RVEC_neigh = -1
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     call report_error(__LINE__, "Invalid mode in get_RVEC_neigh", -2)
     get_RVEC_neigh = -2
     return
  endif

  ! set the returned atom
  atom = atomToReturn
  
  ! set the returned number of neighbors for the returned atom
  numnei = neighborList(1,atom)
  
  ! set the location for the returned neighbor list
  pnei1atom = loc(neighborList(2,atom))
  
  ! set pointer to Rij to appropriate value
  pRij = loc(RijList(3*(numberOfAtoms)*(atom-1) + 1))
  
  get_RVEC_neigh = 1
  return
end function get_RVEC_neigh


!-------------------------------------------------------------------------------
!
! create_FCC_cluster subroutine
!
!  creates a cubic cluster of FCC atoms with lattice spacing `FCCspacing' and
!  `nCellsPerSide' cells along each direction.  This will result in a total of
!  4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
!
!-------------------------------------------------------------------------------
subroutine create_FCC_cluster(FCCspacing, nCellsPerSide, coords)
  implicit none

  !-- Transferred variables
  double precision, intent(in)  :: FCCspacing
  integer,          intent(in)  :: nCellsPerSide
  double precision, dimension(3,*), intent(out) :: coords
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
           enddo
        enddo
        ! Add in the remaining three faces
        ! pos-x face
        latVec(1) = nCellsPerSide*FCCspacing
        latVec(2) = (i-1)*FCCspacing
        latVec(3) = (j-1)*FCCspacing
        a = a+1; coords(:,a) = latVec
        a = a+1; coords(:,a) = latVec + FCCshifts(:,4)
        ! pos-y face
        latVec(1) = (i-1)*FCCspacing
        latVec(2) = nCellsPerSide*FCCspacing
        latVec(3) = (j-1)*FCCspacing
        a = a+1; coords(:,a) = latVec
        a = a+1; coords(:,a) = latVec + FCCshifts(:,3)
        ! pos-z face
        latVec(1) = (i-1)*FCCspacing
        latVec(2) = (j-1)*FCCspacing
        latVec(3) = nCellsPerSide*FCCspacing
        a = a+1; coords(:,a) = latVec
        a = a+1; coords(:,a) = latVec + FCCshifts(:,2)
     enddo
     ! Add in the remaining three edges
     latVec(1) = (i-1)*FCCspacing
     latVec(2) = nCellsPerSide*FCCspacing
     latVec(3) = nCellsPerSide*FCCspacing
     a = a+1; coords(:,a) = latVec
     latVec(1) = nCellsPerSide*FCCspacing
     latVec(2) = (i-1)*FCCspacing
     latVec(3) = nCellsPerSide*FCCspacing
     a = a+1; coords(:,a) = latVec
     latVec(1) = nCellsPerSide*FCCspacing
     latVec(2) = nCellsPerSide*FCCspacing
     latVec(3) = (i-1)*FCCspacing
     a = a+1; coords(:,a) = latVec
  enddo
  ! Add in the remaining corner
  a = a+1; coords(:,a) = nCellsPerSide*FCCspacing

end subroutine create_FCC_cluster
!---------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!
! report_error subroutine
!
!-------------------------------------------------------------------------------
subroutine report_error(line, str, status)
  implicit none
  
  !-- Transferred variables
  integer,   intent(in) :: line
  character(len=*), intent(in) :: str
  integer,   intent(in) :: status
  
  !-- Local variables
  character(len=10000), parameter :: file = __FILE__
  
  !-- print the error message
  print *,'* ERROR at line', line, 'in ',trim(file), ': ', str,'. kimerror =', status
  
end subroutine report_error
