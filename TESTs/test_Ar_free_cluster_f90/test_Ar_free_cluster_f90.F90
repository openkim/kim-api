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
!**        PURE-NEIGH-H
!**        PURE-NEIGH-F
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

  integer,          external  :: get_MI_PURE_neigh
  double precision, parameter :: FCCspacing     = 5.26d0 ! in angstroms
  integer,          parameter :: nCellsPerSide  = 2
  integer,          parameter :: DIM            = 3
  integer,          parameter :: ATypes         = 1
  integer,          parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1

  ! neighbor list
  integer, allocatable  :: neighborList(:,:)

  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_free_cluster_f90"
  character*80              :: modelname
  character*64 :: NBC_Method; pointer(pNBC_Method,NBC_Method)
  integer :: nbc  ! 0 - MI-OPBC-H, 1 - MI-OPBC-F, 2 - NEIGH-PURE-H, 3 - NEIGH-PURE-F
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
  ier = kim_api_init_f(pkim, testname, modelname); if (ier.le.0) stop "The given KIM Model name "&
       //"is not a match for this test."

  ! Allocate memory via the KIM system
  call kim_api_allocate_f(pkim, N, ATypes, ier); if (ier.le.0) call print_error("allocate", ier)

  ! call model's init routine
  ier = kim_api_model_init(pkim); if (ier.le.0) call print_error("model_init", ier)

  ! determine which NBC scenerio to use
  pNBC_Method = kim_api_get_nbc_method(pkim, ier); if (ier.le.0) return ! don't forget to free
  if (index(NBC_Method,"MI-OPBC-H").eq.1) then
     nbc = 0
  elseif (index(NBC_Method,"MI-OPBC-F").eq.1) then
     nbc = 1
  elseif (index(NBC_Method,"NEIGH-PURE-H").eq.1) then
     nbc = 2
  elseif (index(NBC_Method,"NEIGH-PURE-F").eq.1) then
     nbc = 3
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

  if (nbc.le.1) then
     pboxlength = kim_api_get_data_f(pkim, "boxlength", ier)
     if (ier.le.0) call print_error("boxlength", ier)
  endif

  penergy = kim_api_get_data_f(pkim, "energy", ier)
  if (ier.le.0) call print_error("energy", ier)

  pforces = kim_api_get_data_f(pkim, "forces", ier)
  if (ier.le.0) call print_error("forces", ier)
  call toRealArrayWithDescriptor2d(forcesdum, forces, DIM, N4)

  ! Set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "Ar", ier); if (ier.le.0) call print_error("aTypeCode", ier)

  ! set up the cluster atom positions
  call create_FCC_cluster(FCCspacing, nCellsPerSide, coords)
  if (nbc.le.1) boxlength(:)  = 600.d0 ! large enough to make the cluster isolated

  ! compute neighbor lists
  allocate(neighborList(N+1, N))
  if (nbc.eq.0) then
     call MI_OPBC_H_neighborlist(N, coords, (cutoff+0.75), boxlength, neighborList)
  elseif (nbc.eq.1) then
     call MI_OPBC_F_neighborlist(N, coords, (cutoff+0.75), boxlength, neighborList)
  elseif (nbc.eq.2) then
     call NEIGH_PURE_H_neighborlist(N, coords, (cutoff+0.75), neighborlist)
  elseif (nbc.eq.3) then
     call NEIGH_PURE_F_neighborlist(N, coords, (cutoff+0.75), neighborlist)
  endif

  ! store pointers to neighbor list object and access function
  ier = kim_api_set_data_f(pkim, "neighObject", 1, loc(neighborList))
  if (ier.le.0) call print_error("neighObject", ier)

  if (nbc.eq.0) then
     ier = kim_api_set_data_f(pkim, "get_half_neigh", 1, loc(get_MI_PURE_neigh))
     if (ier.le.0) call print_error("get_half_heigh", ier)
  elseif (nbc.eq.1) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", 1, loc(get_MI_PURE_neigh))
     if (ier.le.0) call print_error("get_full_heigh", ier)
  elseif (nbc.eq.2) then
     ier = kim_api_set_data_f(pkim, "get_half_neigh", 1, loc(get_MI_PURE_neigh))
     if (ier.le.0) call print_error("get_half_heigh", ier)
  elseif (nbc.eq.3) then
     ier = kim_api_set_data_f(pkim, "get_full_neigh", 1, loc(get_MI_PURE_neigh))
     if (ier.le.0) call print_error("get_full_heigh", ier)
  endif

  ! Call model compute
  call kim_api_model_compute(pkim, ier); if (ier.le.0) call print_error("model_compute", ier)

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
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: numberOfAtoms
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
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: numberOfAtoms
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
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: numberOfAtoms
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
  implicit none
  
  !-- Transferred variables
  integer,                                             intent(in)  :: numberOfAtoms
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
! This function only implements Locator mode
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
  integer   :: neighborListdum(1); pointer(pneighborListdum, neighborListdum)
  integer, pointer :: neighborList(:,:)
  integer   :: ier
  integer*8 :: numberOfAtoms; pointer(pnAtoms, numberOfAtoms)
  integer   :: N

  ! exit if wrong mode
  if (mode.ne.1) stop "get_MI_PURE_neigh() only supports locator mode!"
  
  ! unpack neighbor list object
  pneighborListdum = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) call print_error("neighObject", ier)
  
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier); N = numberOfAtoms
  call toIntegerArrayWithDescriptor2d(neighborListdum, neighborlist, N+1, N)
  
  ! set the returned atom
  atom = request
  
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
  do i=1,2
     latVec(1) = (i-1)*FCCspacing
     do j=1,2
        latVec(2) = (j-1)*FCCspacing
        do k=1,2
           latVec(3) = (k-1)*FCCspacing
           do m=1,4
              a = a+1
              coords(:,a) = latVec + FCCshifts(:,m)
           enddo
        enddo
        ! Add in the remaining three faces
        ! pos-x face
        latVec(1) = 2.d0*FCCspacing; latVec(2) = (i-1)*FCCspacing; latVec(3) = (j-1)*FCCspacing
        a = a+1; coords(:,a) = latVec
        a = a+1; coords(:,a) = latVec + FCCshifts(:,4)
        ! pos-y face
        latVec(1) = (i-1)*FCCspacing; latVec(2) = 2.d0*FCCspacing; latVec(3) = (j-1)*FCCspacing
        a = a+1; coords(:,a) = latVec
        a = a+1; coords(:,a) = latVec + FCCshifts(:,3)
        ! pos-z face
        latVec(1) = (i-1)*FCCspacing; latVec(2) = (j-1)*FCCspacing; latVec(3) = 2.d0*FCCspacing
        a = a+1; coords(:,a) = latVec
        a = a+1; coords(:,a) = latVec + FCCshifts(:,2)
     enddo
     ! Add in the remaining three edges
     latVec(1) = (i-1)*FCCspacing; latVec(2) = 2.d0*FCCspacing; latVec(3) = 2.d0*FCCspacing
     a = a+1; coords(:,a) = latVec
     latVec(1) = 2.d0*FCCspacing; latVec(2) = (i-1)*FCCspacing; latVec(3) = 2.d0*FCCspacing
     a = a+1; coords(:,a) = latVec
     latVec(1) = 2.d0*FCCspacing; latVec(2) = 2.d0*FCCspacing; latVec(3) = (i-1)*FCCspacing
     a = a+1; coords(:,a) = latVec
  enddo
  ! Add in the remaining corner
  a = a+1; coords(:,a) = 2.d0*FCCspacing

end subroutine create_FCC_cluster
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

