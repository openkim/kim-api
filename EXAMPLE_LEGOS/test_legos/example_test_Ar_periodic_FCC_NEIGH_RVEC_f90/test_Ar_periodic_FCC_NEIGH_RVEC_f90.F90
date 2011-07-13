!******************************************************************************* 
!**
!**  PROGRAM test_Ar_periodic_FCC_NEIGH_RVEC_f90
!**
!**  KIM compliant program to find (using the Golden section search algorithm)
!**  the minimum energy of one atom in a periodic FCC crystal of Ar as a 
!**  function of lattice spacing.
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
program test_Ar_periodic_FCC_NEIGH_RVEC_f90
  use KIMservice
  implicit none

  integer,                  external  :: get_NEIGH_RVEC_neigh
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
  integer, allocatable          :: neighborList(:)
  integer, allocatable          :: NLRvecLocs(:)
  double precision, allocatable :: RijList(:,:)


  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_periodic_FCC_NEIGH_RVEC_f90"
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
  ier = kim_api_model_init(pkim)
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

  ! set values
  numberOfAtoms   = N
  numberAtomTypes = ATypes
  atomTypes(:)    = kim_api_get_atypecode_f(pkim, "Ar", ier)
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
  allocate(neighborList(NNeighbors))
  allocate(RijList(3,NNeighbors))
  ! generate neighbor list
  Spacings(1) = MinSpacing
  call FCC_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(1), neighborList, RijList)

  ! store pointers to neighbor list object and access function
  allocate(NLRvecLocs(2))
  NLRvecLocs(1) = loc(neighborList)
  NLRvecLocs(2) = loc(RijList)
  ier = kim_api_set_data_f(pkim, "neighObject", SizeOne, loc(NLRvecLocs))
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_set_data_f", ier)
     stop
  endif

  ier = kim_api_set_data_f(pkim, "get_full_neigh", SizeOne, loc(get_NEIGH_RVEC_neigh))
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
  call kim_api_model_compute(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif
  Energies(1) = energy
  print *, "Energy/atom = ", Energies(1), "; Spacing = ", Spacings(1)

  ! setup and compute for max spacing
  Spacings(3) = MaxSpacing
  call FCC_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(3), neighborList, RijList)
  ! Call model compute
  call kim_api_model_compute(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif
  Energies(3) = energy
  print *, "Energy/atom = ", Energies(3), "; Spacing = ", Spacings(3)

  ! setup and compute for first intermediate spacing
  Spacings(2) = MinSpacing + (2.0 - Golden)*(MaxSpacing - MinSpacing)
  call FCC_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(2), neighborList, RijList)
  ! Call model compute
  call kim_api_model_compute(pkim, ier)
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
     call FCC_NEIGH_RVEC_F_neighborlist(CellsPerCutoff, (cutoff+0.75), Spacings(4), neighborList, RijList)
     ! Call model compute
     call kim_api_model_compute(pkim, ier)
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
end program test_Ar_periodic_FCC_NEIGH_RVEC_f90

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
  pnAtoms = kim_api_get_data_f(pkim, "numberOfAtoms", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data", ier)
     stop
  endif
  N = numberOfAtoms
  pNLRVecLocs = kim_api_get_data_f(pkim, "neighObject", ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_get_data", ier)
     stop
  endif
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
        call report_error(__LINE__, "Invalid request in get_NEIGH_RVEC_neigh", -6)
        get_NEIGH_RVEC_neigh = -6 ! invalid request value
        return
     endif
  elseif (mode.eq.1) then ! locator mode
     if ((request.gt.N) .or. (request.lt.1)) then
        call report_error(__LINE__, "Invalid request in get_NEIGH_RVEC_neigh", -1)
        get_NEIGH_RVEC_neigh = -1
        return
     else
        atomToReturn = request
     endif
  else ! not iterator or locator mode
     call report_error(__LINE__, "Invalid mode in get_NEIGH_RVEC_neigh", -2)
     get_NEIGH_RVEC_neigh = -2
     return
  endif

  ! set the returned atom
  atom = atomToReturn
  
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
