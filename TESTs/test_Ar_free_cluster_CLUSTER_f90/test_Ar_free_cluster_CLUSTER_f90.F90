!*******************************************************************************
!**
!**  PROGRAM test_Ar_free_cluster_CLUSTER_f90
!**
!**  KIM compliant program to compute the energy of and forces on an isolated 
!**  cluster of Ar atoms
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
program test_Ar_free_cluster_CLUSTER_f90
  use KIMservice
  implicit none

  double precision,         parameter :: FCCspacing     = 5.26d0 ! in angstroms
  integer,                  parameter :: nCellsPerSide  = 2
  integer,                  parameter :: DIM            = 3
  integer,                  parameter :: ATypes         = 1
  integer(kind=kim_intptr), parameter :: &
       N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1

  !
  ! KIM variables
  !
  character*80              :: testname     = "test_Ar_free_cluster_CLUSTER_f90"
  character*80              :: modelname
  integer(kind=kim_intptr)  :: pkim
  integer                   :: ier
  integer(kind=8) numberOfAtoms; pointer(pnAtoms,numberOfAtoms)
  integer numberAtomTypes;      pointer(pnAtomTypes,numberAtomTypes)
  integer atomTypesdum(1); pointer(patomTypesdum,atomTypesdum)

  real*8 cutoff;           pointer(pcutoff,cutoff)
  real*8 energy;           pointer(penergy,energy)
  real*8 coordum(DIM,1);   pointer(pcoor,coordum)
  real*8 forcesdum(DIM,1); pointer(pforces,forcesdum)
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

  ! Call model compute
  call kim_api_model_compute(pkim, ier)
  if (ier.le.0) then
     call report_error(__LINE__, "kim_api_model_compute", ier)
     stop
  endif

  ! print results to screen
  print *, "***********************************************************************************************"
  print *, "Results for KIM Model: ", modelname
  print *, "Forces:"
  print *, "  X                   Y                   Z"
  print 10, forces
10 format(f20.15, f20.15, f20.15)
  print *, ""
  print *, "Energy = ", energy


  ! don't forget to destroy and deallocate
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
end program test_Ar_free_cluster_CLUSTER_f90

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
