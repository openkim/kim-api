!
! CDDL HEADER START
!
! The contents of this file are subject to the terms of the Common Development
! and Distribution License Version 1.0 (the "License").
!
! You can obtain a copy of the license at
! http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
! specific language governing permissions and limitations under the License.
!
! When distributing Covered Code, include this CDDL HEADER in each file and
! include the License file in a prominent location with the name LICENSE.CDDL.
! If applicable, add the following below this CDDL HEADER, with the fields
! enclosed by brackets "[]" replaced with your own identifying information:
!
! Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
!
! CDDL HEADER END
!

!
! Copyright (c) 2013--2018, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!    Ryan S. Elliott
!    Stephen M. Whalen
!


#include "KIM_API_status.h"
#define THIS_FILE_NAME __FILE__
#define TRUEFALSE(TRUTH) merge(1,0,(TRUTH))

!-------------------------------------------------------------------------------
!
! module mod_neighborlist :
!
!    Module contains type and routines related to neighbor list calculation
!
!-------------------------------------------------------------------------------

module mod_neighborlist

  use, intrinsic :: iso_c_binding
  use KIM_API_F03

  public get_neigh

  type neighObject_type
     integer(c_int), pointer :: neighborList(:,:)
  end type neighObject_type
contains

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
integer(c_int) function get_neigh(pkim,mode,request,part,numnei,pnei1part, &
                                  pRij) bind(c)
  implicit none

  !-- Transferred variables
  type(c_ptr),    intent(in)  :: pkim
  integer(c_int), intent(in)  :: mode
  integer(c_int), intent(in)  :: request
  integer(c_int), intent(out) :: part
  integer(c_int), intent(out) :: numnei
  type(c_ptr),    intent(out) :: pnei1part
  type(c_ptr),    intent(out) :: pRij

  !-- Local variables
  integer(c_int), parameter :: DIM = 3
  integer(c_int)  N
  integer(c_int)  partToReturn
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnParts
  type(neighObject_type), pointer :: neighObject; type(c_ptr) :: pneighObject
  integer(c_int)  ier, idum

  ! unpack number of particles
  pnParts = kim_api_get_data(pkim, "numberOfParticles", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pnParts, numberOfParticles)

  ! unpack neighbor list object
  pneighObject = kim_api_get_data(pkim, "neighObject", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_data", ier)
     stop
  endif
  call c_f_pointer(pneighObject, neighObject)

  N = size(neighObject%neighborList, 2)

  ! check mode and request
  if (mode.eq.1) then ! locator mode
     if ( (request.gt.N) .or. (request.lt.1)) then
        idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                    "Invalid part ID in get_neigh", &
                                    KIM_STATUS_PARTICLE_INVALID_ID)
        get_neigh = KIM_STATUS_PARTICLE_INVALID_ID
        return
     else
        partToReturn = request
     endif
  else ! not iterator or locator mode
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Invalid mode in get_neigh", &
                                 KIM_STATUS_NEIGH_INVALID_MODE)
     get_neigh = KIM_STATUS_NEIGH_INVALID_MODE
     return
  endif

  ! set the returned part
  part = partToReturn

  ! set the returned number of neighbors for the returned part
  numnei = neighObject%neighborList(1,part)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighObject%neighborList(2,part))

  pRij = c_null_ptr

  get_neigh = KIM_STATUS_OK
  return
end function get_neigh

end module mod_neighborlist


module mod_utility
  implicit none
  public

contains

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_cluster_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_cluster_neighborlist(half, numberOfParticles, coords, &
                                           cutoff, neighObject)
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  logical,        intent(in)            :: half
  integer(c_int), intent(in)            :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                  intent(in)            :: coords
  real(c_double), intent(in)            :: cutoff
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2

  cutoff2 = cutoff**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! part j is a neighbor of part i
           if ( (j .gt. i) .OR. ((.not. half) .AND. (i.ne.j)) ) then
               a = a+1
               neighObject%neighborList(a,i) = j
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighObject%neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_PURE_cluster_neighborlist

!-------------------------------------------------------------------------------
!
! create_FCC_configuration subroutine
!
!  creates a cubic configuration of FCC particles with lattice spacing
!  `FCCspacing' and `nCellsPerSide' cells along each direction.
!
!  With periodic==.true. this will result in a total number of particles equal
!  to 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
!
!  With periodic==.false. this will result in a total number of particles equal
!  to 4*(nCellsPerSide)**3
!
!  Returns the Id of the particle situated in the middle of the configuration
!  (this particle will have the most neighbors.)
!
!-------------------------------------------------------------------------------
subroutine create_FCC_configuration(FCCspacing, nCellsPerSide, periodic, &
                                    coords, MiddlePartId)
  use, intrinsic :: iso_c_binding
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  !-- Transferred variables
  real(c_double), intent(in)  :: FCCspacing
  integer(c_int), intent(in)  :: nCellsPerSide
  logical,        intent(in)  :: periodic
  real(c_double), intent(out) :: coords(3,*)
  integer(c_int), intent(out) :: MiddlePartId
  !
  ! cluster setup variables
  !
  real(c_double) FCCshifts(3,4)
  real(c_double) latVec(3)
  integer(c_int) a, i, j, k, m

  ! Create a cubic FCC cluster
  !
  FCCshifts(1,1) = 0.0_cd
  FCCshifts(2,1) = 0.0_cd
  FCCshifts(3,1) = 0.0_cd
  FCCshifts(1,2) = 0.5_cd*FCCspacing
  FCCshifts(2,2) = 0.5_cd*FCCspacing
  FCCshifts(3,2) = 0.0_cd
  FCCshifts(1,3) = 0.5_cd*FCCspacing
  FCCshifts(2,3) = 0.0_cd
  FCCshifts(3,3) = 0.5_cd*FCCspacing
  FCCshifts(1,4) = 0.0_cd
  FCCshifts(2,4) = 0.5_cd*FCCspacing
  FCCshifts(3,4) = 0.5_cd*FCCspacing

  MiddlePartID = 1 ! Always put middle particle as #1
  a = 1            ! leave space for middle particle as particle #1
  do i=1,nCellsPerSide
     latVec(1) = (i-1)*FCCspacing
     do j=1,nCellsPerSide
        latVec(2) = (j-1)*FCCspacing
        do k=1,nCellsPerSide
           latVec(3) = (k-1)*FCCspacing
           do m=1,4
              a = a+1
              coords(:,a) = latVec + FCCshifts(:,m)
              if ((i.eq.nCellsPerside/2+1).and.(j.eq.nCellsPerSide/2+1) .and. &
                   (k.eq.nCellsPerSide/2+1) .and. (m.eq.1)) then
                 coords(:,1) = latVec + FCCshifts(:,m) ! put middle particle as #1
                 a = a - 1
              endif
           enddo
        enddo
        if (.not. periodic) then
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
         endif
     enddo
     if (.not. periodic) then
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
      endif
  enddo
  if (.not. periodic) then
      ! Add in the remaining corner
      a = a+1; coords(:,a) = nCellsPerSide*FCCspacing
  endif

  return

end subroutine create_FCC_configuration

end module mod_utility

!*******************************************************************************
!**
!**  PROGRAM vc_forces_numer_deriv
!**
!**  KIM compliant program to perform numerical derivative check on a model
!**
!*******************************************************************************

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program ex_test_ar_fcc_cluster_fortran
  use, intrinsic :: iso_c_binding
  use KIM_API_F03
  use mod_neighborlist
  use mod_utility
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: nCellsPerSide  = 2
  integer(c_int), parameter :: DIM            = 3
  integer(c_int), parameter :: ASpecies       = 1

  real(c_double), parameter :: cutpad         = 0.75_cd
  real(c_double), parameter :: FCCspacing     = 5.260_cd
  real(c_double), parameter :: min_spacing    = 0.8*FCCspacing
  real(c_double), parameter :: max_spacing    = 1.2*FCCspacing
  real(c_double), parameter :: spacing_incr   = 0.025*FCCspacing
  real(c_double) :: current_spacing
  real(c_double) :: force_norm

  integer(c_int), parameter :: &
    N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  integer(c_int), parameter :: SizeOne        = 1

  type(neighObject_type), target :: neighObject

  !
  ! KIM variables
  !
  character(len=KIM_KEY_STRING_LENGTH) :: testkimfile = "descriptor.kim"
  character(len=KIM_KEY_STRING_LENGTH) :: modelname
  character(len=KIM_KEY_STRING_LENGTH) :: NBC_Method

  type(c_ptr) :: pkim
  integer(c_int), pointer :: numberOfParticles;   type(c_ptr) :: pnParts
  integer(c_int), pointer :: numberOfSpecies;     type(c_ptr) :: pnOfSpecies
  integer(c_int), pointer :: particleSpecies(:);  type(c_ptr) :: pparticleSpecies
  real(c_double), pointer :: cutoff;              type(c_ptr) :: pcutoff
  real(c_double), pointer :: energy;              type(c_ptr) :: penergy
  real(c_double), pointer :: coords(:,:);         type(c_ptr) :: pcoor
  real(c_double), pointer :: forces(:,:);         type(c_ptr) :: pforces


  integer(c_int) ier, idum
  integer :: middledum
  integer :: i, j

  ! Initialize error flag
  ier = 0

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Print output header
  !
  print *
  print *,'This is Test : ex_test_Ar_fcc_cluster_fortran'
  print *
  print '(80(''-''))'
  print '("Results for KIM Model : ",A)', trim(modelname)

  ! Initialize the KIM object
  ier = kim_api_file_init(pkim, testkimfile, modelname)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_file_init", ier)
     stop
  endif

  ! make sure NEIGH_PURE_F
  ier = kim_api_get_nbc_method(pkim, NBC_Method)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_nbc_method", ier)
     stop
  endif
  if (index(NBC_Method,"NEIGH_PURE_F").ne.1) then
     ier = KIM_STATUS_FAIL
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "Unknown NBC method", ier)
     stop
  endif

  ! Allocate memory via the KIM system
  call kim_api_allocate(pkim, N, ASpecies, ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_allocate", ier)
     stop
  endif

  ! Allocate storage for neighbor lists
  !
  allocate(neighObject%neighborList(N+1,N))

  ! Set pointer in KIM object to neighbor list routine and object
  !
  ier = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(neighObject))
  if (ier.lt.KIM_STATUS_OK) then
    idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                "kim_api_set_data", ier)
    stop
  endif

  ier = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh))
  if (ier.lt.KIM_STATUS_OK) then
    idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                "kim_api_set_method", ier)
    stop
  endif

  ! call model's init routine
  ier = kim_api_model_init(pkim)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_model_init", ier)
     stop
  endif


  ! Unpack data from KIM object
  !
  call kim_api_getm_data(pkim, ier, &
       "numberOfParticles",           pnParts,          1,                                   &
       "numberOfSpecies",             pnOfSpecies,      1,                                   &
       "particleSpecies",             pparticleSpecies, 1,                                   &
       "coordinates",                 pcoor,            1,                                   &
       "cutoff",                      pcutoff,          1,                                   &
       "energy",                      penergy,          1,                                   &
       "forces",                      pforces,          1)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_getm_data", ier)
     stop
  endif
  call c_f_pointer(pnParts, numberOfParticles)
  call c_f_pointer(pnOfSpecies, numberOfSpecies)
  call c_f_pointer(pparticleSpecies, particleSpecies, [N])
  call c_f_pointer(pcoor, coords, [DIM,N])
  call c_f_pointer(pcutoff, cutoff)
  call c_f_pointer(penergy, energy)
  call c_f_pointer(pforces, forces, [DIM,N])


  ! Set values
  numberOfParticles = N
  numberOfSpecies = ASpecies
  particleSpecies(:)    = kim_api_get_species_code(pkim, "Ar", ier)
  if (ier.lt.KIM_STATUS_OK) then
     idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                 "kim_api_get_species_code", ier)
     stop
  endif

  ! print header
  print '(3A20)', "Energy", "Force Norm", "Lattice Spacing"
  ! do the computations
  current_spacing = min_spacing
  do while (current_spacing < max_spacing)

    call create_FCC_configuration(current_spacing, nCellsPerSide, .false., &
      coords, middleDum)
    ! Compute neighbor lists
    call NEIGH_PURE_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), &
                                         neighObject)

    ! Call model compute
    ier = kim_api_model_compute(pkim)
    if (ier.lt.KIM_STATUS_OK) then
       idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
                                   "kim_api_model_compute", ier)
       stop
     endif

    ! compue force_norm
    force_norm = 0.0;
    do i = 1, N
      do j = 1, DIM
        force_norm = force_norm + forces(j,i)*forces(j,i)
      end do
    end do
    force_norm = sqrt(force_norm)

    ! Print results to screen
    !
    print '(3ES20.10)', energy, force_norm, current_spacing

    current_spacing = current_spacing + spacing_incr
  enddo

  ! Deallocate neighbor list object
  deallocate( neighObject%neighborList )

  ier = kim_api_model_destroy(pkim)
  if (ier.lt.KIM_STATUS_OK) then
    idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
      "kim_api_model_destroy", ier)
    stop
  endif
  call kim_api_free(pkim, ier)
  if (ier.lt.KIM_STATUS_OK) then
    idum = kim_api_report_error(__LINE__, THIS_FILE_NAME, &
      "kim_api_free", ier)
    stop
  endif

end program ex_test_ar_fcc_cluster_fortran
