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

module error
  implicit none
  public

contains
  subroutine my_error(message, line, file)
    implicit none
    character(len=*), intent(in) :: message
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    print *,"* Error : '", trim(message), "' ", line, ":", &
      trim(file)
    stop
  end subroutine my_error

  subroutine my_warning(message, line, file)
    implicit none
    character(len=*), intent(in) :: message
    integer, intent(in) :: line
    character(len=*), intent(in) :: file

    print *,"* Error : '", trim(message), "' ", line, ":", &
      trim(file)
  end subroutine my_warning
end module error


!-------------------------------------------------------------------------------
!
! module mod_neighborlist :
!
!    Module contains type and routines related to neighbor list calculation
!
!-------------------------------------------------------------------------------

module mod_neighborlist

  use, intrinsic :: iso_c_binding

  public get_neigh

  type neighObject_type
     real(c_double) :: cutoff
     integer(c_int) :: number_of_particles
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
subroutine get_neigh(data_object, number_of_cutoffs, cutoffs, &
  neighbor_list_index, request, numnei, pnei1part, ierr) bind(c)
  use error
  implicit none

  !-- Transferred variables
  type(c_ptr),    value, intent(in) :: data_object
  integer(c_int), value, intent(in) :: number_of_cutoffs
  read(c_double),        intent(in) :: cutoffs(number_of_cutoffs)
  integer(c_int), value, intent(in) :: neighbor_list_index
  integer(c_int), value, intent(in)  :: request
  integer(c_int),        intent(out) :: numnei
  type(c_ptr),           intent(out) :: pnei1part
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int), parameter :: DIM = 3
  integer(c_int) numberOfParticles
  type(neighObject_type), pointer :: neighObject

  call c_f_pointer(data_object, neighObject)

  if (number_of_cutoffs /= 1) then
    call my_warning("invalid number of cutoffs", __LINE__, __FILE__)
    ierr = 1
    return
  endif

  if (cutoffs(1) > neighObject%cutoff) then
    call my_warning("neighbor list cutoff too small for model cutoff", &
      __LINE__, __FILE__)
    ierr = 1
    return
  endif

  if (neighbor_list_index /= 1) then
    call my_warning("wrong list index", __LINE__, __FILE__)
    ierr = 1
    return
  endif

  numberOfParticles = neighObject%number_of_particles

  if ( (request.gt.numberOfParticles) .or. (request.lt.1)) then
    print *, request
    call my_warning("Invalid part ID in get_neigh", &
      __LINE__, __FILE__)
    ierr = 1
    return
  endif

  ! set the returned number of neighbors for the returned part
  numnei = neighObject%neighborList(1,request)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighObject%neighborList(2,request))

  ierr = 0
  return
end subroutine get_neigh

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

  neighObject%cutoff = cutoff

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
  use error
  use kim_simulator_headers_module
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


  integer(c_int) ierr
  integer :: middledum
  integer :: i, j

  ! Initialize error flag
  ierr = 0

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
  ierr = kim_api_file_init(pkim, testkimfile, modelname)
  if (ierr.lt.KIM_STATUS_OK) then
     call my_error("kim_api_file_init", __LINE__, __FILE__)
     stop
  endif

  ! make sure NEIGH_PURE_F
  ierr = kim_api_get_nbc_method(pkim, NBC_Method)
  if (ierr.lt.KIM_STATUS_OK) then
     call my_error("kim_api_get_nbc_method", __LINE__, __FILE__)
     stop
  endif
  if (index(NBC_Method,"NEIGH_PURE_F").ne.1) then
     ierr = KIM_STATUS_FAIL
     call my_error("Unknown NBC method", __LINE__, __FILE__)
     stop
  endif

  ! Allocate memory via the KIM system
  call kim_api_allocate(pkim, N, ASpecies, ierr)
  if (ierr.lt.KIM_STATUS_OK) then
     call my_error("kim_api_allocate", __LINE__, __FILE__)
     stop
  endif

  ! Allocate storage for neighbor lists
  !
  allocate(neighObject%neighborList(N+1,N))
  neighObject%number_of_particles = N

  ! Set pointer in KIM object to neighbor list routine and object
  !
  ierr = kim_api_set_data(pkim, "neighObject", SizeOne, c_loc(neighObject))
  if (ierr.lt.KIM_STATUS_OK) then
    call my_error("kim_api_set_data", __LINE__, __FILE__)
    stop
  endif

  ierr = kim_api_set_method(pkim, "get_neigh", SizeOne, c_funloc(get_neigh))
  if (ierr.lt.KIM_STATUS_OK) then
    call my_error("kim_api_set_method", __LINE__, __FILE__)
    stop
  endif

  ! call model's init routine
  ierr = kim_api_model_init(pkim)
  if (ierr.lt.KIM_STATUS_OK) then
     call my_error("kim_api_model_init", __LINE__, __FILE__)
     stop
  endif


  ! Unpack data from KIM object
  !
  call kim_api_getm_data(pkim, ierr, &
       "numberOfParticles",           pnParts,          1,                                   &
       "numberOfSpecies",             pnOfSpecies,      1,                                   &
       "particleSpecies",             pparticleSpecies, 1,                                   &
       "coordinates",                 pcoor,            1,                                   &
       "cutoff",                      pcutoff,          1,                                   &
       "energy",                      penergy,          1,                                   &
       "forces",                      pforces,          1)
  if (ierr.lt.KIM_STATUS_OK) then
     call my_error("kim_api_getm_data", __LINE__, __FILE__)
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
  particleSpecies(:)    = kim_api_get_species_code(pkim, "Ar", ierr)
  if (ierr.lt.KIM_STATUS_OK) then
     call my_error("kim_api_get_species_code", __LINE__, __FILE__)
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
    ierr = kim_api_model_compute(pkim)
    if (ierr.lt.KIM_STATUS_OK) then
       call my_error("kim_api_model_compute", __LINE__, __FILE__)
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

  ierr = kim_api_model_destroy(pkim)
  if (ierr.lt.KIM_STATUS_OK) then
    call my_error("kim_api_model_destroy", __LINE__, __FILE__)
    stop
  endif
  call kim_api_free(pkim, ierr)
  if (ierr.lt.KIM_STATUS_OK) then
    call my_error("kim_api_free", __LINE__, __FILE__)
    stop
  endif

end program ex_test_ar_fcc_cluster_fortran
