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
  use, intrinsic :: iso_c_binding
  implicit none

  public

  character(len=4096, kind=c_char) :: myfile

contains
  subroutine my_error(message, line)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message
    integer, intent(in) :: line

    print *,"* Error : '", trim(message), "' ", line, ":", &
      trim(myfile)
    stop 1
  end subroutine my_error

  subroutine my_warning(message, line)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message
    integer, intent(in) :: line

    print *,"* Error : '", trim(message), "' ", line, ":", &
      trim(myfile)
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

  type, bind(c) :: neighObject_type
     real(c_double) :: cutoff
     integer(c_int) :: number_of_particles
     type(c_ptr) :: neighbor_list_pointer
  end type neighObject_type
contains

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
subroutine get_neigh(data_object, number_of_neighbor_lists, cutoffs, &
  neighbor_list_index, request, numnei, pnei1part, ierr) bind(c)
  use error
  implicit none

  !-- Transferred variables
  type(c_ptr),    value, intent(in) :: data_object
  integer(c_int), value, intent(in) :: number_of_neighbor_lists
  real(c_double),        intent(in) :: cutoffs(number_of_neighbor_lists)
  integer(c_int), value, intent(in) :: neighbor_list_index
  integer(c_int), value, intent(in)  :: request
  integer(c_int),        intent(out) :: numnei
  type(c_ptr),           intent(out) :: pnei1part
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int) numberOfParticles
  type(neighObject_type), pointer :: neighObject
  integer(c_int), pointer :: neighborList(:,:)

  myfile = __FILE__

  call c_f_pointer(data_object, neighObject)
  numberOfParticles = neighObject%number_of_particles
  call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
    [numberOfParticles+1, numberOfParticles])

  if (number_of_neighbor_lists /= 1) then
    call my_warning("invalid number of cutoffs", __LINE__)
    ierr = 1
    return
  endif

  if (cutoffs(1) > neighObject%cutoff) then
    call my_warning("neighbor list cutoff too small for model cutoff", &
      __LINE__)
    ierr = 1
    return
  endif

  if (neighbor_list_index /= 1) then
    call my_warning("wrong list index", __LINE__)
    ierr = 1
    return
  endif

  if ( (request.gt.numberOfParticles) .or. (request.lt.1)) then
    print *, request
    call my_warning("Invalid part ID in get_neigh", &
      __LINE__)
    ierr = 1
    return
  endif

  ! set the returned number of neighbors for the returned part
  numnei = neighborList(1,request)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighborList(2,request))

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
  integer(c_int), pointer :: neighborList(:,:)

  call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
    [numberOfParticles+1, numberOfParticles])

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
               neighborList(a,i) = j
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighborList(1,i) = a-1
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

  real(c_double), parameter :: cutpad         = 0.75_cd
  real(c_double), parameter :: FCCspacing     = 5.260_cd
  real(c_double), parameter :: min_spacing    = 0.8*FCCspacing
  real(c_double), parameter :: max_spacing    = 1.2*FCCspacing
  real(c_double), parameter :: spacing_incr   = 0.025*FCCspacing
  real(c_double) :: current_spacing
  real(c_double) :: force_norm

  character(len=256, kind=c_char) :: modelname

  integer(c_int), parameter :: &
    N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1

  type(neighObject_type), target :: neighObject
  integer(c_int), allocatable, target :: neighborList(:,:)

  type(kim_model_handle_type) :: model_handle
  type(kim_compute_arguments_handle_type) :: compute_arguments_handle
  real(c_double) :: influence_distance
  integer(c_int) :: number_of_neighbor_lists
  real(c_double) :: cutoff
  real(c_double) :: cutoffs(1)
  integer(c_int) :: &
    model_will_not_request_neighbors_of_noncontributing_particles(1)
  integer(c_int), target :: particle_species_codes(N)
  integer(c_int), target :: particle_contributing(N)
  real(c_double), target :: energy
  real(c_double), target :: coords(DIM, N)
  real(c_double), target :: forces(DIM, N)
  integer(c_int) i, j, ierr, ierr2

  integer(c_int) species_is_supported
  integer(c_int) species_code
  integer(c_int) requested_units_accepted

  integer :: middledum

  myfile = __FILE__

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

  ! Create empty KIM object
  !
  call kim_model_create(kim_numbering_one_based, &
    kim_length_unit_a, &
    kim_energy_unit_ev, &
    kim_charge_unit_e, &
    kim_temperature_unit_k, &
    kim_time_unit_ps, &
    trim(modelname), &
    requested_units_accepted, &
    model_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_create", __LINE__)
  endif

  ! check that we are compatible
  if (requested_units_accepted == 0) then
    call my_error("Must adapt to model units", __LINE__)
  end if

  ! check that model supports Ar
  !
  call kim_model_get_species_support_and_code(model_handle, &
    kim_species_name_ar, species_is_supported, species_code, ierr)
  if ((ierr /= 0) .or. (species_is_supported /= 1)) then
    call my_error("Model does not support Ar", __LINE__)
  endif

  ! Best-practice is to check that the model is compatible
  ! but we will skip it here

  ! create compute_arguments object
  call kim_model_compute_arguments_create( &
    model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_create", __LINE__)
  endif

  ! register memory with the KIM system
  ierr = 0
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_number_of_particles, n, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_particle_species_codes, particle_species_codes, &
    ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_particle_contributing, particle_contributing, &
    ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_coordinates, coords, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_partial_energy, energy, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_partial_forces, forces, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
     call my_error("set_argument_pointer", __LINE__)
  endif

  ! Allocate storage for neighbor lists
  !
  allocate(neighborList(N+1,N))
  neighObject%neighbor_list_pointer = c_loc(neighborList)
  neighObject%number_of_particles = N

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_compute_arguments_set_callback_pointer(compute_arguments_handle, &
    kim_compute_callback_name_get_neighbor_list, kim_language_name_fortran, &
    c_funloc(get_neigh), c_loc(neighobject), ierr)
  if (ierr /= 0) then
    call my_error("set_callback_pointer", __LINE__)
  end if

  call kim_model_get_influence_distance(model_handle, influence_distance)
  call kim_model_get_number_of_neighbor_lists(model_handle, &
    number_of_neighbor_lists)
  if (number_of_neighbor_lists /= 1) then
    call my_error("too many neighbor lists", __LINE__)
  endif
  call kim_model_get_neighbor_list_values(model_handle, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles, ierr)
  if (ierr /= 0) then
    call my_error("get_neighbor_list_values", __LINE__)
  end if
  cutoff = cutoffs(1)

  ! Setup cluster
  !
  do i=1,N
    particle_species_codes(i) = species_code
  enddo

  ! setup contributing particles
  do i=1,N
    particle_contributing(i) = 1  ! every particle contributes
  enddo

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
    call kim_model_compute(model_handle, compute_arguments_handle, ierr)
    if (ierr /= 0) then
      call my_error("kim_api_model_compute", __LINE__)
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
  deallocate( neighborList )

  call kim_model_compute_arguments_destroy(&
    model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("compute_arguments_destroy", __LINE__)
  endif
  call kim_model_destroy(model_handle)

end program ex_test_ar_fcc_cluster_fortran
