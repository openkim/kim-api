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
! Copyright (c) 2013--2017, Regents of the University of Minnesota.
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
     integer(c_int) :: number_of_particles
     integer(c_int), pointer :: neighborList(:,:)
     real(c_double), pointer :: RijList(:,:,:)
  end type neighObject_type
contains

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
subroutine get_neigh(data_object, neighbor_list_index, request, numnei, &
  pnei1part, ierr) bind(c)
  use error
  implicit none

  !-- Transferred variables
  type(c_ptr),    value, intent(in) :: data_object
  integer(c_int), value, intent(in) :: neighbor_list_index
  integer(c_int), value, intent(in)  :: request
  integer(c_int),        intent(out) :: numnei
  type(c_ptr),           intent(out) :: pnei1part
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int), parameter :: DIM = 3
  integer(c_int) numberOfParticles
  type(neighObject_type), pointer :: neighObject

  if (neighbor_list_index /= 1) then
    call my_warning("wrong list index", __LINE__, __FILE__)
    ierr = 1
    return
  endif

  call c_f_pointer(data_object, neighObject)

  numberOfParticles = neighObject%number_of_particles

  if ( (request.gt.numberOfParticles) .or. (request.lt.1)) then
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
program ex_test_ar_fcc_cluster
  use, intrinsic :: iso_c_binding
  use error
  use kim_language_name_module
  use kim_species_name_module
  use kim_numbering_module
  use kim_model_module
  use kim_argument_name_module
  use kim_callback_name_module
  use kim_unit_system_module
  use mod_neighborlist
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

  character(len=256) :: modelname

  integer(c_int), parameter :: &
    N = 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1

  type(neighObject_type), target :: neighObject

  type(kim_model_type), pointer :: model
  real(c_double) :: influence_distance
  integer(c_int) :: number_of_cutoffs
  real(c_double) :: cutoff
  real(c_double) :: cutoffs(1)
  integer(c_int), target          :: particle_species_codes(N)
  integer(c_int), target          :: particle_contributing(N)
  real(c_double), target :: energy
  real(c_double), target :: coords(DIM, N)
  integer(c_int) i, ierr, ierr2

  integer(c_int) species_is_supported
  integer(c_int) species_code
  integer(c_int) requested_units_accepted
  real(c_double), pointer :: null_pointer

  integer :: middledum

  nullify(null_pointer)

  ! Initialize error flag
  ierr = 0

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Print output header
  !
  print *
  print *,'This is Test : ex_test_Ar_fcc_cluster.'
  print *
  print '(120(''-''))'
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
    model, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_create", __LINE__, __FILE__)
  endif

  ! check that we are compatible
  if (requested_units_accepted == 0) then
    call my_error("Must adapt to model units", __LINE__, __FILE__)
  end if

  ! check that model supports Ar
  !
  call kim_model_get_species_support_and_code(model, kim_species_name_ar, &
    species_is_supported, species_code, ierr)
  if ((ierr /= 0) .or. (species_is_supported /= 1)) then
    call my_error("Model does not support Ar", __LINE__, __FILE__)
  endif

  ! it would be good to check that the model is compatible
  ! but we will skip it here

  ! register memory with the KIM system
  ierr = 0
  call kim_model_set_argument_pointer(model, &
    kim_argument_name_number_of_particles, n, ierr2)
  ierr = ierr + ierr2
  call kim_model_set_argument_pointer(model, &
    kim_argument_name_particle_species_codes, particle_species_codes, ierr2)
  ierr = ierr + ierr2
  call kim_model_set_argument_pointer(model, &
    kim_argument_name_particle_contributing, particle_contributing, ierr2)
  ierr = ierr + ierr2
  call kim_model_set_argument_pointer(model, &
    kim_argument_name_coordinates, coords, ierr2)
  ierr = ierr + ierr2
  call kim_model_set_argument_pointer(model, &
    kim_argument_name_partial_energy, energy, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
     call my_error("set_argument_pointer", __LINE__, __FILE__)
  endif

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_model_set_callback_pointer(model, &
    kim_callback_name_get_neighbor_list, kim_language_name_fortran, &
    c_funloc(get_neigh), c_loc(neighobject), ierr)
  if (ierr /= 0) then
    call my_error("set_callback_pointer", __LINE__, __FILE__)
  end if

  call kim_model_get_influence_distance(model, influence_distance)
  call kim_model_get_number_of_cutoffs(model, number_of_cutoffs)
  if (number_of_cutoffs /= 1) then
    call my_error("too many cutoffs", __LINE__, __FILE__)
  endif
  call kim_model_get_cutoffs(model, cutoffs, ierr)
  if (ierr /= 0) then
    call my_error("get_cutoffs", __LINE__, __FILE__)
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

  ! Allocate storage for neighbor lists and
  ! store pointers to neighbor list object and access function
  !
  allocate(neighObject%neighborList(N+1,N))
  neighObject%number_of_particles = N

  ! do the computations
  current_spacing = min_spacing
  do while (current_spacing < max_spacing)

    call create_FCC_configuration(current_spacing, nCellsPerSide, .false., &
      coords, middleDum)
    ! Compute neighbor lists
    !
    call NEIGH_PURE_cluster_neighborlist(.false., N, coords, cutoff+cutpad, &
      neighObject)
    if (ierr /= 0) then
      call my_error("update_neighborlist", __LINE__, __FILE__)
    endif

    ! Call model compute to get forces (gradient)
    !
    call kim_model_compute(model, ierr)
    if (ierr /= 0) then
      call my_error("kim_api_model_compute", __LINE__, __FILE__)
    endif

    ! Print results to screen
    !
    print '(2ES20.10)', energy, current_spacing

    current_spacing = current_spacing + spacing_incr
  enddo

end program ex_test_ar_fcc_cluster
