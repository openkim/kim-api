!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2022, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!    Ryan S. Elliott
!    Stephen M. Whalen
!
! SPDX-License-Identifier: LGPL-2.1-or-later
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!

module error
  use, intrinsic :: iso_c_binding
  implicit none
  public

contains
  recursive subroutine my_error(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *, "* Error : ", trim(message)
    stop
  end subroutine my_error

  recursive subroutine my_warning(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *, "* Warning : ", trim(message)
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
!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
  recursive subroutine get_neigh(data_object, number_of_neighbor_lists, &
                                 cutoffs, neighbor_list_index, request, &
                                 numnei, pnei1part, ierr) bind(c)
    use error
    implicit none

    !-- Transferred variables
    type(c_ptr), value, intent(in) :: data_object
    integer(c_int), value, intent(in) :: number_of_neighbor_lists
    real(c_double), intent(in) :: cutoffs(*)
    integer(c_int), value, intent(in) :: neighbor_list_index
    integer(c_int), value, intent(in)  :: request
    integer(c_int), intent(out) :: numnei
    type(c_ptr), intent(out) :: pnei1part
    integer(c_int), intent(out) :: ierr

    !-- Local variables
    integer(c_int) numberOfParticles
    type(neighObject_type), pointer :: neighObject
    integer(c_int), pointer :: neighborList(:, :)

    if (number_of_neighbor_lists > 1) then
      call my_warning("Model requires too many neighbor lists")
      ierr = 1
      return
    end if

    call c_f_pointer(data_object, neighObject)
    numberOfParticles = neighObject%number_of_particles
    call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
                     [numberOfParticles + 1, numberOfParticles])

    if (cutoffs(neighbor_list_index) > neighObject%cutoff) then
      call my_warning("neighbor list cutoff too small for model cutoff")
      ierr = 1
      return
    end if

    if ((request > numberOfParticles) .or. (request < 1)) then
      print *, request
      call my_warning("Invalid part ID in get_neigh")
      ierr = 1
      return
    end if

    ! set the returned number of neighbors for the returned part
    numnei = neighborList(1, request)

    ! set the location for the returned neighbor list
    pnei1part = c_loc(neighborList(2, request))

    ierr = 0
    return
  end subroutine get_neigh

end module mod_neighborlist

module mod_utilities
  use kim_simulator_headers_module
  implicit none
  public

contains

!-------------------------------------------------------------------------------
!
!  Check if we are compatible with the model
!
!-------------------------------------------------------------------------------
  recursive subroutine check_model_compatibility( &
    compute_arguments_handle, forces_optional, particle_energy_supported, &
    particle_energy_optional, model_is_compatible, ierr)
    use, intrinsic :: iso_c_binding
    use error
    implicit none

    !-- Transferred variables
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    logical, intent(out) :: forces_optional
    logical, intent(out) :: particle_energy_supported
    logical, intent(out) :: particle_energy_optional
    logical, intent(out) :: model_is_compatible
    integer(c_int), intent(out) :: ierr

    !-- Local variables
    integer(c_int) i
    integer(c_int) number_of_argument_names
    integer(c_int) number_of_callback_names
    type(kim_compute_argument_name_type) argument_name
    type(kim_support_status_type) support_status
    type(kim_compute_callback_name_type) callback_name

    ! assume fail
    model_is_compatible = .false.
    particle_energy_supported = .false.
    particle_energy_optional = .false.
    forces_optional = .false.
    ierr = 0

    ! check arguments
    call kim_get_number_of_compute_argument_names( &
      number_of_argument_names)
    do i = 1, number_of_argument_names
      call kim_get_compute_argument_name(i, argument_name, &
                                         ierr)
      if (ierr /= 0) then
        call my_warning("can't get argument name")
        return
      end if
      call kim_get_argument_support_status( &
        compute_arguments_handle, argument_name, support_status, ierr)
      if (ierr /= 0) then
        call my_warning("can't get argument support_status")
        return
      end if

      ! can only handle energy, particle_energy and forces as required args
      if (support_status == KIM_SUPPORT_STATUS_REQUIRED) then
        if (.not. ( &
            (argument_name == KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY) .or. &
            (argument_name == &
             KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY) .or. &
            (argument_name == KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES))) then
          call my_warning("unsupported required argument")
          ierr = 0
          return
        end if
      end if

      ! need both energy and forces not "notSupported"
      if ((argument_name == KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY) .and. &
          (support_status == KIM_SUPPORT_STATUS_NOT_SUPPORTED)) then
        call my_warning("model does not support energy")
        ierr = 0
        return
      end if
      if (argument_name == KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES) then
        if (support_status == KIM_SUPPORT_STATUS_NOT_SUPPORTED) then
          call my_warning("model does not support forces")
          ierr = 0
          return
        else if (support_status == KIM_SUPPORT_STATUS_REQUIRED) then
          forces_optional = .false.
        else if (support_status == KIM_SUPPORT_STATUS_OPTIONAL) then
          forces_optional = .true.
        else
          call my_warning("unknown support_status for forces")
          ierr = 0
          return
        end if
      end if

      ! check support for particle_energy
      if (argument_name == &
          KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY) then
        if (support_status == KIM_SUPPORT_STATUS_NOT_SUPPORTED) then
          call my_warning("model does not support partial_particle_energy.  &
            &The associated checks will be disabled.")
          particle_energy_supported = .false.
          particle_energy_optional = .false.
        else if (support_status == KIM_SUPPORT_STATUS_REQUIRED) then
          particle_energy_supported = .true.
          particle_energy_optional = .false.
        else if (support_status == KIM_SUPPORT_STATUS_OPTIONAL) then
          particle_energy_supported = .true.
          particle_energy_optional = .true.
        else
          call my_warning("unknown support_status for particle energy")
          ierr = 0
          return
        end if
      end if
    end do

    ! check call backs
    call kim_get_number_of_compute_callback_names( &
      number_of_callback_names)
    do i = 1, number_of_callback_names
      call kim_get_compute_callback_name(i, callback_name, &
                                         ierr)
      if (ierr /= 0) then
        call my_warning("can't get call back name")
        return
      end if
      call kim_get_callback_support_status( &
        compute_arguments_handle, callback_name, support_status, ierr)
      if (ierr /= 0) then
        call my_warning("can't get call back support_status")
        return
      end if

      ! cannot handle any "required" call backs
      if (support_status == KIM_SUPPORT_STATUS_REQUIRED) then
        call my_warning("unsupported required call back")
        ierr = 0
        return
      end if
    end do

    ! got to here, then everything must be OK
    model_is_compatible = .true.
    ierr = 0
    return
  end subroutine Check_Model_Compatibility

  !-----------------------------------------------------------------------------
  !
  !  Get number and identities of particle species supported by
  !  KIM Model `modelname'
  !
  !-----------------------------------------------------------------------------
  recursive subroutine Get_Model_Supported_Species( &
    model_handle, max_species, model_species, num_species, ier)
    use, intrinsic :: iso_c_binding
    implicit none

    !-- Transferred variables
    type(kim_model_handle_type), intent(in)   :: model_handle
    integer(c_int), intent(in)   :: max_species
    type(kim_species_name_type), intent(out) :: model_species(max_species)
    integer(c_int), intent(out)  :: num_species
    integer(c_int), intent(out)  :: ier

    !-- Local variables
    integer(c_int) i
    integer(c_int) total_num_species
    type(kim_species_name_type) :: species_name
    integer(c_int) species_is_supported
    integer(c_int) code

    ! Initialize error flag
    ier = 1

    num_species = 0 ! initialize

    call kim_get_number_of_species_names(total_num_species)

    if (total_num_species > max_species) return

    num_species = 0
    do i = 1, total_num_species
      call kim_get_species_name(i, species_name, ier)
      call kim_get_species_support_and_code(model_handle, species_name, &
                                            species_is_supported, code, ier)
      if ((ier == 0) .and. (species_is_supported /= 0)) then
        num_species = num_species + 1
        model_species(num_species) = species_name
      end if
    end do

    ier = 0
    return

  end subroutine Get_Model_Supported_Species

  recursive subroutine update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                                           do_update_list, coordsave, &
                                           neighObject, ierr)
    use, intrinsic :: iso_c_binding
    use mod_neighborlist
    implicit none
    integer(c_int), parameter :: cd = c_double ! used for literal constants

    !-- Transferred variables
    integer(c_int), intent(in)    :: DIM
    integer(c_int), intent(in)    :: N
    real(c_double), intent(in)    :: coords(DIM, N)
    real(c_double), intent(in)    :: cutoff
    real(c_double), intent(in)    :: cutpad
    logical, intent(inout) :: do_update_list
    real(c_double), intent(inout) :: coordsave(DIM, N)
    type(neighObject_type), intent(inout) :: neighObject
    integer(c_int), intent(out)   :: ierr

    !-- Local variables
    real(c_double) disp, disp1, disp2, cutrange, dispvec(DIM)
    integer(c_int) i

    ! Initialize error code
    !
    ierr = 0

    ! Update neighbor lists if necessary
    !
    if (.not. do_update_list) then   ! if update not requested

      ! check whether a neighbor list update is necessary even if it hasn't been
      ! requested using the "two max sum" criterion
      disp1 = 0.0_cd
      disp2 = 0.0_cd
      do i = 1, N
        dispvec(1:DIM) = coords(1:DIM, i) - coordsave(1:DIM, i)
        disp = sqrt(dot_product(dispvec, dispvec))
        if (disp >= disp1) then        !  1st position taken
          disp2 = disp1               !  push current 1st into 2nd place
          disp1 = disp                !  and put this one into current 1st
        else if (disp >= disp2) then   !  2nd position taken
          disp2 = disp
        end if
      end do
      do_update_list = (disp1 + disp2 > cutpad)

    end if

    if (do_update_list) then

      ! save current coordinates
      coordsave(1:DIM, 1:N) = coords(1:DIM, 1:N)

      ! compute neighbor lists
      cutrange = cutoff + cutpad
      call NEIGH_PURE_cluster_neighborlist(.false., N, coords, cutrange, &
                                           neighObject)

      ! neighbor list uptodate, no need to compute again for now
      do_update_list = .false.
    end if

    return

  end subroutine update_neighborlist

  !-----------------------------------------------------------------------------
  !
  ! NEIGH_PURE_cluster_neighborlist
  !
  !-----------------------------------------------------------------------------
  recursive subroutine NEIGH_PURE_cluster_neighborlist( &
    half, numberOfParticles, coords, cutoff, neighObject)
    use, intrinsic :: iso_c_binding
    use mod_neighborlist
    implicit none

    !-- Transferred variables
    logical, intent(in)            :: half
    integer(c_int), intent(in)            :: numberOfParticles
    real(c_double), dimension(3, numberOfParticles), &
      intent(in)            :: coords
    real(c_double), intent(in)            :: cutoff
    type(neighObject_type), intent(inout) :: neighObject

    !-- Local variables
    integer(c_int), pointer :: neighborList(:, :)
    integer(c_int) i, j, a
    real(c_double) dx(3)
    real(c_double) r2
    real(c_double) cutoff2

    call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
                     [numberOfParticles + 1, numberOfParticles])

    neighObject%cutoff = cutoff

    cutoff2 = cutoff**2

    do i = 1, numberOfParticles
      a = 1
      do j = 1, numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2 <= cutoff2) then
          ! part j is a neighbor of part i
          if ((j > i) .OR. ((.not. half) .AND. (i /= j))) then
            a = a + 1
            neighborList(a, i) = j
          end if
        end if
      end do
      ! part i has a-1 neighbors
      neighborList(1, i) = a - 1
    end do

    return

  end subroutine NEIGH_PURE_cluster_neighborlist

  !-----------------------------------------------------------------------------
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
  !-----------------------------------------------------------------------------
  recursive subroutine create_FCC_configuration(FCCspacing, nCellsPerSide, &
                                                periodic, coords, MiddlePartId)
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int), parameter :: cd = c_double ! used for literal constants

    !-- Transferred variables
    real(c_double), intent(in)  :: FCCspacing
    integer(c_int), intent(in)  :: nCellsPerSide
    logical, intent(in)  :: periodic
    real(c_double), intent(out) :: coords(3, *)
    integer(c_int), intent(out) :: MiddlePartId
    !
    ! cluster setup variables
    !
    real(c_double) FCCshifts(3, 4)
    real(c_double) latVec(3)
    integer(c_int) a, i, j, k, m

    ! Create a cubic FCC cluster
    !
    FCCshifts(1, 1) = 0.0_cd
    FCCshifts(2, 1) = 0.0_cd
    FCCshifts(3, 1) = 0.0_cd
    FCCshifts(1, 2) = 0.5_cd * FCCspacing
    FCCshifts(2, 2) = 0.5_cd * FCCspacing
    FCCshifts(3, 2) = 0.0_cd
    FCCshifts(1, 3) = 0.5_cd * FCCspacing
    FCCshifts(2, 3) = 0.0_cd
    FCCshifts(3, 3) = 0.5_cd * FCCspacing
    FCCshifts(1, 4) = 0.0_cd
    FCCshifts(2, 4) = 0.5_cd * FCCspacing
    FCCshifts(3, 4) = 0.5_cd * FCCspacing

    MiddlePartID = 1 ! Always put middle particle as #1
    a = 1            ! leave space for middle particle as particle #1
    do i = 1, nCellsPerSide
      latVec(1) = (i - 1) * FCCspacing
      do j = 1, nCellsPerSide
        latVec(2) = (j - 1) * FCCspacing
        do k = 1, nCellsPerSide
          latVec(3) = (k - 1) * FCCspacing
          do m = 1, 4
            a = a + 1
            coords(:, a) = latVec + FCCshifts(:, m)
            if ((i == nCellsPerside / 2 + 1) &
                .and. (j == nCellsPerSide / 2 + 1) &
                .and. (k == nCellsPerSide / 2 + 1) &
                .and. (m == 1)) &
              then
              ! put middle particle as #1
              coords(:, 1) = latVec + FCCshifts(:, m)
              a = a - 1
            end if
          end do
        end do
        if (.not. periodic) then
          ! Add in the remaining three faces
          ! pos-x face
          latVec(1) = nCellsPerSide * FCCspacing
          latVec(2) = (i - 1) * FCCspacing
          latVec(3) = (j - 1) * FCCspacing
          a = a + 1; coords(:, a) = latVec
          a = a + 1; coords(:, a) = latVec + FCCshifts(:, 4)
          ! pos-y face
          latVec(1) = (i - 1) * FCCspacing
          latVec(2) = nCellsPerSide * FCCspacing
          latVec(3) = (j - 1) * FCCspacing
          a = a + 1; coords(:, a) = latVec
          a = a + 1; coords(:, a) = latVec + FCCshifts(:, 3)
          ! pos-z face
          latVec(1) = (i - 1) * FCCspacing
          latVec(2) = (j - 1) * FCCspacing
          latVec(3) = nCellsPerSide * FCCspacing
          a = a + 1; coords(:, a) = latVec
          a = a + 1; coords(:, a) = latVec + FCCshifts(:, 2)
        end if
      end do
      if (.not. periodic) then
        ! Add in the remaining three edges
        latVec(1) = (i - 1) * FCCspacing
        latVec(2) = nCellsPerSide * FCCspacing
        latVec(3) = nCellsPerSide * FCCspacing
        a = a + 1; coords(:, a) = latVec
        latVec(1) = nCellsPerSide * FCCspacing
        latVec(2) = (i - 1) * FCCspacing
        latVec(3) = nCellsPerSide * FCCspacing
        a = a + 1; coords(:, a) = latVec
        latVec(1) = nCellsPerSide * FCCspacing
        latVec(2) = nCellsPerSide * FCCspacing
        latVec(3) = (i - 1) * FCCspacing
        a = a + 1; coords(:, a) = latVec
      end if
    end do
    if (.not. periodic) then
      ! Add in the remaining corner
      a = a + 1; coords(:, a) = nCellsPerSide * FCCspacing
    end if

    return

  end subroutine create_FCC_configuration

  recursive subroutine compute_numer_deriv( &
    partnum, dir, model_handle, compute_arguments_handle, DIM, N, coords, &
    cutoff, cutpad, energy, do_update_list, coordsave, neighObject, deriv, &
    deriv_err, ierr)
    use, intrinsic :: iso_c_binding
    use error
    use mod_neighborlist
    implicit none
    integer(c_int), parameter :: cd = c_double ! used for literal constants

    !--Transferred variables
    integer(c_int), intent(in)    :: partnum
    integer(c_int), intent(in)    :: dir
    type(kim_model_handle_type), intent(in) :: model_handle
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    integer(c_int), intent(in)    :: DIM
    integer(c_int), intent(in)    :: N
    real(c_double), intent(inout) :: coords(DIM, N)
    real(c_double), intent(in)    :: cutoff
    real(c_double), intent(in)    :: cutpad
    real(c_double), intent(inout) :: energy
    logical, intent(inout) :: do_update_list
    real(c_double), intent(inout) :: coordsave(DIM, N)
    type(neighObject_type), intent(inout) :: neighObject
    real(c_double), intent(out)   :: deriv
    real(c_double), intent(out)   :: deriv_err
    integer(c_int), intent(out)   :: ierr

    !-- Local variables
    real(c_double), parameter :: eps_init = 1.e-6_cd
    integer(c_int), parameter :: number_eps_levels = 15
    real(c_double) eps, deriv_last, deriv_err_last
    integer(c_int) i

    ! Initialize error flag
    ierr = 0

    deriv_last = 0.0_cd ! initialize

    ! Outer loop of Ridders' method for computing numerical derivative
    !
    eps = eps_init
    deriv_err_last = huge(1.0_cd)
    do i = 1, number_eps_levels
      deriv = dfridr(eps, deriv_err)
      if (ierr /= 0) then
        call my_error("compute_numer_deriv")
      end if
      if (deriv_err > deriv_err_last) then
        deriv = deriv_last
        deriv_err = deriv_err_last
        exit
      end if
      eps = eps * 10.0_cd
      deriv_last = deriv
      deriv_err_last = deriv_err
    end do

    return

  contains

    !----------------------------------------------------------------------------
    !
    ! Compute numerical derivative using Ridders' method
    !
    ! Based on code from Numerical Recipes, Press et al., Second Ed., Cambridge,
    ! 1992
    !
    ! Ref: Ridders, C. J. F., "Two algorithms for the calculation of F'(x)=D",
    !      Advances in Engineering Software, Vol. 4, no. 2, pp. 75-76, 1982.
    !
    !
    ! Returns the gradient grad() of a KIM-compliant interatomic model at the
    ! current configuration by Ridders' method of polynomial extrapolation.
    ! An estimate for the error in each component of the gradient is returned in
    ! grad_err().
    !
    !----------------------------------------------------------------------------
    real(c_double) recursive function dfridr(h, err)
      implicit none

      !-- Transferred variables
      real(c_double), intent(inout) :: h
      real(c_double), intent(out)   :: err

      !-- Local variables
      integer(c_int), parameter :: NTAB = 10     ! Maximum size of tableau
      real(c_double), parameter :: CON = 1.4_cd  ! Stepsize incr. by CON at each iter
      real(c_double), parameter :: CON2 = CON * CON
      real(c_double), parameter :: BIG = huge(1.0_cd)
      real(c_double), parameter :: SAFE = 2.0_cd ! Returns when error is SAFE worse
      ! than the best so far
      integer(c_int) i, j
      real(c_double) errt, fac, hh, a(NTAB, NTAB), fp, fm, coordorig

      dfridr = 0.0_cd ! initialize
      err = BIG ! initialize

      if (abs(h) <= tiny(0.0_cd)) then  ! avoid division by zero
        ierr = 1
        return
      end if

      hh = h
      coordorig = coords(dir, partnum)
      coords(dir, partnum) = coordorig + hh
      call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                               do_update_list, coordsave, &
                               neighObject, ierr)
      call kim_compute(model_handle, compute_arguments_handle, ierr)
      if (ierr /= 0) then
        call my_error("kim_api_model_compute")
      end if
      fp = energy
      coords(dir, partnum) = coordorig - hh
      call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                               do_update_list, coordsave, &
                               neighObject, ierr)
      call kim_compute(model_handle, compute_arguments_handle, ierr)
      if (ierr /= 0) then
        call my_error("kim_api_model_compute")
      end if
      fm = energy
      coords(dir, partnum) = coordorig
      call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                               do_update_list, coordsave, &
                               neighObject, ierr)
      a(1, 1) = (fp - fm) / (2.0_cd * hh)
      ! successive columns in the Neville tableau will go to smaller step sizes
      ! and higher orders of extrapolation
      do i = 2, NTAB
        ! try new, smaller step size
        hh = hh / CON
        coords(dir, partnum) = coordorig + hh
        call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                                 do_update_list, coordsave, &
                                 neighObject, ierr)
        call kim_compute(model_handle, compute_arguments_handle, ierr)
        if (ierr /= 0) then
          call my_error("kim_api_model_compute")
        end if
        fp = energy
        coords(dir, partnum) = coordorig - hh
        call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                                 do_update_list, coordsave, &
                                 neighObject, ierr)
        call kim_compute(model_handle, compute_arguments_handle, ierr)
        if (ierr /= 0) then
          call my_error("kim_api_model_compute")
        end if
        fm = energy
        coords(dir, partnum) = coordorig
        call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                                 do_update_list, coordsave, &
                                 neighObject, ierr)
        a(1, i) = (fp - fm) / (2.0_cd * hh)
        fac = CON2
        ! compute extrapolations of various orders, requiring no new function
        ! evaluations
        do j = 2, i
          a(j, i) = (a(j - 1, i) * fac - a(j - 1, i - 1)) / (fac - 1.0_cd)
          fac = CON2 * fac
          ! The error strategy is to compute each new extrapolation to one order
          ! lower, both at the present step size and the previous one.
          errt = max(abs(a(j, i) - a(j - 1, i)), abs(a(j, i) - a(j - 1, i - 1)))
          if (errt <= err) then ! if error is decreased, save the improved answer
            err = errt
            dfridr = a(j, i)
          end if
        end do
        if (abs(a(i, i) - a(i - 1, i - 1)) >= SAFE * err) return ! if higher order is worse
        ! by significant factor
        ! `SAFE', then quit early.
      end do
      return
    end function dfridr

  end subroutine compute_numer_deriv

end module mod_utilities

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
program vc_forces_numer_deriv
  use, intrinsic :: iso_c_binding
  use error
  use mod_neighborlist
  use mod_utilities
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: nCellsPerSide = 2
  integer(c_int), parameter :: DIM = 3
  real(c_double), parameter :: cutpad = 0.75_cd
  integer(c_int), parameter :: max_species = 200 ! most species supported
  real(c_double), parameter :: eps_prec = epsilon(1.0_cd)
  real(c_double) FCCspacing

  integer(c_int), parameter :: N = 4 * (nCellsPerSide)**3 &
                               + 6 * (nCellsPerSide)**2 &
                               + 3 * (nCellsPerSide) + 1
  real(c_double), allocatable          :: forces_num(:, :)
  real(c_double), allocatable          :: forces_num_err(:, :)
  type(kim_species_name_type) :: model_species(max_species)
  integer(c_int), target               :: num_species
  character(len=5, kind=c_char)        :: passfail
  real(c_double)                       :: forcediff
  real(c_double)                       :: forcediff_sumsq
  real(c_double)                       :: weight
  real(c_double)                       :: weight_sum
  real(c_double)                       :: alpha
  real(c_double)                       :: term
  real(c_double)                       :: term_max
  real(c_double), allocatable          :: cluster_coords(:, :)
  real(c_double), allocatable          :: cluster_disps(:, :)
  type(kim_species_name_type), allocatable :: cluster_species(:)
  integer(c_int) I, J, Imax, Jmax, species_code
  integer(c_int) seed_size
  integer(c_int), allocatable :: seed(:)

  !
  ! neighbor list
  !
  type(neighObject_type), target :: neighObject
  integer(c_int), allocatable, target :: neighborList(:, :)
  real(c_double), allocatable :: coordsave(:, :)
  logical do_update_list

  !
  ! KIM variables
  !
  character(len=256, kind=c_char) :: testname = "vc_forces_numer_deriv"
  character(len=256, kind=c_char) :: modelname

  type(kim_model_handle_type) :: model_handle
  type(kim_compute_arguments_handle_type) :: compute_arguments_handle
  integer(c_int) ierr, ierr2
  integer(c_int) species_is_supported
  integer(c_int), target :: numberOfParticles
  integer(c_int), target :: particleSpeciesCodes(N)
  integer(c_int), target :: particleContributing(N)
  real(c_double) :: influence_distance
  integer(c_int) :: number_of_neighbor_lists
  real(c_double), allocatable :: cutoffs(:)
  integer(c_int), allocatable :: &
    model_will_not_request_neighbors_of_noncontributing_particles(:)
  real(c_double) :: cutoff
  real(c_double), target :: energy
  real(c_double), target :: particle_energy(N)
  real(c_double), target :: particle_energy_sum
  real(c_double), target :: coords(3, N)
  real(c_double), target :: forces_kim(3, N)
  real(c_double) :: forces(3, N)
  integer(c_int) middleDum
  logical doing_non_contributing
  logical particle_energy_supported
  logical particle_energy_optional
  logical noncontrib_particle_energy_nonzero
  logical forces_optional
  logical model_is_compatible
  integer(c_int) number_of_model_routine_names
  type(kim_model_routine_name_type) model_routine_name
  integer(c_int) present
  integer(c_int) required
  integer(c_int) requested_units_accepted
  real(c_double) rnd, deriv, deriv_err
  real(c_double), pointer :: null_pointer

  nullify (null_pointer)

  doing_non_contributing = .false. ! .true. on 2nd pass through main program

  numberOfParticles = N

  term_max = 0.0_cd ! initialize

  ! Initialize error flag
  ierr = 0

  ! Initialize seed for random number generator
  !
  ! NOTE: Here, we set the seed to a fixed value for reproducibility
  call random_seed(size=seed_size) ! Get seed size for this CPU
  allocate (seed(seed_size))
  seed(:) = 13
  call random_seed(put=seed)
  deallocate (seed)

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read (*, *) modelname

  ! Print output header
  !
  print *
  print *, 'VERIFICATION CHECK: NUMERICAL DERIVATIVE VERIFICATION OF FORCES'
  print *
  print '(120(''-''))'
  print '("This is Test          : ",A)', trim(testname)
  print '("Results for KIM Model : ",A)', trim(modelname)

  ! Create empty KIM object
  !
  call kim_model_create(KIM_NUMBERING_ONE_BASED, &
                        KIM_LENGTH_UNIT_A, &
                        KIM_ENERGY_UNIT_EV, &
                        KIM_CHARGE_UNIT_E, &
                        KIM_TEMPERATURE_UNIT_K, &
                        KIM_TIME_UNIT_PS, &
                        trim(modelname), &
                        requested_units_accepted, &
                        model_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_create")
  end if

  ! check that we are compatible
  if (requested_units_accepted == 0) then
    call my_error("Must adapt to model units")
  end if

  ! check that we know about all required routines
  call kim_get_number_of_model_routine_names(number_of_model_routine_names)
  do i = 1, number_of_model_routine_names
    call kim_get_model_routine_name(i, model_routine_name, ierr)
    if (ierr /= 0) then
      call my_error("kim_get_model_routine_name")
    end if
    call kim_is_routine_present(model_handle, model_routine_name, present, &
                                required, ierr)
    if (ierr /= 0) then
      call my_error("kim_is_routine_present")
    end if

    if ((present == 1) .and. (required == 1)) then
      if (.not. ((model_routine_name == KIM_MODEL_ROUTINE_NAME_CREATE) &
                 .or. (model_routine_name == &
                       KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_CREATE) &
                 .or. (model_routine_name == KIM_MODEL_ROUTINE_NAME_COMPUTE) &
                 .or. (model_routine_name == KIM_MODEL_ROUTINE_NAME_REFRESH) &
                 .or. (model_routine_name == &
                       KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_DESTROY) &
                 .or. (model_routine_name == KIM_MODEL_ROUTINE_NAME_DESTROY))) &
        then
        call my_error("Unknown required ModelRoutineName found.")
      end if
    end if
  end do

  ! create compute_arguments object
  call kim_compute_arguments_create(model_handle, &
                                    compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_create")
  end if

  call check_model_compatibility(compute_arguments_handle, forces_optional, &
                                 particle_energy_supported, &
                                 particle_energy_optional, &
                                 model_is_compatible, ierr)
  if (ierr /= 0) then
    call my_error("error checking compatibility")
  end if
  if (.not. model_is_compatible) then
    call my_error("incompatibility reported by check_model_compatibility")
  end if

  ! Get list of particle species supported by the model
  !
  call Get_Model_Supported_Species(model_handle, max_species, model_species, &
                                   num_species, ierr)
  if (ierr /= 0) then
    call my_error("Get_Model_Supported_Species")
  end if
  ! Setup random cluster
  !
  allocate (cluster_coords(3, N), cluster_disps(3, N), cluster_species(N))
  do i = 1, N
    call random_number(rnd)  ! return random number between 0 and 1
    species_code = 1 + int(rnd * num_species)
    cluster_species(i) = model_species(species_code)
  end do
  FCCspacing = 1.0_cd  ! initially generate an FCC cluster with lattice
  ! spacing equal to one. This is scaled below based
  ! on the cutoff radius of the model.
  call create_FCC_configuration(FCCspacing, nCellsPerSide, .false., &
                                cluster_coords, middleDum)
  ! Generate random displacements for all particles
  !
  do I = 1, N
    do J = 1, DIM
      call random_number(rnd)  ! return random number between 0 and 1
      cluster_disps(J, I) = 0.1_cd * (rnd - 0.5_cd)
    end do
  end do

  ! register memory with the KIM system
  ierr = 0
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_NUMBER_OF_PARTICLES, &
    numberOfParticles, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_SPECIES_CODES, &
    particleSpeciesCodes, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_CONTRIBUTING, &
    particleContributing, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_COORDINATES, coords, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY, energy, ierr2)
  ierr = ierr + ierr2
  if (particle_energy_supported) then
    call kim_set_argument_pointer( &
      compute_arguments_handle, &
      KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY, particle_energy, ierr2)
    ierr = ierr + ierr2
  end if
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES, forces_kim, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
    call my_error("set_argument_pointer")
  end if

  ! Allocate storage for neighbor lists and
  ! store pointers to neighbor list object and access function
  !
  allocate (coordsave(DIM, N))
  allocate (neighborList(N + 1, N))
  neighObject%neighbor_list_pointer = c_loc(neighborList)
  neighObject%number_of_particles = N
  allocate (forces_num(DIM, N), forces_num_err(DIM, N))

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_set_callback_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_CALLBACK_NAME_GET_NEIGHBOR_LIST, KIM_LANGUAGE_NAME_FORTRAN, &
    c_funloc(get_neigh), c_loc(neighobject), ierr)
  if (ierr /= 0) then
    call my_error("set_callback_pointer")
  end if

  call kim_get_influence_distance(model_handle, influence_distance)
  call kim_get_number_of_neighbor_lists(model_handle, &
                                        number_of_neighbor_lists)
  allocate (cutoffs(number_of_neighbor_lists), &
            model_will_not_request_neighbors_of_noncontributing_particles( &
            number_of_neighbor_lists))
  call kim_get_neighbor_list_values( &
    model_handle, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles, ierr)
  if (ierr /= 0) then
    call my_error("get_neighbor_list_values")
  end if
  cutoff = maxval(cutoffs)

  ! Scale reference FCC configuration based on cutoff radius.
  FCCspacing = 0.75_cd * cutoff ! set the FCC spacing to a fraction
  ! of the cutoff radius
  do i = 1, N
    cluster_coords(:, i) = FCCspacing * cluster_coords(:, i)
  end do
  print '("Using FCC lattice parameter: ",f12.5)', FCCspacing
  print *

1000 continue ! Start of configuration setup

  if (doing_non_contributing) then
    ! Turn particle energy computation back on, if possible
    if (particle_energy_optional) then
      call kim_set_argument_pointer( &
        compute_arguments_handle, &
        KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY, &
        particle_energy, ierr)
      if (ierr /= 0) then
        call my_error("set_argument_pointer")
      end if
    end if

    ! Turn force computation back on, if possible
    !
    if (forces_optional) then
      call kim_set_argument_pointer( &
        compute_arguments_handle, KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES, &
        forces_kim, ierr)
      if (ierr /= 0) then
        call my_error("set_argument_pointer")
      end if
    end if
  end if

  do i = 1, N
    call kim_get_species_support_and_code( &
      model_handle, cluster_species(i), species_is_supported, &
      particleSpeciesCodes(i), ierr)
  end do
  if (ierr /= 0) then
    call my_error("kim_api_get_species_code")
  end if
  if (.not. doing_non_contributing) then
    do i = 1, N
      particleContributing(i) = 1  ! all particle contribute
    end do
  else
    do i = 1, N
      ! Random collection of contributing atoms
      call random_number(rnd)  ! return random number between 0 and 1
      if (rnd > 0.5) then
        particleContributing(i) = 1
      else
        particleContributing(i) = 0
      end if
    end do
  end if
  do i = 1, N
    coords(:, i) = cluster_coords(:, i) + cluster_disps(:, i)
  end do

  ! Compute neighbor lists
  !
  do_update_list = .true.
  call update_neighborlist(DIM, N, coords, cutoff, cutpad, &
                           do_update_list, coordsave, &
                           neighObject, ierr)
  if (ierr /= 0) then
    call my_error("update_neighborlist")
  end if

  ! Call model compute to get forces (gradient)
  !
  call kim_compute(model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_model_compute")
  end if

  ! Copy forces in case model will overwrite forces_kim below
  !
  forces = forces_kim

  ! Add up particle_energy to compare with energy
  noncontrib_particle_energy_nonzero = .false.
  if (particle_energy_supported) then
    particle_energy_sum = 0.0_cd
    do i = 1, N
      if (particleContributing(i) == 0) then
        if (abs(particle_energy(i)) > epsilon(0.0_cd)) then
          noncontrib_particle_energy_nonzero = .true.
        end if
      else
        particle_energy_sum = particle_energy_sum + particle_energy(i)
      end if
    end do
  end if

  ! Print results to screen
  !
  print '(41(''=''))'
  if (.not. doing_non_contributing) then
    print '("Configuration with all contributing particles")'
  else
    print '("Configuration with some non-contributing particles")'
  end if
  if (particle_energy_supported) then
    print '(A25,2X,A25,2X,A15)', "Energy", "Sum Contrib. Energies", "Diff"
    print '(ES25.15,2X,ES25.15,2X,ES15.5)', energy, particle_energy_sum, &
      energy - particle_energy_sum
    if (noncontrib_particle_energy_nonzero) then
      call my_error( &
        "Some non-contributing particles have non-zero &
        &partial_particle_energy")
    end if
  else
    print '("Energy = ",ES25.15)', energy
  end if
  print '(41(''=''))'
  print *

  ! Turn off particle energy computation, if possible
  if (particle_energy_optional) then
    call kim_set_argument_pointer( &
      compute_arguments_handle, &
      KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY, &
      null_pointer, ierr)
    if (ierr /= 0) then
      call my_error("set_argument_pointer")
    end if
  end if

  ! Turn off force computation, if possible
  !
  if (forces_optional) then
    call kim_set_argument_pointer( &
      compute_arguments_handle, KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES, &
      null_pointer, ierr)
    if (ierr /= 0) then
      call my_error("set_argument_pointer")
    end if
  end if

  ! Compute gradient using numerical differentiation
  !
  do I = 1, N
    do J = 1, DIM
      call compute_numer_deriv(I, J, model_handle, compute_arguments_handle, &
                               DIM, N, coords, cutoff, &
                               cutpad, energy, do_update_list, &
                               coordsave, neighObject, deriv, deriv_err, ierr)
      if (ierr /= 0) then
        call my_error("compute_numer_deriv")
      end if
      forces_num(J, I) = -deriv
      forces_num_err(J, I) = deriv_err
    end do
  end do

  ! Continue printing results to screen
  !
  print '(A6,2X,A7,2X,A4,2X,A3,2X,2A25,3A15,2X,A4)', "Part", "Contrib", &
    "Spec", "Dir", "Force_model", "Force_numer", "Force diff", "pred error", &
    "weight", "stat"
  forcediff_sumsq = 0.0_cd
  weight_sum = 0.0_cd
  do I = 1, N
    do J = 1, DIM
      forcediff = abs(forces(J, I) - forces_num(J, I))
      if (forcediff < forces_num_err(J, I)) then
        passfail = "ideal"
      else
        passfail = "     "
      end if
      weight = max(abs(forces_num(J, I)), eps_prec) / &
               max(abs(forces_num_err(J, I)), eps_prec)
      term = weight * forcediff**2
      if (term > term_max) then
        term_max = term
        Imax = I
        Jmax = J
      end if
      forcediff_sumsq = forcediff_sumsq + term
      weight_sum = weight_sum + weight
      if (J == 1) then
        print '(I6,2X,I7,2X,I4,2X,I3,2X,2ES25.15,3ES15.5,2X,A5)', &
          I, particleContributing(I), &
          particleSpeciesCodes(I), J, forces(J, I), &
          forces_num(J, I), forcediff, &
          forces_num_err(J, I), weight, passfail
      else
        print '(23X,I3,2X,2ES25.15,3ES15.5,2X,A5)', &
          J, forces(J, I), forces_num(J, I), &
          forcediff, forces_num_err(J, I), weight, passfail
      end if
    end do
    print *
  end do
  alpha = sqrt(forcediff_sumsq / weight_sum) / dble(DIM * N)
  print *
  print '("alpha = |Force_model - Force_numer|_w/(DIM*N) = ",ES15.5," &
        &(units of force)")', alpha
  print *
  print '(''Maximum term obtained for Part = '',I6,'', Dir = '',I1,'// &
    ''', forcediff = '',ES15.5, '', forcediff/force_model = '',ES15.5)', &
    Imax, Jmax, abs(forces(Jmax, Imax) - forces_num(Jmax, Imax)), &
    abs(forces(Jmax, Imax) - forces_num(Jmax, Imax)) / abs(forces(Jmax, Imax))

  if (.not. doing_non_contributing) then
    doing_non_contributing = .true.
    print *
    print *
    goto 1000
  end if

  ! Free temporary storage
  !
  deallocate (forces_num)
  deallocate (forces_num_err)
  deallocate (neighborList)
  deallocate (coordsave)
  deallocate (cutoffs)
  deallocate (model_will_not_request_neighbors_of_noncontributing_particles)

  call kim_compute_arguments_destroy(model_handle, &
                                     compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_destroy")
  end if
  call kim_model_destroy(model_handle)

  ! Print output footer
  !
  print *
  print '(120(''-''))'

  ! Free cluster storage
  !
  deallocate (cluster_coords, cluster_disps, cluster_species)

  stop

end program vc_forces_numer_deriv
