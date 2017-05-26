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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_argument_name_module
  use, intrinsic :: iso_c_binding
  use kim_argument_name_id_module
  implicit none
  private

  public &
    kim_argument_name_type, &
    kim_argument_name_string, &
    operator (.eq.), &


    kim_argument_name_number_of_particles, &
    kim_argument_name_particle_species, &
    kim_argument_name_particle_contributing, &
    kim_argument_name_coordinates, &
    kim_argument_name_energy, &
    kim_argument_name_forces, &
    kim_argument_name_particle_energy, &
    kim_argument_name_virial, &
    kim_argument_name_particle_virial, &
    kim_argument_name_hessian, &

    kim_argument_name_get_number_of_arguments, &
    kim_argument_name_get_argument_name, &
    kim_argument_name_get_argument_data_type

  type, bind(c) :: kim_argument_name_type
    integer(c_int) argument_name_id
  end type kim_argument_name_type

  type(kim_argument_name_type), parameter :: &
    kim_argument_name_number_of_particles = &
    kim_argument_name_type(number_of_particles_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_particle_species = &
    kim_argument_name_type(particle_species_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_particle_contributing = &
    kim_argument_name_type(particle_contributing_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_coordinates = &
    kim_argument_name_type(coordinates_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_energy = &
    kim_argument_name_type(energy_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_forces = &
    kim_argument_name_type(forces_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_particle_energy = &
    kim_argument_name_type(particle_energy_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_virial = &
    kim_argument_name_type(virial_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_particle_virial = &
    kim_argument_name_type(particle_virial_id)
  type(kim_argument_name_type), parameter :: &
    kim_argument_name_hessian = &
    kim_argument_name_type(hessian_id)

  interface operator (.eq.)
    module procedure kim_argument_name_equality
  end interface operator (.eq.)

  interface
    subroutine kim_argument_name_string(argument_name, name_string)
      import kim_argument_name_type
      implicit none
      type(kim_argument_name_type), intent(in), value :: argument_name
      character(len=*), intent(out) :: name_string
    end subroutine kim_argument_name_string

    subroutine kim_argument_name_get_number_of_arguments( &
      number_of_arguments)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_arguments
    end subroutine kim_argument_name_get_number_of_arguments

    subroutine kim_argument_name_get_argument_name(index, argument_name, &
      ierr)
      use, intrinsic :: iso_c_binding
      import kim_argument_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_argument_name_type), intent(out) :: argument_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_argument_name_get_argument_name

    subroutine kim_argument_name_get_argument_data_type(argument_name, &
      data_type, ierr)
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : kim_data_type_type
      import kim_argument_name_type
      implicit none
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: ierr
    end subroutine kim_argument_name_get_argument_data_type
  end interface

contains
  logical function kim_argument_name_equality(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_argument_name_type), intent(in) :: left
    type(kim_argument_name_type), intent(in) :: right

    kim_argument_name_equality &
      = (left%argument_name_id .eq. right%argument_name_id)
  end function kim_argument_name_equality
end module kim_argument_name_module
