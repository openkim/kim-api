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


module kim_compute_argument_name_module
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_id_module
  implicit none
  private

  public &
    kim_compute_argument_name_type, &
    kim_compute_argument_name_string, &

    kim_compute_argument_name_number_of_particles, &
    kim_compute_argument_name_number_of_species, &
    kim_compute_argument_name_particle_species, &
    kim_compute_argument_name_particle_contributing, &
    kim_compute_argument_name_coordinates, &
    kim_compute_argument_name_energy, &
    kim_compute_argument_name_forces, &
    kim_compute_argument_name_particle_energy, &
    kim_compute_argument_name_virial, &
    kim_compute_argument_name_particle_virial, &
    kim_compute_argument_name_hessian, &

    kim_compute_argument_name_get_number_of_arguments, &
    kim_compute_argument_name_get_argument, &
    kim_compute_argument_name_get_argument_data_type

  type, bind(c) :: kim_compute_argument_name_type
    integer(c_int) argument_id
  end type kim_compute_argument_name_type

  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_number_of_particles = &
    kim_compute_argument_name_type(number_of_particles_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_number_of_species = &
    kim_compute_argument_name_type(number_of_species_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_particle_species = &
    kim_compute_argument_name_type(particle_species_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_particle_contributing = &
    kim_compute_argument_name_type(particle_contributing_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_coordinates = &
    kim_compute_argument_name_type(coordinates_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_energy = &
    kim_compute_argument_name_type(energy_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_forces = &
    kim_compute_argument_name_type(forces_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_particle_energy = &
    kim_compute_argument_name_type(particle_energy_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_virial = &
    kim_compute_argument_name_type(virial_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_particle_virial = &
    kim_compute_argument_name_type(particle_virial_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_hessian = &
    kim_compute_argument_name_type(hessian_id)

  interface
    subroutine kim_compute_argument_name_string(argument_name, name_string)
      import kim_compute_argument_name_type
      implicit none
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      character(len=*), intent(out) :: name_string
    end subroutine kim_compute_argument_name_string

    subroutine kim_compute_argument_name_get_number_of_arguments( &
      number_of_arguments)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_arguments
    end subroutine kim_compute_argument_name_get_number_of_arguments

    subroutine kim_compute_argument_name_get_argument(index, argument_name, &
      ierr)
      use, intrinsic :: iso_c_binding
      import kim_compute_argument_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_compute_argument_name_type), intent(out) :: argument_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_argument_name_get_argument

    subroutine kim_compute_argument_name_get_argument_data_type(argument_name, &
      data_type, ierr)
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : kim_data_type_type
      import kim_compute_argument_name_type
      implicit none
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_argument_name_get_argument_data_type
  end interface
end module kim_compute_argument_name_module
