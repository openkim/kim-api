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


module kim_compute_module
  use, intrinsic :: iso_c_binding
  use kim_compute_id_module
  implicit none
  private

  public &
    kim_compute_argument_name_type, &
    kim_compute_argument_name_number_of_particles, &
    kim_compute_argument_name_number_of_species, &
    kim_compute_argument_name_particle_species, &
    kim_compute_argument_name_particle_contributing, &
    kim_compute_argument_name_coordinates, &
    kim_compute_argument_name_process_dedr, &
    kim_compute_argument_name_process_d2edr2, &
    kim_compute_argument_name_energy, &
    kim_compute_argument_name_forces, &
    kim_compute_argument_name_particle_energy, &
    kim_compute_argument_name_virial, &
    kim_compute_argument_name_particle_virial, &
    kim_compute_argument_name_hessian, &
    kim_compute_argument_name_end

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
    kim_compute_argument_name_process_dedr = &
    kim_compute_argument_name_type(process_dedr_id)
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_process_d2edr2 = &
    kim_compute_argument_name_type(process_d2edr2_id)
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
  type(kim_compute_argument_name_type), parameter :: &
    kim_compute_argument_name_end = &
    kim_compute_argument_name_type(end_id)
end module kim_compute_module
