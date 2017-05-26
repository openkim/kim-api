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


module kim_argument_name_id_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    number_of_particles_id, &
    particle_species_id, &
    particle_contributing_id, &
    coordinates_id, &
    energy_id, &
    forces_id, &
    particle_energy_id, &
    virial_id, &
    particle_virial_id, &
    hessian_id

  integer(c_int), parameter :: number_of_particles_id = 0
  integer(c_int), parameter :: particle_species_id = 1
  integer(c_int), parameter :: particle_contributing_id = 2
  integer(c_int), parameter :: coordinates_id = 3
  integer(c_int), parameter :: energy_id = 4
  integer(c_int), parameter :: forces_id = 5
  integer(c_int), parameter :: particle_energy_id = 6
  integer(c_int), parameter :: virial_id = 7
  integer(c_int), parameter :: particle_virial_id = 8
  integer(c_int), parameter :: hessian_id = 9
end module kim_argument_name_id_module
