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
! Copyright (c) 2016--2018, Regents of the University of Minnesota.
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
  implicit none
  private

  public &
    kim_compute_argument_name_type, &
    kim_compute_argument_name_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_compute_argument_name_string, &

    kim_compute_argument_name_number_of_particles, &
    kim_compute_argument_name_particle_species_codes, &
    kim_compute_argument_name_particle_contributing, &
    kim_compute_argument_name_coordinates, &
    kim_compute_argument_name_partial_energy, &
    kim_compute_argument_name_partial_forces, &
    kim_compute_argument_name_partial_particle_energy, &
    kim_compute_argument_name_partial_virial, &
    kim_compute_argument_name_partial_particle_virial, &

    kim_compute_argument_name_get_number_of_compute_argument_names, &
    kim_compute_argument_name_get_compute_argument_name, &
    kim_compute_argument_name_get_compute_argument_data_type

  type, bind(c) :: kim_compute_argument_name_type
    integer(c_int) compute_argument_name_id
  end type kim_compute_argument_name_type

  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles") &
    :: kim_compute_argument_name_number_of_particles
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes") &
    :: kim_compute_argument_name_particle_species_codes
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleContributing") &
    :: kim_compute_argument_name_particle_contributing
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_coordinates") &
    :: kim_compute_argument_name_coordinates
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialEnergy") &
    :: kim_compute_argument_name_partial_energy
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialForces") &
    :: kim_compute_argument_name_partial_forces
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy") &
    :: kim_compute_argument_name_partial_particle_energy
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialVirial") &
    :: kim_compute_argument_name_partial_virial
  type(kim_compute_argument_name_type), protected, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial") &
    :: kim_compute_argument_name_partial_particle_virial

  interface operator (.eq.)
    logical function kim_compute_argument_name_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_compute_argument_name_type
      implicit none
      type(kim_compute_argument_name_type), intent(in) :: left
      type(kim_compute_argument_name_type), intent(in) :: right
    end function kim_compute_argument_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_compute_argument_name_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_compute_argument_name_type
      implicit none
      type(kim_compute_argument_name_type), intent(in) :: left
      type(kim_compute_argument_name_type), intent(in) :: right
    end function kim_compute_argument_name_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_compute_argument_name_from_string(string, &
      compute_argument_name)
      import kim_compute_argument_name_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_compute_argument_name_type), intent(out) :: compute_argument_name
    end subroutine kim_compute_argument_name_from_string

    subroutine kim_compute_argument_name_string(compute_argument_name, string)
      import kim_compute_argument_name_type
      implicit none
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      character(len=*), intent(out) :: string
    end subroutine kim_compute_argument_name_string

    subroutine kim_compute_argument_name_get_number_of_compute_argument_names( &
      number_of_compute_argument_names)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_compute_argument_names
    end subroutine &
      kim_compute_argument_name_get_number_of_compute_argument_names

    subroutine kim_compute_argument_name_get_compute_argument_name(index, &
      compute_argument_name, ierr)
      use, intrinsic :: iso_c_binding
      import kim_compute_argument_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_compute_argument_name_type), intent(out) :: compute_argument_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_argument_name_get_compute_argument_name

    subroutine kim_compute_argument_name_get_compute_argument_data_type( &
      compute_argument_name, data_type, ierr)
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : kim_data_type_type
      import kim_compute_argument_name_type
      implicit none
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_argument_name_get_compute_argument_data_type
  end interface
end module kim_compute_argument_name_module
