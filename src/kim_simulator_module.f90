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


module kim_simulator_module
  use, intrinsic :: iso_c_binding
  use :: kim_language_name_module
  implicit none
  !private  ! so kim_language_name_module is public

  public &
    kim_simulator_type, &
    kim_simulator_set_influence_distance, &
    kim_simulator_set_cutoffs, &
    kim_simulator_set_reinit, &
    kim_simulator_set_destroy, &
    kim_simulator_set_compute_func, &
    kim_simulator_print, &
    kim_simulator_get_num_model_species, &
    kim_simulator_get_model_species, &
    kim_simulator_get_num_sim_species, &
    kim_simulator_get_sim_species, &
    kim_simulator_get_species_code, &
    kim_simulator_set_species_code, &
    kim_simulator_set_parameter, &
    kim_simulator_set_parameter_description, &
    kim_simulator_set_model_buffer, &
    kim_simulator_get_model_buffer, &
    kim_simulator_get_unit_handling, &
    kim_simulator_get_unit_length, &
    kim_simulator_get_unit_energy, &
    kim_simulator_get_unit_charge, &
    kim_simulator_get_unit_temperature, &
    kim_simulator_get_unit_time, &
    kim_simulator_convert_to_act_unit


  type, bind(c) :: kim_simulator_type
    type(c_ptr) :: p
  end type kim_simulator_type

  interface
    subroutine kim_simulator_set_influence_distance(simulator, &
      influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      real(c_double), intent(in), target :: influence_distance
    end subroutine kim_simulator_set_influence_distance

    subroutine kim_simulator_set_cutoffs(simulator, number_of_cutoffs, &
      cutoffs)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      integer(c_int), intent(in), value :: number_of_cutoffs
      real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)
    end subroutine kim_simulator_set_cutoffs

    subroutine kim_simulator_set_reinit(simulator, language_name, fptr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine kim_simulator_set_reinit

    subroutine kim_simulator_set_destroy(simulator, language_name, fptr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine kim_simulator_set_destroy

    subroutine kim_simulator_set_compute_func(simulator, language_name, fptr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine kim_simulator_set_compute_func

    subroutine kim_simulator_print(simulator)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
    end subroutine kim_simulator_print

    subroutine kim_simulator_get_num_model_species(simulator, number_of_species)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(out) :: number_of_species
    end subroutine kim_simulator_get_num_model_species

    subroutine kim_simulator_get_model_species(simulator, index, species_name, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_get_model_species

    subroutine kim_simulator_get_num_sim_species(simulator, number_of_species)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(out) :: number_of_species
    end subroutine kim_simulator_get_num_sim_species

    subroutine kim_simulator_get_sim_species(simulator, index, species_name, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_get_sim_species

    subroutine kim_simulator_get_species_code(simulator, species_name, code, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_get_species_code

    subroutine kim_simulator_set_species_code(simulator, species_name, code, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_set_species_code

    subroutine kim_simulator_set_parameter(simulator, index, extent, ptr, ierr)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_set_parameter

    subroutine kim_simulator_set_parameter_description(simulator, index, &
      description, ierr)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      integer(c_int), intent(in), value :: index
      character(len=*), intent(in) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_set_parameter_description

    subroutine kim_simulator_set_model_buffer(simulator, ptr)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_simulator_set_model_buffer

    subroutine kim_simulator_get_model_buffer(simulator, ptr)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_simulator_get_model_buffer

    subroutine kim_simulator_get_unit_handling(simulator, flag, ierr)
      use, intrinsic :: iso_c_binding
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(out) :: flag
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_get_unit_handling

    subroutine kim_simulator_get_unit_length(simulator, length)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_length_unit_type), intent(out) :: length
    end subroutine kim_simulator_get_unit_length

    subroutine kim_simulator_get_unit_energy(simulator, energy)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_energy_unit_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_energy_unit_type), intent(out) :: energy
    end subroutine kim_simulator_get_unit_energy

    subroutine kim_simulator_get_unit_charge(simulator, charge)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_charge_unit_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_charge_unit_type), intent(out) :: charge
    end subroutine kim_simulator_get_unit_charge

    subroutine kim_simulator_get_unit_temperature(simulator, temperature)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_temperature_unit_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_temperature_unit_type), intent(out) :: temperature
    end subroutine kim_simulator_get_unit_temperature

    subroutine kim_simulator_get_unit_time(simulator, time)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_time_unit_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_time_unit_type), intent(out) :: time
    end subroutine kim_simulator_get_unit_time

    subroutine kim_simulator_convert_to_act_unit(simulator, length, energy, &
      charge, temperature, time, length_exponent, energy_exponent, &
      charge_exponent, temperature_exponent, time_exponent, factor, ierr)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_unit_system_module, only : kim_time_unit_type
      import kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_length_unit_type), intent(in), value :: length
      type(kim_energy_unit_type), intent(in), value :: energy
      type(kim_charge_unit_type), intent(in), value :: charge
      type(kim_temperature_unit_type), intent(in), value :: temperature
      type(kim_time_unit_type), intent(in), value :: time
      real(c_double), intent(in), value :: length_exponent
      real(c_double), intent(in), value :: energy_exponent
      real(c_double), intent(in), value :: charge_exponent
      real(c_double), intent(in), value :: temperature_exponent
      real(c_double), intent(in), value :: time_exponent
      real(c_double), intent(out) :: factor
      integer(c_int), intent(out) :: ierr
    end subroutine kim_simulator_convert_to_act_unit
end interface
end module kim_simulator_module
