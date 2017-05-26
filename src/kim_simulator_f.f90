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


module kim_simulator_f_module
  implicit none
  private

  public &
    set_influence_distance, &
    set_cutoffs, &
    set_reinit, &
    set_destroy, &
    set_compute_func, &
    print_model, &
    get_num_model_species, &
    get_model_species, &
    get_num_sim_species, &
    get_sim_species, &
    get_species_code, &
    set_species_code, &
    set_parameter, &
    set_parameter_description, &
    set_model_buffer, &
    get_model_buffer, &
    get_unit_handling, &
    get_unit_length, &
    get_unit_energy, &
    get_unit_charge, &
    get_unit_temperature, &
    get_unit_time, &
    convert_to_act_unit

  interface
    subroutine set_influence_distance(simulator, influence_distance) &
      bind(c, name="KIM_Simulator_set_influence_distance")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(c_ptr), intent(in), value :: influence_distance
    end subroutine set_influence_distance

    subroutine set_cutoffs(simulator, number_of_cutoffs, cutoffs_ptr) &
      bind(c, name="KIM_Simulator_set_cutoffs")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      integer(c_int), intent(in), value :: number_of_cutoffs
      type(c_ptr), intent(in), value :: cutoffs_ptr
    end subroutine set_cutoffs

    subroutine set_reinit(simulator, language_name, fptr) &
      bind(c, name="KIM_Simulator_set_reinit")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_reinit

    subroutine set_destroy(simulator, language_name, fptr) &
      bind(c, name="KIM_Simulator_set_destroy")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_destroy

    subroutine set_compute_func(simulator, language_name, fptr) &
      bind(c, name="KIM_Simulator_set_compute_func")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_compute_func

    subroutine print_model(simulator) bind(c, name="KIM_Simulator_print")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
    end subroutine print_model

    subroutine get_num_model_species(simulator, number_of_species) &
      bind(c, name="KIM_Simulator_get_num_model_species")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_num_model_species

    integer(c_int) function get_model_species(simulator, index, species_name) &
      bind(c, name="KIM_Simulator_get_model_species")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_model_species

    subroutine get_num_sim_species(simulator, number_of_species) &
      bind(c, name="KIM_Simulator_get_num_sim_species")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_num_sim_species

    integer(c_int) function get_sim_species(simulator, index, species_name) &
      bind(c, name="KIM_Simulator_get_sim_species")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_sim_species

    integer(c_int) function get_species_code(simulator, species_name, code) &
      bind(c, name="KIM_Simulator_get_species_code")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: code
    end function get_species_code

    integer(c_int) function set_species_code(simulator, species_name, code) &
      bind(c, name="KIM_Simulator_set_species_code")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
    end function set_species_code

    integer(c_int) function set_parameter(simulator, index, extent, ptr) &
      bind(c, name="KIM_Simulator_set_parameter")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
    end function set_parameter

    integer(c_int) function set_parameter_description(simulator, index, &
      description) bind(c, name="KIM_Simulator_set_parameter_description")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      integer(c_int), intent(in), value :: index
      character(c_char), intent(in) :: description(*)
    end function set_parameter_description

    subroutine set_model_buffer(simulator, ptr) &
      bind(c, name="KIM_Simulator_set_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(inout) :: simulator
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_model_buffer

    subroutine get_model_buffer(simulator, ptr) &
      bind(c, name="KIM_Simulator_get_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(c_ptr), intent(out) :: ptr
    end subroutine get_model_buffer

    integer(c_int) function get_unit_handling(simulator, flag) &
      bind(c, name="KIM_Simulator_get_unit_handling")
      use, intrinsic :: iso_c_binding
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      integer(c_int), intent(out) :: flag
    end function get_unit_handling

    subroutine get_unit_length(simulator, length) &
      bind(c, name="KIM_Simulator_get_unit_length")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_length_unit_type), intent(out) :: length
    end subroutine get_unit_length

    subroutine get_unit_energy(simulator, energy) &
      bind(c, name="KIM_Simulator_get_unit_energy")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_energy_unit_type), intent(out) :: energy
    end subroutine get_unit_energy

    subroutine get_unit_charge(simulator, charge) &
      bind(c, name="KIM_Simulator_get_unit_charge")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_charge_unit_type), intent(out) :: charge
    end subroutine get_unit_charge

    subroutine get_unit_temperature(simulator, temperature) &
      bind(c, name="KIM_Simulator_get_unit_temperature")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_temperature_unit_type), intent(out) :: temperature
    end subroutine get_unit_temperature

    subroutine get_unit_time(simulator, time) &
      bind(c, name="KIM_Simulator_get_unit_time")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_time_unit_type
      use kim_simulator_module, only : kim_simulator_type
      implicit none
      type(kim_simulator_type), intent(in) :: simulator
      type(kim_time_unit_type), intent(out) :: time
    end subroutine get_unit_time

    integer(c_int) function convert_to_act_unit(simulator, length, energy, &
      charge, temperature, time, length_exponent, energy_exponent, &
      charge_exponent, temperature_exponent, time_exponent, factor) &
      bind(c, name="KIM_Simulator_convert_to_act_unit")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_unit_system_module, only : kim_time_unit_type
      use kim_simulator_module, only : kim_simulator_type
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
    end function convert_to_act_unit
  end interface
end module kim_simulator_f_module


! free functions to implement kim_simulator_module

subroutine kim_simulator_set_influence_distance(simulator, &
  influence_distance)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_influence_distance
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  real(c_double), intent(in), target :: influence_distance

  call set_influence_distance(simulator, c_loc(influence_distance))
end subroutine kim_simulator_set_influence_distance

subroutine kim_simulator_set_cutoffs(simulator, number_of_cutoffs, cutoffs)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_cutoffs
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  integer(c_int), intent(in), value :: number_of_cutoffs
  real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)

  call set_cutoffs(simulator, number_of_cutoffs, c_loc(cutoffs))
end subroutine kim_simulator_set_cutoffs

subroutine kim_simulator_set_reinit(simulator, language_name, fptr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_reinit
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr

  call set_reinit(simulator, language_name, fptr)
end subroutine kim_simulator_set_reinit

subroutine kim_simulator_set_destroy(simulator, language_name, fptr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_destroy
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr

  call set_destroy(simulator, language_name, fptr)
end subroutine kim_simulator_set_destroy

subroutine kim_simulator_set_compute_func(simulator, language_name, fptr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_compute_func
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr

  call set_compute_func(simulator, language_name, fptr)
end subroutine kim_simulator_set_compute_func

subroutine kim_simulator_print(simulator)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : print_model
  implicit none
  type(kim_simulator_type), intent(in) :: simulator

  call print_model(simulator)
end subroutine kim_simulator_print

subroutine kim_simulator_get_num_model_species(simulator, number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_num_model_species
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  integer(c_int), intent(out) :: number_of_species

  call get_num_model_species(simulator, number_of_species)
end subroutine kim_simulator_get_num_model_species

subroutine kim_simulator_get_model_species(simulator, index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_model_species
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_model_species(simulator, index, species_name)
end subroutine kim_simulator_get_model_species

subroutine kim_simulator_get_num_sim_species(simulator, number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_num_sim_species
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  integer(c_int), intent(out) :: number_of_species

  call get_num_sim_species(simulator, number_of_species)
end subroutine kim_simulator_get_num_sim_species

subroutine kim_simulator_get_sim_species(simulator, index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_sim_species
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_sim_species(simulator, index, species_name)
end subroutine kim_simulator_get_sim_species

subroutine kim_simulator_get_species_code(simulator, species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_species_code
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(out) :: code
  integer(c_int), intent(out) :: ierr

  ierr = get_species_code(simulator, species_name, code)
end subroutine kim_simulator_get_species_code

subroutine kim_simulator_set_species_code(simulator, species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_species_code
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(in), value :: code
  integer(c_int), intent(out) :: ierr

  ierr = set_species_code(simulator, species_name, code)
end subroutine kim_simulator_set_species_code

subroutine kim_simulator_set_parameter(simulator, index, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_parameter
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  integer(c_int), intent(in), value :: index
  integer(c_int), intent(in), value :: extent
  type(c_ptr), intent(in), value :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = set_parameter(simulator, index-1, extent, ptr)
end subroutine kim_simulator_set_parameter

subroutine kim_simulator_set_parameter_description(simulator, index, &
  description, ierr)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_parameter_description
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  integer(c_int), intent(in), value :: index
  character(len=*), intent(in) :: description
  integer(c_int), intent(out) :: ierr

  ierr = set_parameter_description(simulator, index-1, &
    trim(description)//c_null_char)
end subroutine kim_simulator_set_parameter_description

subroutine kim_simulator_set_model_buffer(simulator, ptr)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : set_model_buffer
  implicit none
  type(kim_simulator_type), intent(inout) :: simulator
  type(c_ptr), intent(in), value :: ptr

  call set_model_buffer(simulator, ptr)
end subroutine kim_simulator_set_model_buffer

subroutine kim_simulator_get_model_buffer(simulator, ptr)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_model_buffer
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(c_ptr), intent(out) :: ptr

  call get_model_buffer(simulator, ptr)
end subroutine kim_simulator_get_model_buffer

subroutine kim_simulator_get_unit_handling(simulator, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_unit_handling
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  integer(c_int), intent(out) :: flag
  integer(c_int), intent(out) :: ierr

  ierr = get_unit_handling(simulator, flag)
end subroutine kim_simulator_get_unit_handling

subroutine kim_simulator_get_unit_length(simulator, length)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_length_unit_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_unit_length
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(kim_length_unit_type), intent(out) :: length

  call get_unit_length(simulator, length)
end subroutine kim_simulator_get_unit_length

subroutine kim_simulator_get_unit_energy(simulator, energy)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_energy_unit_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_unit_energy
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(kim_energy_unit_type), intent(out) :: energy

  call get_unit_energy(simulator, energy)
end subroutine kim_simulator_get_unit_energy

subroutine kim_simulator_get_unit_charge(simulator, charge)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_charge_unit_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_unit_charge
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(kim_charge_unit_type), intent(out) :: charge

  call get_unit_charge(simulator, charge)
end subroutine kim_simulator_get_unit_charge

subroutine kim_simulator_get_unit_temperature(simulator, temperature)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_temperature_unit_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_unit_temperature
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(kim_temperature_unit_type), intent(out) :: temperature

  call get_unit_temperature(simulator, temperature)
end subroutine kim_simulator_get_unit_temperature

subroutine kim_simulator_get_unit_time(simulator, time)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_time_unit_type
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : get_unit_time
  implicit none
  type(kim_simulator_type), intent(in) :: simulator
  type(kim_time_unit_type), intent(out) :: time

  call get_unit_time(simulator, time)
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
  use kim_simulator_module, only : kim_simulator_type
  use kim_simulator_f_module, only : convert_to_act_unit
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

  ierr = convert_to_act_unit(simulator, length, energy, charge, temperature, &
    time, length_exponent, energy_exponent, charge_exponent, &
    temperature_exponent, time_exponent, factor)
end subroutine kim_simulator_convert_to_act_unit
