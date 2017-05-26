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


module kim_model_f_module
  implicit none
  private

  public &
    create, &
    destroy, &
    get_data, &
    set_data, &
    get_method, &
    set_method, &
    get_compute, &
    set_compute, &
    get_size, &
    print_model, &
    compute, &
    get_neigh, &
    init, &
    reinit, &
    destroy_model, &
    get_num_model_species, &
    get_model_species, &
    get_num_sim_species, &
    get_sim_species, &
    get_model_kim_string_length, &
    get_model_kim_string, &
    get_species_code, &
    set_species_code, &
    get_num_params, &
    get_parameter_data_type, &
    get_parameter, &
    set_parameter, &
    get_parameter_description, &
    set_parameter_description, &
    set_model_buffer, &
    get_model_buffer, &
    set_sim_buffer, &
    get_sim_buffer, &
    process_dedr, &
    process_d2edr2, &
    get_unit_handling, &
    get_unit_length, &
    get_unit_energy, &
    get_unit_charge, &
    get_unit_temperature, &
    get_unit_time, &
    convert_to_act_unit

  interface
    integer(c_int) function create(simulator_string, model_name, &
      model) bind(c, name="KIM_Model_create")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      character(c_char), intent(in) :: simulator_string(*)
      character(c_char), intent(in) :: model_name(*)
      type(c_ptr), intent(out) :: model
    end function create

    subroutine destroy(model) bind(c, name="KIM_Model_destroy")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(c_ptr), intent(inout) :: model
    end subroutine destroy

    integer(c_int) function get_data(model, argument_name, ptr) &
      bind(c, name="KIM_Model_get_data")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
    end function get_data

    integer(c_int) function set_data(model, argument_name, extent, ptr) &
      bind(c, name="KIM_Model_set_data")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
    end function set_data

    integer(c_int) function get_method(model, argument_name, language_name, &
      fptr) bind(c, name="KIM_Model_get_method")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_compute_module, only : &
        kim_compute_language_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(kim_compute_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(out) :: fptr
    end function get_method

    integer(c_int) function set_method(model, argument_name, extent, &
      language_name, fptr) bind(c, name="KIM_Model_set_method")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_compute_module, only : &
        kim_compute_language_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(kim_compute_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_method

    integer(c_int) function get_compute(model, argument_name, flag) &
      bind(c, name="KIM_Model_get_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: flag
    end function get_compute

    integer(c_int) function set_compute(model, argument_name, flag) &
      bind(c, name="KIM_Model_set_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: flag
    end function set_compute

    integer(c_int) function get_size(model, argument_name, size) &
      bind(c, name="KIM_Model_get_size")
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: size
    end function get_size

    subroutine print_model(model) bind(c, name="KIM_Model_print")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end subroutine print_model

    integer(c_int) function compute(model) bind(c, name="KIM_Model_compute")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end function compute

    integer(c_int) function get_neigh(model, particle_number, &
      number_of_neighbors, neighbors_of_particle) &
      bind(c, name="KIM_Model_get_neigh")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: particle_number
      integer(c_int), intent(out) :: number_of_neighbors
      type(c_ptr), intent(out) :: neighbors_of_particle
    end function get_neigh

    integer(c_int) function init(model) bind(c, name="KIM_Model_init")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function init

    integer(c_int) function reinit(model) bind(c, name="KIM_Model_reinit")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function reinit

    integer(c_int) function destroy_model(model) &
      bind(c, name="KIM_Model_destroy_model")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function destroy_model

    subroutine get_num_model_species(model, number_of_species) &
      bind(c, name="KIM_Model_get_num_model_species")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_num_model_species

    integer(c_int) function get_model_species(model, index, species_name) &
      bind(c, name="KIM_Model_get_model_species")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_model_species

    subroutine get_num_sim_species(model, number_of_species) &
      bind(c, name="KIM_Model_get_num_sim_species")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_species
    end subroutine get_num_sim_species

    integer(c_int) function get_sim_species(model, index, species_name) &
      bind(c, name="KIM_Model_get_sim_species")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
    end function get_sim_species

    integer(c_int) function get_model_kim_string_length(model_name, &
      kim_string_length) bind(c, name="KIM_Model_get_model_kim_string_length")
      use, intrinsic :: iso_c_binding
      implicit none
      character(c_char), intent(in) :: model_name(*)
      integer(c_int), intent(out) :: kim_string_length
    end function get_model_kim_string_length

    integer(c_int) function get_model_kim_string(model_name, kim_string) &
      bind(c, name="KIM_Model_get_model_kim_string")
      use, intrinsic :: iso_c_binding
      implicit none
      character(c_char), intent(in) :: model_name(*)
      type(c_ptr), intent(out) :: kim_string
    end function get_model_kim_string

    integer(c_int) function get_species_code(model, species_name, code) &
      bind(c, name="KIM_Model_get_species_code")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: code
    end function get_species_code

    integer(c_int) function set_species_code(model, species_name, code) &
      bind(c, name="KIM_Model_set_species_code")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
    end function set_species_code

    subroutine get_num_params(model, number_of_parameters) &
      bind(c, name="KIM_Model_get_num_parames")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine get_num_params

    integer(c_int) function get_parameter_data_type(model, index, data_type) &
      bind(c, name="KIM_Model_get_parameter_data_type")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      use kim_parameter_module, only : kim_parameter_data_type_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_parameter_data_type_type), intent(out) :: data_type
    end function get_parameter_data_type

    integer(c_int) function get_parameter(model, index, extent, ptr) &
      bind(c, name="KIM_Model_get_parameter")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(out) :: extent
      type(c_ptr), intent(out) :: ptr
    end function get_parameter

    integer(c_int) function set_parameter(model, index, extent, ptr) &
      bind(c, name="KIM_Model_set_parameter")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
    end function set_parameter

    integer(c_int) function get_parameter_description(model, index, &
      description) bind(c, name="KIM_Model_get_parameter_description")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(c_ptr), intent(out) :: description
    end function get_parameter_description

    integer(c_int) function set_parameter_description(model, index, &
      description) bind(c, name="KIM_Model_set_parameter_description")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: index
      character(c_char), intent(in) :: description(*)
    end function set_parameter_description

    subroutine set_model_buffer(model, ptr) &
      bind(c, name="KIM_Model_set_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_model_buffer

    subroutine get_model_buffer(model, ptr) &
      bind(c, name="KIM_Model_get_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine get_model_buffer

    subroutine set_sim_buffer(model, ptr) &
      bind(c, name="KIM_Model_set_sim_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_sim_buffer

    subroutine get_sim_buffer(model, ptr) &
      bind(c, name="KIM_Model_get_sim_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine get_sim_buffer

    integer(c_int) function process_dedr(model, de, r, dx, i, j) &
      bind(c, name="KIM_Model_process_dedr")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(in), value :: de
      real(c_double), intent(in), value :: r
      real(c_double), intent(in) :: dx
      real(c_double), intent(in), value :: i
      real(c_double), intent(in), value :: j
    end function process_dedr

    integer(c_int) function process_d2edr2(model, de, r, dx, i, j) &
      bind(c, name="KIM_Model_process_d2edr2")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(in), value :: de
      type(c_ptr), intent(in), value :: r
      type(c_ptr), intent(in), value :: dx
      type(c_ptr), intent(in), value :: i
      type(c_ptr), intent(in), value :: j
    end function process_d2edr2

    integer(c_int) function get_unit_handling(model, flag) &
      bind(c, name="KIM_Model_get_unit_handling")
      use, intrinsic :: iso_c_binding
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: flag
    end function get_unit_handling

    subroutine get_unit_length(model, length) &
      bind(c, name="KIM_Model_get_unit_length")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_length_unit_type), intent(out) :: length
    end subroutine get_unit_length

    subroutine get_unit_energy(model, energy) &
      bind(c, name="KIM_Model_get_unit_energy")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_energy_unit_type), intent(out) :: energy
    end subroutine get_unit_energy

    subroutine get_unit_charge(model, charge) &
      bind(c, name="KIM_Model_get_unit_charge")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_charge_unit_type), intent(out) :: charge
    end subroutine get_unit_charge

    subroutine get_unit_temperature(model, temperature) &
      bind(c, name="KIM_Model_get_unit_temperature")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_temperature_unit_type), intent(out) :: temperature
    end subroutine get_unit_temperature

    subroutine get_unit_time(model, time) &
      bind(c, name="KIM_Model_get_unit_time")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_time_unit_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_time_unit_type), intent(out) :: time
    end subroutine get_unit_time

    integer(c_int) function convert_to_act_unit(model, length, energy, charge, &
      temperature, time, length_exponent, energy_exponent, charge_exponent, &
      temperature_exponent, time_exponent, factor) &
      bind(c, name="KIM_Model_convert_to_act_unit")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_unit_system_module, only : kim_time_unit_type
      use kim_model_module, only : kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
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
end module kim_model_f_module


! free functions to implement kim_model_module

subroutine kim_model_create(simulator_string, model_name, model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : create
  implicit none
  character(len=*), intent(in) :: simulator_string
  character(len=*), intent(in) :: model_name
  type(kim_model_type), intent(out), pointer :: model
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: pmodel

  ierr = create(trim(simulator_string)//c_null_char, &
    trim(model_name)//c_null_char, pmodel)
  call c_f_pointer(pmodel, model)
end subroutine kim_model_create

subroutine kim_model_destroy(model)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : destroy
  implicit none
  type(kim_model_type), intent(inout), pointer :: model

  type(c_ptr) :: pmodel
  pmodel = c_loc(model)
  call destroy(pmodel)
  nullify(model)
end subroutine kim_model_destroy

subroutine kim_model_get_data(model, argument_name, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_data
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  type(c_ptr), intent(out) :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = get_data(model, argument_name, ptr)
end subroutine kim_model_get_data

subroutine kim_model_set_data(model, argument_name, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_data
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent
  type(c_ptr), intent(in), value :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = set_data(model, argument_name, extent, ptr)
end subroutine kim_model_set_data

subroutine kim_model_get_method(model, argument_name, language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_compute_module, only : &
    kim_compute_language_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_method
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  type(kim_compute_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(out) :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = get_method(model, argument_name, language_name, fptr)
end subroutine kim_model_get_method

subroutine kim_model_set_method(model, argument_name, extent, language_name, &
  fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_compute_module, only : &
    kim_compute_language_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_method
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent
  type(kim_compute_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_method(model, argument_name, extent, language_name, fptr)
end subroutine kim_model_set_method

subroutine kim_model_get_compute(model, argument_name, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_compute
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out) :: flag
  integer(c_int), intent(out) :: ierr

  ierr = get_compute(model, argument_name, flag)
end subroutine kim_model_get_compute

subroutine kim_model_set_compute(model, argument_name, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_compute
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: flag
  integer(c_int), intent(out) :: ierr

  ierr = set_compute(model, argument_name, flag)
end subroutine kim_model_set_compute

subroutine kim_model_get_size(model, argument_name, size, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_module, only : &
    kim_compute_argument_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_size
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out) :: size
  integer(c_int), intent(out) :: ierr

  ierr = get_size(model, argument_name, size)
end subroutine kim_model_get_size

subroutine kim_model_print(model)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : print_model
  implicit none
  type(kim_model_type), intent(in) :: model

  call print_model(model)
end subroutine kim_model_print

subroutine kim_model_compute(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : compute
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: ierr

  ierr = compute(model)
end subroutine kim_model_compute

subroutine kim_model_get_neigh(model, particle_number, &
  number_of_neighbors, neighbors_of_particle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_neigh
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: particle_number
  integer(c_int), intent(out) :: number_of_neighbors
  type(c_ptr), intent(out) :: neighbors_of_particle
  integer(c_int), intent(out) :: ierr

  ierr = get_neigh(model, particle_number, number_of_neighbors, &
    neighbors_of_particle)
end subroutine kim_model_get_neigh

subroutine kim_model_init(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : init
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(out) :: ierr

  ierr = init(model)
end subroutine kim_model_init

subroutine kim_model_reinit(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : reinit
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(out) :: ierr

  ierr = reinit(model)
end subroutine kim_model_reinit

subroutine kim_model_destroy_model(model, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : destroy_model
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(out) :: ierr

  ierr = destroy_model(model)
end subroutine kim_model_destroy_model

subroutine kim_model_get_num_model_species(model, number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_num_model_species
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: number_of_species

  call get_num_model_species(model, number_of_species)
end subroutine kim_model_get_num_model_species

subroutine kim_model_get_model_species(model, index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_model_species
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_model_species(model, index, species_name)
end subroutine kim_model_get_model_species

subroutine kim_model_get_num_sim_species(model, number_of_species)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_num_sim_species
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: number_of_species

  call get_num_sim_species(model, number_of_species)
end subroutine kim_model_get_num_sim_species

subroutine kim_model_get_sim_species(model, index, species_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_sim_species
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  type(kim_species_name_type), intent(out) :: species_name
  integer(c_int), intent(out) :: ierr

  ierr = get_sim_species(model, index, species_name)
end subroutine kim_model_get_sim_species

subroutine kim_model_get_model_kim_string_length(model_name, &
  kim_string_length, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_f_module, only : get_model_kim_string_length
  implicit none
  character(len=*), intent(in) :: model_name
  integer(c_int), intent(out) :: kim_string_length
  integer(c_int), intent(out) :: ierr

  ierr = get_model_kim_string_length(trim(model_name)//c_null_char, &
    kim_string_length)
end subroutine kim_model_get_model_kim_string_length

subroutine kim_model_get_model_kim_string(model_name, &
  kim_string, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_f_module, only : get_model_kim_string
  implicit none
  character(len=*), intent(in) :: model_name
  character(len=*), intent(out) :: kim_string
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: p
  character(len=len(kim_string)), pointer :: fp
  integer(c_int) :: null_index

  ierr = get_model_kim_string(trim(model_name)//c_null_char, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  kim_string = fp(1:null_index)
end subroutine kim_model_get_model_kim_string

subroutine kim_model_get_species_code(model, species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_species_code
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(out) :: code
  integer(c_int), intent(out) :: ierr

  ierr = get_species_code(model, species_name, code)
end subroutine kim_model_get_species_code

subroutine kim_model_set_species_code(model, species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_species_code
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(in), value :: code
  integer(c_int), intent(out) :: ierr

  ierr = set_species_code(model, species_name, code)
end subroutine kim_model_set_species_code

subroutine kim_model_get_num_params(model, number_of_parameters)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_num_params
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: number_of_parameters

  call get_num_params(model, number_of_parameters)
end subroutine kim_model_get_num_params

subroutine kim_model_get_parameter_data_type(model, index, data_type, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_parameter_module, only : kim_parameter_data_type_type
  use kim_model_f_module, only : get_parameter_data_type
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  type(kim_parameter_data_type_type), intent(out) :: data_type
  integer(c_int), intent(out) :: ierr

  ierr = get_parameter_data_type(model, index-1, data_type)
end subroutine kim_model_get_parameter_data_type

subroutine kim_model_get_parameter(model, index, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_parameter
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  integer(c_int), intent(out) :: extent
  type(c_ptr), intent(out) :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = get_parameter(model, index-1, extent, ptr)
end subroutine kim_model_get_parameter

subroutine kim_model_set_parameter(model, index, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_parameter
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(in), value :: index
  integer(c_int), intent(in), value :: extent
  type(c_ptr), intent(in), value :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = set_parameter(model, index-1, extent, ptr)
end subroutine kim_model_set_parameter

subroutine kim_model_get_parameter_description(model, index, description, &
  ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_parameter_description
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(in), value :: index
  character(len=*), intent(out) :: description
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: p
  character(len=len(description)), pointer :: fp
  integer(c_int) :: null_index

  ierr = get_parameter_description(model, index-1, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  description = fp(1:null_index)
end subroutine kim_model_get_parameter_description

subroutine kim_model_set_parameter_description(model, index, description, &
  ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_parameter_description
  implicit none
  type(kim_model_type), intent(inout) :: model
  integer(c_int), intent(in), value :: index
  character(len=*), intent(in) :: description
  integer(c_int), intent(out) :: ierr

  ierr = set_parameter_description(model, index-1, &
    trim(description)//c_null_char)
end subroutine kim_model_set_parameter_description

subroutine kim_model_set_model_buffer(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_model_buffer
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(c_ptr), intent(in), value :: ptr

  call set_model_buffer(model, ptr)
end subroutine kim_model_set_model_buffer

subroutine kim_model_get_model_buffer(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_model_buffer
  implicit none
  type(kim_model_type), intent(in) :: model
  type(c_ptr), intent(out) :: ptr

  call get_model_buffer(model, ptr)
end subroutine kim_model_get_model_buffer

subroutine kim_model_set_sim_buffer(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : set_sim_buffer
  implicit none
  type(kim_model_type), intent(inout) :: model
  type(c_ptr), intent(in), value :: ptr

  call set_sim_buffer(model, ptr)
end subroutine kim_model_set_sim_buffer

subroutine kim_model_get_sim_buffer(model, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_sim_buffer
  implicit none
  type(kim_model_type), intent(in) :: model
  type(c_ptr), intent(out) :: ptr

  call get_sim_buffer(model, ptr)
end subroutine kim_model_get_sim_buffer

subroutine kim_model_process_dedr(model, de, r, dx, i, j, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : process_dedr
  implicit none
  type(kim_model_type), intent(in) :: model
  real(c_double), intent(in), value :: de
  real(c_double), intent(in), value :: r
  real(c_double), intent(in) :: dx
  real(c_double), intent(in), value :: i
  real(c_double), intent(in), value :: j
  integer(c_int), intent(out) :: ierr

  ierr = process_dedr(model, de, r, dx, i, j)
end subroutine kim_model_process_dedr

subroutine kim_model_process_d2edr2(model, de, r, dx, i, j, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : process_d2edr2
  implicit none
  type(kim_model_type), intent(in) :: model
  real(c_double), intent(in), value :: de
  type(c_ptr), intent(in), value :: r
  type(c_ptr), intent(in), value :: dx
  type(c_ptr), intent(in), value :: i
  type(c_ptr), intent(in), value :: j
  integer(c_int), intent(out) :: ierr

  ierr = process_d2edr2(model, de, r, dx, i, j)
end subroutine kim_model_process_d2edr2

subroutine kim_model_get_unit_handling(model, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_unit_handling
  implicit none
  type(kim_model_type), intent(in) :: model
  integer(c_int), intent(out) :: flag
  integer(c_int), intent(out) :: ierr

  ierr = get_unit_handling(model, flag)
end subroutine kim_model_get_unit_handling

subroutine kim_model_get_unit_length(model, length)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_length_unit_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_unit_length
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_length_unit_type), intent(out) :: length

  call get_unit_length(model, length)
end subroutine kim_model_get_unit_length

subroutine kim_model_get_unit_energy(model, energy)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_energy_unit_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_unit_energy
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_energy_unit_type), intent(out) :: energy

  call get_unit_energy(model, energy)
end subroutine kim_model_get_unit_energy

subroutine kim_model_get_unit_charge(model, charge)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_charge_unit_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_unit_charge
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_charge_unit_type), intent(out) :: charge

  call get_unit_charge(model, charge)
end subroutine kim_model_get_unit_charge

subroutine kim_model_get_unit_temperature(model, temperature)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_temperature_unit_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_unit_temperature
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_temperature_unit_type), intent(out) :: temperature

  call get_unit_temperature(model, temperature)
end subroutine kim_model_get_unit_temperature

subroutine kim_model_get_unit_time(model, time)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_time_unit_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : get_unit_time
  implicit none
  type(kim_model_type), intent(in) :: model
  type(kim_time_unit_type), intent(out) :: time

  call get_unit_time(model, time)
end subroutine kim_model_get_unit_time

subroutine kim_model_convert_to_act_unit(model, length, energy, charge, &
  temperature, time, length_exponent, energy_exponent, charge_exponent, &
  temperature_exponent, time_exponent, factor, ierr)
  use, intrinsic :: iso_c_binding
  use kim_unit_system_module, only : kim_length_unit_type
  use kim_unit_system_module, only : kim_energy_unit_type
  use kim_unit_system_module, only : kim_charge_unit_type
  use kim_unit_system_module, only : kim_temperature_unit_type
  use kim_unit_system_module, only : kim_time_unit_type
  use kim_model_module, only : kim_model_type
  use kim_model_f_module, only : convert_to_act_unit
  implicit none
  type(kim_model_type), intent(in) :: model
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

  ierr = convert_to_act_unit(model, length, energy, charge, temperature, &
    time, length_exponent, energy_exponent, charge_exponent, &
    temperature_exponent, time_exponent, factor)
end subroutine kim_model_convert_to_act_unit
