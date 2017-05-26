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


module kim_model_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_type, &
    kim_model_create, &
    kim_model_destroy, &
    kim_model_get_data, &
    kim_model_set_data, &
    kim_model_get_method, &
    kim_model_set_method, &
    kim_model_get_compute, &
    kim_model_set_compute, &
    kim_model_get_size, &
    kim_model_print, &
    kim_model_compute, &
    kim_model_get_neigh, &
    kim_model_init, &
    kim_model_reinit, &
    kim_model_destroy_model, &
    kim_model_get_num_model_species, &
    kim_model_get_model_species, &
    kim_model_get_num_sim_species, &
    kim_model_get_sim_species, &
    kim_model_get_model_kim_string_length, &
    kim_model_get_model_kim_string, &
    kim_model_get_species_code, &
    kim_model_set_species_code, &
    kim_model_get_num_params, &
    kim_model_get_parameter_data_type, &
    kim_model_get_parameter, &
    kim_model_set_parameter, &
    kim_model_get_parameter_description, &
    kim_model_set_parameter_description, &
    kim_model_set_model_buffer, &
    kim_model_get_model_buffer, &
    kim_model_set_sim_buffer, &
    kim_model_get_sim_buffer, &
    kim_model_process_dedr, &
    kim_model_process_d2edr2, &
    kim_model_get_unit_handling, &
    kim_model_get_unit_length, &
    kim_model_get_unit_energy, &
    kim_model_get_unit_charge, &
    kim_model_get_unit_temperature, &
    kim_model_get_unit_time, &
    kim_model_convert_to_act_unit


  type, bind(c) :: kim_model_type
    type(c_ptr) :: p
  end type kim_model_type

  interface
    subroutine kim_model_create(simulator_string, model_name, model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      character(len=*), intent(in) :: simulator_string
      character(len=*), intent(in) :: model_name
      type(kim_model_type), intent(out), pointer :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_create

    subroutine kim_model_destroy(model)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout), pointer :: model
    end subroutine kim_model_destroy

    subroutine kim_model_get_data(model, argument_name, ptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_data

    subroutine kim_model_set_data(model, argument_name, extent, ptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_data

    subroutine kim_model_get_method(model, argument_name, language_name, fptr, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_compute_module, only : &
        kim_compute_language_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(kim_compute_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(out) :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_method

    subroutine kim_model_set_method(model, argument_name, extent, &
      language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      use kim_compute_module, only : &
        kim_compute_language_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(kim_compute_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_method

    subroutine kim_model_get_compute(model, argument_name, flag, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: flag
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_compute

    subroutine kim_model_set_compute(model, argument_name, flag, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: flag
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_compute

    subroutine kim_model_get_size(model, argument_name, size, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_module, only : &
        kim_compute_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: size
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_size

    subroutine kim_model_print(model)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end subroutine kim_model_print

    subroutine kim_model_compute(model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute

    subroutine kim_model_get_neigh(model, particle_number, &
      number_of_neighbors, neighbors_of_particle, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: particle_number
      integer(c_int), intent(out) :: number_of_neighbors
      type(c_ptr), intent(out) :: neighbors_of_particle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_neigh

    subroutine kim_model_init(model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_init

    subroutine kim_model_reinit(model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_reinit

    subroutine kim_model_destroy_model(model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_destroy_model

    subroutine kim_model_get_num_model_species(model, number_of_species)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_species
    end subroutine kim_model_get_num_model_species

    subroutine kim_model_get_model_species(model, index, species_name, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_model_species

    subroutine kim_model_get_num_sim_species(model, number_of_species)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_species
    end subroutine kim_model_get_num_sim_species

    subroutine kim_model_get_sim_species(model, index, species_name, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_species_name_type), intent(out) :: species_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_sim_species

    subroutine kim_model_get_model_kim_string_length(model_name, &
      kim_string_length, ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*), intent(in) :: model_name
      integer(c_int), intent(out) :: kim_string_length
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_model_kim_string_length

    subroutine kim_model_get_model_kim_string(model_name, &
      kim_string, ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      character(len=*), intent(in) :: model_name
      character(len=*), intent(out) :: kim_string
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_model_kim_string

    subroutine kim_model_get_species_code(model, species_name, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_species_code

    subroutine kim_model_set_species_code(model, species_name, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_species_code

    subroutine kim_model_get_num_params(model, number_of_parameters)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine kim_model_get_num_params

    subroutine kim_model_get_parameter_data_type(model, index, data_type, ierr)
      use, intrinsic :: iso_c_binding
      use :: kim_parameter_module, only : kim_parameter_data_type_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_parameter_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_data_type

    subroutine kim_model_get_parameter(model, index, extent, ptr, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(out) :: extent
      type(c_ptr), intent(out) :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter

    subroutine kim_model_set_parameter(model, index, extent, ptr, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: index
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_parameter

    subroutine kim_model_get_parameter_description(model, index, description, &
      ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      character(len=*), intent(out) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_description

    subroutine kim_model_set_parameter_description(model, index, description, &
      ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: index
      character(len=*), intent(in) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_parameter_description

    subroutine kim_model_set_model_buffer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_set_model_buffer

    subroutine kim_model_get_model_buffer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_get_model_buffer

    subroutine kim_model_set_sim_buffer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_set_sim_buffer

    subroutine kim_model_get_sim_buffer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_get_sim_buffer

    subroutine kim_model_process_dedr(model, de, r, dx, i, j, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(in), value :: de
      real(c_double), intent(in), value :: r
      type(c_ptr), intent(in) :: dx
      integer(c_int), intent(in), value :: i
      integer(c_int), intent(in), value :: j
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_process_dedr

    subroutine kim_model_process_d2edr2(model, de, r, dx, i, j, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(in), value :: de
      type(c_ptr), intent(in), value :: r
      type(c_ptr), intent(in), value :: dx
      type(c_ptr), intent(in), value :: i
      type(c_ptr), intent(in), value :: j
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_process_d2edr2

    subroutine kim_model_get_unit_handling(model, flag, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: flag
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_unit_handling

    subroutine kim_model_get_unit_length(model, length)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_length_unit_type), intent(out) :: length
    end subroutine kim_model_get_unit_length

    subroutine kim_model_get_unit_energy(model, energy)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_energy_unit_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_energy_unit_type), intent(out) :: energy
    end subroutine kim_model_get_unit_energy

    subroutine kim_model_get_unit_charge(model, charge)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_charge_unit_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_charge_unit_type), intent(out) :: charge
    end subroutine kim_model_get_unit_charge

    subroutine kim_model_get_unit_temperature(model, temperature)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_temperature_unit_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_temperature_unit_type), intent(out) :: temperature
    end subroutine kim_model_get_unit_temperature

    subroutine kim_model_get_unit_time(model, time)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_time_unit_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_time_unit_type), intent(out) :: time
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
      import kim_model_type
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
    end subroutine kim_model_convert_to_act_unit
end interface
end module kim_model_module
