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


module kim_model_module
  use, intrinsic :: iso_c_binding
  use :: kim_language_name_module
  implicit none
  private

  public &
    kim_model_handle_type, &
    kim_model_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_create, &
    kim_model_destroy, &
    kim_model_get_influence_distance, &
    kim_model_get_number_of_neighbor_lists, &
    kim_model_get_neighbor_list_values, &
    kim_model_get_units, &
    kim_model_compute_arguments_create, &
    kim_model_compute_arguments_destroy, &
    kim_model_compute, &
    kim_model_clear_then_refresh, &
    kim_model_get_species_support_and_code, &
    kim_model_get_number_of_parameters, &
    kim_model_get_parameter_data_type_extent_and_description, &
    kim_model_get_parameter, &
    kim_model_set_parameter, &
    kim_model_set_simulator_buffer_pointer, &
    kim_model_get_simulator_buffer_pointer, &
    kim_model_string, &
    kim_model_set_log_id, &
    kim_model_push_log_verbosity, &
    kim_model_pop_log_verbosity

  type, bind(c) :: kim_model_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_handle_type

  type(kim_model_handle_type), protected, save &
    :: kim_model_null_handle

  interface operator (.eq.)
    logical function kim_model_handle_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: left
      type(kim_model_handle_type), intent(in) :: right
    end function kim_model_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_model_handle_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: left
      type(kim_model_handle_type), intent(in) :: right
    end function kim_model_handle_not_equal
  end interface operator (.ne.)

  interface kim_model_get_parameter
    subroutine kim_model_get_parameter_integer(model_handle, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      integer(c_int), intent(out) :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_integer

    subroutine kim_model_get_parameter_double(model_handle, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      real(c_double), intent(out) :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_double
  end interface kim_model_get_parameter

  interface kim_model_set_parameter
    subroutine kim_model_set_parameter_integer(model_handle, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      integer(c_int), intent(in), value :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_parameter_integer

    subroutine kim_model_set_parameter_double(model_handle, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      real(c_double), intent(in), value :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_parameter_double
  end interface kim_model_set_parameter

  interface
    subroutine kim_model_create(numbering, requested_length_unit, &
      requested_energy_unit, requested_charge_unit, &
      requested_temperature_unit, requested_time_unit, model_name, &
      requested_units_accepted, model_handle, ierr)
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      use kim_unit_system_module, only : kim_length_unit_type, &
        kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_handle_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
      type(kim_length_unit_type), intent(in), value :: requested_length_unit
      type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
      type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
      type(kim_temperature_unit_type), intent(in), value :: &
        requested_temperature_unit
      type(kim_time_unit_type), intent(in), value :: requested_time_unit
      character(len=*, kind=c_char), intent(in) :: model_name
      type(kim_model_handle_type), intent(out) :: model_handle
      integer(c_int), intent(out) :: requested_units_accepted
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_create

    subroutine kim_model_destroy(model_handle)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(inout) :: model_handle
    end subroutine kim_model_destroy

    subroutine kim_model_get_influence_distance(model_handle, &
      influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      real(c_double), intent(out) :: influence_distance
    end subroutine kim_model_get_influence_distance

    subroutine kim_model_get_number_of_neighbor_lists(model_handle, &
      number_of_neighbor_lists)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(out) :: number_of_neighbor_lists
    end subroutine kim_model_get_number_of_neighbor_lists

    subroutine kim_model_get_neighbor_list_values(model_handle, cutoffs, &
      model_will_not_request_neighbors_of_noncontributing_particles, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      real(c_double), intent(out) :: cutoffs(:)
      integer(c_int), intent(out) :: &
        model_will_not_request_neighbors_of_noncontributing_particles(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_neighbor_list_values

    subroutine kim_model_get_units(model_handle, length_unit, energy_unit, &
      charge_unit, temperature_unit, time_unit)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type, &
        kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_handle_type
      type(kim_model_handle_type), intent(in) :: model_handle
      type(kim_length_unit_type), intent(out) :: length_unit
      type(kim_energy_unit_type), intent(out) :: energy_unit
      type(kim_charge_unit_type), intent(out) :: charge_unit
      type(kim_temperature_unit_type), intent(out) :: temperature_unit
      type(kim_time_unit_type), intent(out) :: time_unit
    end subroutine kim_model_get_units

    subroutine kim_model_compute_arguments_create(model_handle, &
      compute_arguments_handle, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_arguments_module, only : &
        kim_compute_arguments_handle_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(kim_compute_arguments_handle_type), intent(out) :: &
        compute_arguments_handle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_create

    subroutine kim_model_compute_arguments_destroy(model_handle, &
      compute_arguments_handle, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_arguments_module, only: &
        kim_compute_arguments_handle_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(kim_compute_arguments_handle_type), intent(inout) :: &
        compute_arguments_handle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_destroy

    subroutine kim_model_compute(model_handle, compute_arguments_handle, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute

    subroutine kim_model_clear_then_refresh( &
      model_handle, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_clear_then_refresh

    subroutine kim_model_get_species_support_and_code(model_handle, &
      species_name, species_is_supported, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: species_is_supported
      integer(c_int), intent(out) :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_species_support_and_code

    subroutine kim_model_get_number_of_parameters(model_handle, &
      number_of_parameters)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine kim_model_get_number_of_parameters

    subroutine kim_model_get_parameter_data_type_extent_and_description( &
      model_handle, index, data_type, extent, description, ierr)
      use, intrinsic :: iso_c_binding
      use :: kim_data_type_module, only : kim_data_type_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      integer(c_int), intent(in), value :: index
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: extent
      character(len=*, kind=c_char), intent(out) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_data_type_extent_and_description

    subroutine kim_model_set_simulator_buffer_pointer(model_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_set_simulator_buffer_pointer

    subroutine kim_model_get_simulator_buffer_pointer(model_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_get_simulator_buffer_pointer

    subroutine kim_model_string(model_handle, string)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      character(len=*, kind=c_char), intent(out) :: string
    end subroutine kim_model_string

    subroutine kim_model_set_log_id(model_handle, log_id)
      use, intrinsic :: iso_c_binding
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      character(len=*, kind=c_char), intent(in) :: log_id
    end subroutine kim_model_set_log_id

    subroutine kim_model_push_log_verbosity(model_handle, log_verbosity)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
      type(kim_log_verbosity_type), intent(in) :: log_verbosity
    end subroutine kim_model_push_log_verbosity

    subroutine kim_model_pop_log_verbosity(model_handle)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_handle_type
      implicit none
      type(kim_model_handle_type), intent(in) :: model_handle
    end subroutine kim_model_pop_log_verbosity
  end interface
end module kim_model_module
