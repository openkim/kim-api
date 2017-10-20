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
  use :: kim_language_name_module
  implicit none
  private

  public &
    kim_model_type, &
    kim_model_create, &
    kim_model_destroy, &
    kim_model_get_influence_distance, &
    kim_model_get_number_of_neighbor_list_cutoffs, &
    kim_model_get_neighbor_list_cutoffs, &
    kim_model_get_argument_support_status, &
    kim_model_get_callback_support_status, &
    kim_model_get_units, &
    kim_model_set_argument_pointer, &
    kim_model_set_callback_pointer, &
    kim_model_compute, &
    kim_model_clear_influence_dist_and_cutoffs_then_refresh_model, &
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

  type, bind(c) :: kim_model_type
    private
    type(c_ptr) :: p
  end type kim_model_type

  interface kim_model_get_parameter
    subroutine kim_model_get_parameter_integer(model, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      integer(c_int), intent(out) :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_integer

    subroutine kim_model_get_parameter_double(model, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      real(c_double), intent(out) :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_double
  end interface kim_model_get_parameter

  interface kim_model_set_parameter
    subroutine kim_model_set_parameter_integer(model, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      integer(c_int), intent(in), value :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_parameter_integer

    subroutine kim_model_set_parameter_double(model, parameter_index, &
      array_index, parameter_value, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      real(c_double), intent(in), value :: parameter_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_parameter_double
  end interface kim_model_set_parameter

  interface kim_model_set_argument_pointer
    subroutine kim_model_set_argument_pointer_int0(model, argument_name, int0, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), target :: int0
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_argument_pointer_int0

    subroutine kim_model_set_argument_pointer_int1(model, argument_name, int1, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), target :: int1(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_argument_pointer_int1

    subroutine kim_model_set_argument_pointer_int2(model, argument_name, int2, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), target :: int2(:,:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_argument_pointer_int2

    subroutine kim_model_set_argument_pointer_double0(model, argument_name, &
      double0, ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : &
        kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      real(c_double), intent(in), target :: double0
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_argument_pointer_double0

    subroutine kim_model_set_argument_pointer_double1(model, argument_name, &
      double1, ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      real(c_double), intent(in), target :: double1(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_argument_pointer_double1

    subroutine kim_model_set_argument_pointer_double2(model, argument_name, &
      double2, ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      real(c_double), intent(in), target :: double2(:,:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_argument_pointer_double2
  end interface kim_model_set_argument_pointer

  interface
    subroutine kim_model_create(numbering, requested_length_unit, &
      requested_energy_unit, requested_charge_unit, &
      requested_temperature_unit, requested_time_unit, model_name, &
      requested_units_accepted, model, ierr)
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      use kim_unit_system_module, only : kim_length_unit_type, &
        kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
      type(kim_length_unit_type), intent(in), value :: requested_length_unit
      type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
      type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
      type(kim_temperature_unit_type), intent(in), value :: &
        requested_temperature_unit
      type(kim_time_unit_type), intent(in), value :: requested_time_unit
      character(len=*), intent(in) :: model_name
      type(kim_model_type), intent(out), pointer :: model
      integer(c_int), intent(out) :: requested_units_accepted
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_create

    subroutine kim_model_destroy(model)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout), pointer :: model
    end subroutine kim_model_destroy

    subroutine kim_model_get_influence_distance(model, influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(out) :: influence_distance
    end subroutine kim_model_get_influence_distance

    subroutine kim_model_get_number_of_neighbor_list_cutoffs(model, &
      number_of_cutoffs)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_cutoffs
    end subroutine kim_model_get_number_of_neighbor_list_cutoffs

    subroutine kim_model_get_neighbor_list_cutoffs(model, cutoffs, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(out) :: cutoffs(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_neighbor_list_cutoffs

    subroutine kim_model_get_argument_support_status(model, argument_name, &
      support_status, ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_support_status_type), intent(out) :: support_status
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_argument_support_status

    subroutine kim_model_get_callback_support_status(model, callback_name, &
      support_status, ierr)
      use, intrinsic :: iso_c_binding
      use kim_callback_name_module, only : kim_callback_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_callback_name_type), intent(in), value :: callback_name
      type(kim_support_status_type), intent(out) :: support_status
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_callback_support_status

    subroutine kim_model_set_callback_pointer(model, callback_name, &
      language_name, fptr, data_object, ierr)
      use, intrinsic :: iso_c_binding
      use kim_callback_name_module, only : kim_callback_name_type
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_callback_name_type), intent(in), value :: callback_name
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      type(c_ptr), intent(in), value :: data_object
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_set_callback_pointer

    subroutine kim_model_get_units(model, length_unit, energy_unit, &
      charge_unit, temperature_unit, time_unit)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type, &
        kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_type
      type(kim_length_unit_type), intent(out) :: length_unit
      type(kim_energy_unit_type), intent(out) :: energy_unit
      type(kim_charge_unit_type), intent(out) :: charge_unit
      type(kim_temperature_unit_type), intent(out) :: temperature_unit
      type(kim_time_unit_type), intent(out) :: time_unit
    end subroutine kim_model_get_units

    subroutine kim_model_compute(model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute

    subroutine kim_model_clear_influence_dist_and_cutoffs_then_refresh_model( &
      model, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_clear_influence_dist_and_cutoffs_then_refresh_model

    subroutine kim_model_get_species_support_and_code(model, species_name, &
      species_is_supported, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: species_is_supported
      integer(c_int), intent(out) :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_species_support_and_code

    subroutine kim_model_get_number_of_parameters(model, number_of_parameters)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine kim_model_get_number_of_parameters

    subroutine kim_model_get_parameter_data_type_extent_and_description(model, &
      index, data_type, extent, description, ierr)
      use, intrinsic :: iso_c_binding
      use :: kim_data_type_module, only : kim_data_type_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: index
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: extent
      character(len=*), intent(out) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_get_parameter_data_type_extent_and_description

    subroutine kim_model_set_simulator_buffer_pointer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_set_simulator_buffer_pointer

    subroutine kim_model_get_simulator_buffer_pointer(model, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_get_simulator_buffer_pointer

    subroutine kim_model_string(model, string)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      character(len=*), intent(out) :: string
    end subroutine kim_model_string

    subroutine kim_model_set_log_id(model, log_id)
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      character(len=*), intent(in) :: log_id
    end subroutine kim_model_set_log_id

    subroutine kim_model_push_log_verbosity(model, log_verbosity)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_log_verbosity_type), intent(in) :: log_verbosity
    end subroutine kim_model_push_log_verbosity

    subroutine kim_model_pop_log_verbosity(model)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end subroutine kim_model_pop_log_verbosity
  end interface
end module kim_model_module