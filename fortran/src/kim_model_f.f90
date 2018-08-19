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


module kim_model_f_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_type, &
    create, &
    destroy, &
    get_influence_distance, &
    get_neighbor_list_pointers, &
    get_units, &
    compute_arguments_create, &
    compute_arguments_destroy, &
    compute, &
    clear_then_refresh, &
    get_species_support_and_code, &
    get_number_of_parameters, &
    get_parameter_data_type_extent_and_description, &
    get_parameter_integer, &
    get_parameter_double, &
    set_parameter_integer, &
    set_parameter_double, &
    set_simulator_buffer_pointer, &
    get_simulator_buffer_pointer, &
    model_string, &
    set_log_id, &
    push_log_verbosity, &
    pop_log_verbosity

  type, bind(c) :: kim_model_type
    private
    type(c_ptr) :: p
  end type kim_model_type

  interface
    integer(c_int) function create(numbering, requested_length_unit, &
      requested_energy_unit, requested_charge_unit, &
      requested_temperature_unit, requested_time_unit, model_name, &
      requested_units_accepted, model) bind(c, name="KIM_Model_Create")
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      use kim_unit_system_module, only : kim_length_unit_type, &
        kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
        kim_time_unit_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
      type(kim_length_unit_type), intent(in), value :: requested_length_unit
      type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
      type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
      type(kim_temperature_unit_type), intent(in), value :: &
        requested_temperature_unit
      type(kim_time_unit_type), intent(in), value :: requested_time_unit
      character(c_char), intent(in) :: model_name(*)
      integer(c_int), intent(out) :: requested_units_accepted
      type(c_ptr), intent(out) :: model
    end function create

    subroutine destroy(model) bind(c, name="KIM_Model_Destroy")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: model
    end subroutine destroy

    subroutine get_influence_distance(model, influence_distance) &
      bind(c, name="KIM_Model_GetInfluenceDistance")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      real(c_double), intent(out) :: influence_distance
    end subroutine get_influence_distance

    subroutine get_neighbor_list_pointers(model, number_of_neighbor_lists, &
      cutoffs_ptr, &
      model_will_not_request_neighbors_of_noncontributing__ptr) &
      bind(c, name="KIM_Model_GetNeighborListPointers")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_neighbor_lists
      type(c_ptr), intent(out) :: cutoffs_ptr
      type(c_ptr), intent(out) :: &
        model_will_not_request_neighbors_of_noncontributing__ptr
    end subroutine get_neighbor_list_pointers

    subroutine get_units(model, length_unit, energy_unit, charge_unit, &
      temperature_unit, time_unit) bind(c, name="KIM_Model_GetUnits")
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type, &
        kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_type
      type(kim_model_type), intent(in) :: model
      type(kim_length_unit_type), intent(out) :: length_unit
      type(kim_energy_unit_type), intent(out) :: energy_unit
      type(kim_charge_unit_type), intent(out) :: charge_unit
      type(kim_temperature_unit_type), intent(out) :: temperature_unit
      type(kim_time_unit_type), intent(out) :: time_unit
    end subroutine get_units

    integer(c_int) function compute_arguments_create(model, compute_arguments) &
      bind(c, name="KIM_Model_ComputeArgumentsCreate")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: compute_arguments
    end function compute_arguments_create

    integer(c_int) function compute_arguments_destroy(model, &
      compute_arguments) bind(c, name="KIM_Model_ComputeArgumentsDestroy")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(inout) :: compute_arguments
    end function compute_arguments_destroy

    integer(c_int) function compute(model, compute_arguments) &
      bind(c, name="KIM_Model_Compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_arguments_f_module, only : kim_compute_arguments_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
    end function compute

    integer(c_int) function clear_then_refresh(&
      model) &
      bind(c, &
      name="KIM_Model_ClearThenRefresh")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function clear_then_refresh

    integer(c_int) function get_species_support_and_code(model, species_name, &
      species_is_supported, code) &
      bind(c, name="KIM_Model_GetSpeciesSupportAndCode")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(out) :: species_is_supported
      integer(c_int), intent(out) :: code
    end function get_species_support_and_code

    subroutine get_number_of_parameters(model, number_of_parameters) &
      bind(c, name="KIM_Model_GetNumberOfParameters")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_parameters
    end subroutine get_number_of_parameters

    integer(c_int) function get_parameter_data_type_extent_and_description( &
      model, parameter_index, data_type, extent, description) &
      bind(c, name="KIM_Model_GetParameterDataTypeExtentAndDescription")
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : kim_data_type_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: parameter_index
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: extent
      type(c_ptr), intent(out) :: description
    end function get_parameter_data_type_extent_and_description

    integer(c_int) function get_parameter_integer(model, &
      parameter_index, array_index, parameter_value) &
      bind(c, name="KIM_Model_GetParameterInteger")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      integer(c_int), intent(out) :: parameter_value
    end function get_parameter_integer

    integer(c_int) function get_parameter_double(model, &
      parameter_index, array_index, parameter_value) &
      bind(c, name="KIM_Model_GetParameterDouble")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      real(c_double), intent(out) :: parameter_value
    end function get_parameter_double

    integer(c_int) function set_parameter_integer(model, &
      parameter_index, array_index, parameter_value) &
      bind(c, name="KIM_Model_SetParameterInteger")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      integer(c_int), intent(in), value :: parameter_value
    end function set_parameter_integer

    integer(c_int) function set_parameter_double(model, &
      parameter_index, array_index, parameter_value) &
      bind(c, name="KIM_Model_SetParameterDouble")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      integer(c_int), intent(in), value :: parameter_index
      integer(c_int), intent(in), value :: array_index
      real(c_double), intent(in), value :: parameter_value
    end function set_parameter_double

    subroutine set_simulator_buffer_pointer(model, ptr) &
      bind(c, name="KIM_Model_SetSimulatorBufferPointer")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_simulator_buffer_pointer

    subroutine get_simulator_buffer_pointer(model, ptr) &
      bind(c, name="KIM_Model_GetSimulatorBufferPointer")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(c_ptr), intent(out) :: ptr
    end subroutine get_simulator_buffer_pointer

    type(c_ptr) function model_string(model) &
      bind(c, name="KIM_Model_String")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end function model_string

    subroutine set_log_id(model, log_id) &
      bind(c, name="KIM_Model_SetLogID")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      character(c_char), intent(in) :: log_id(*)
    end subroutine set_log_id

    subroutine push_log_verbosity(model, log_verbosity) &
      bind(c, name="KIM_Model_PushLogVerbosity")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
    end subroutine push_log_verbosity

    subroutine pop_log_verbosity(model) &
      bind(c, name="KIM_Model_PopLogVerbosity")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end subroutine pop_log_verbosity
  end interface
end module kim_model_f_module


! free functions to implement kim_model_module

logical function kim_model_handle_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  implicit none
  type(kim_model_handle_type), intent(in) :: left
  type(kim_model_handle_type), intent(in) :: right

  if ((.not. c_associated(left%p)) .and. (.not. c_associated(right%p))) then
    kim_model_handle_equal = .true.
  else
    kim_model_handle_equal = c_associated(left%p, right%p)
  end if
end function kim_model_handle_equal

logical function kim_model_handle_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_module, only : operator (.eq.)
  implicit none
  type(kim_model_handle_type), intent(in) :: left
  type(kim_model_handle_type), intent(in) :: right

  kim_model_handle_not_equal = .not. (left .eq. right)
end function kim_model_handle_not_equal

subroutine kim_model_create(numbering, requested_length_unit, &
  requested_energy_unit, requested_charge_unit, &
  requested_temperature_unit, requested_time_unit, model_name, &
  requested_units_accepted, model_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  use kim_unit_system_module, only : kim_length_unit_type, &
    kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
    kim_time_unit_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : create
  implicit none
  type(kim_numbering_type), intent(in), value :: numbering
  type(kim_length_unit_type), intent(in), value :: requested_length_unit
  type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
  type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
  type(kim_temperature_unit_type), intent(in), value :: &
    requested_temperature_unit
  type(kim_time_unit_type), intent(in), value :: requested_time_unit
  character(len=*, kind=c_char), intent(in) :: model_name
  integer(c_int), intent(out) :: requested_units_accepted
  type(kim_model_handle_type), intent(out) :: model_handle
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: pmodel

  ierr = create(numbering, requested_length_unit, requested_energy_unit, &
    requested_charge_unit, requested_temperature_unit, requested_time_unit, &
    trim(model_name)//c_null_char, requested_units_accepted, pmodel)
  model_handle%p = pmodel
end subroutine kim_model_create

subroutine kim_model_destroy(model_handle)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : destroy
  implicit none
  type(kim_model_handle_type), intent(inout) :: model_handle

  type(c_ptr) :: pmodel
  pmodel = model_handle%p
  call destroy(pmodel)
  model_handle%p = c_null_ptr
end subroutine kim_model_destroy

subroutine kim_model_get_influence_distance(model_handle, influence_distance)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, get_influence_distance
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  real(c_double), intent(out) :: influence_distance
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call get_influence_distance(model, influence_distance)
end subroutine kim_model_get_influence_distance

subroutine kim_model_get_number_of_neighbor_lists(model_handle, &
  number_of_neighbor_lists)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, &
    get_neighbor_list_pointers
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(out) :: number_of_neighbor_lists
  type(kim_model_type), pointer :: model

  type(c_ptr) cutoffs_ptr, hint_ptr

  call c_f_pointer(model_handle%p, model)
  call get_neighbor_list_pointers(model, number_of_neighbor_lists, &
    cutoffs_ptr, hint_ptr)
end subroutine kim_model_get_number_of_neighbor_lists

subroutine kim_model_get_neighbor_list_values(model_handle, cutoffs, &
  model_will_not_request_neighbors_of_noncontributing_particles, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, &
    get_neighbor_list_pointers
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  real(c_double), intent(out) :: cutoffs(:)
  integer(c_int), intent(out) :: &
    model_will_not_request_neighbors_of_noncontributing_particles(:)
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  integer(c_int) number_of_neighbor_lists
  real(c_double), pointer :: cutoffs_fpointer(:)
  integer(c_int), pointer :: &
    model_will_not_request_neighbors_of_noncontributing__fpointer(:)
  type(c_ptr) cutoffs_ptr
  type(c_ptr) model_will_not_request_neighbors_of_noncontributing__ptr

  call c_f_pointer(model_handle%p, model)
  call get_neighbor_list_pointers(model, number_of_neighbor_lists, &
    cutoffs_ptr, &
    model_will_not_request_neighbors_of_noncontributing__ptr)
  if (c_associated(cutoffs_ptr)) then
    call c_f_pointer(cutoffs_ptr, cutoffs_fpointer, [number_of_neighbor_lists])
  else
    nullify(cutoffs_fpointer)
  end if
  if (size(cutoffs) < number_of_neighbor_lists) then
    ierr = 1
  else
    ierr = 0
    cutoffs = cutoffs_fpointer(1:number_of_neighbor_lists)
  end if

  if (c_associated( &
    model_will_not_request_neighbors_of_noncontributing__ptr)) then
    call c_f_pointer( &
      model_will_not_request_neighbors_of_noncontributing__ptr, &
      model_will_not_request_neighbors_of_noncontributing__fpointer, &
      [number_of_neighbor_lists])
  else
    nullify( &
      model_will_not_request_neighbors_of_noncontributing__fpointer)
  end if
  if (size( &
    model_will_not_request_neighbors_of_noncontributing_particles) &
    < number_of_neighbor_lists) then
    ierr = 1
  else
    ierr = 0
    model_will_not_request_neighbors_of_noncontributing_particles = &
      model_will_not_request_neighbors_of_noncontributing__fpointer( &
      1:number_of_neighbor_lists)
  end if
end subroutine kim_model_get_neighbor_list_values

subroutine kim_model_get_units(model_handle, length_unit, energy_unit, &
  charge_unit, temperature_unit, time_unit)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_unit_system_module, only : kim_length_unit_type, &
    kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
    kim_time_unit_type
  use kim_model_f_module, only : kim_model_type, get_units
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_length_unit_type), intent(out) :: length_unit
  type(kim_energy_unit_type), intent(out) :: energy_unit
  type(kim_charge_unit_type), intent(out) :: charge_unit
  type(kim_temperature_unit_type), intent(out) :: temperature_unit
  type(kim_time_unit_type), intent(out) :: time_unit
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call get_units(model, length_unit, energy_unit, charge_unit, &
    temperature_unit, time_unit)
end subroutine kim_model_get_units

subroutine kim_model_compute_arguments_create(model_handle, &
  compute_arguments_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_compute_arguments_module, only : &
    kim_compute_arguments_handle_type
  use kim_model_f_module, only : kim_model_type, compute_arguments_create
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_compute_arguments_handle_type), intent(out) :: &
    compute_arguments_handle
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model
  type(c_ptr) :: pcompute_arguments

  call c_f_pointer(model_handle%p, model)

  ierr = compute_arguments_create(model, pcompute_arguments)
  if (ierr == 0) then
    compute_arguments_handle%p = pcompute_arguments
  end if
end subroutine kim_model_compute_arguments_create

subroutine kim_model_compute_arguments_destroy(model_handle, &
  compute_arguments_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_compute_arguments_module, only : &
    kim_compute_arguments_handle_type
  use kim_model_f_module, only : kim_model_type, compute_arguments_destroy
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_compute_arguments_handle_type), intent(inout) :: &
    compute_arguments_handle
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model
  type(c_ptr) pcompute_arguments

  call c_f_pointer(model_handle%p, model)
  pcompute_arguments = compute_arguments_handle%p
  ierr = compute_arguments_destroy(model, pcompute_arguments)
  if (ierr /= 0) then
    compute_arguments_handle%p = c_null_ptr
  end if
end subroutine kim_model_compute_arguments_destroy

subroutine kim_model_compute(model_handle, compute_arguments_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_model_f_module, only : kim_model_type, compute
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(model_handle%p, model)
  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = compute(model, compute_arguments)
end subroutine kim_model_compute

subroutine kim_model_clear_then_refresh(model_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, clear_then_refresh
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = clear_then_refresh(model)
end subroutine kim_model_clear_then_refresh

subroutine kim_model_get_species_support_and_code(model_handle, species_name, &
  species_is_supported, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, get_species_support_and_code
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(out) :: species_is_supported
  integer(c_int), intent(out) :: code
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = get_species_support_and_code(model, species_name, &
    species_is_supported, code)
end subroutine kim_model_get_species_support_and_code

subroutine kim_model_get_number_of_parameters(model_handle, &
  number_of_parameters)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, get_number_of_parameters
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(out) :: number_of_parameters
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call get_number_of_parameters(model, number_of_parameters)
end subroutine kim_model_get_number_of_parameters

subroutine kim_model_get_parameter_data_type_extent_and_description( &
  model_handle, parameter_index, data_type, extent, description, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_data_type_module, only : kim_data_type_type
  use kim_model_f_module, only : kim_model_type, &
    get_parameter_data_type_extent_and_description
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(in), value :: parameter_index
  type(kim_data_type_type), intent(out) :: data_type
  integer(c_int), intent(out) :: extent
  character(len=*, kind=c_char), intent(out) :: description
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  type(c_ptr) :: p

  call c_f_pointer(model_handle%p, model)
  ierr = get_parameter_data_type_extent_and_description(model, &
    parameter_index-1, data_type, extent, p)
  if (c_associated(p)) then
    call kim_convert_string(p, description)
  else
    description = ""
  end if
end subroutine kim_model_get_parameter_data_type_extent_and_description

subroutine kim_model_get_parameter_integer(model_handle, parameter_index, &
  array_index, parameter_value, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, get_parameter_integer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(in), value :: parameter_index
  integer(c_int), intent(in), value :: array_index
  integer(c_int), intent(out) :: parameter_value
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = get_parameter_integer(model, parameter_index-1, array_index-1, &
    parameter_value)
end subroutine kim_model_get_parameter_integer

subroutine kim_model_get_parameter_double(model_handle, parameter_index, &
  array_index, parameter_value, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, get_parameter_double
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(in), value :: parameter_index
  integer(c_int), intent(in), value :: array_index
  real(c_double), intent(out) :: parameter_value
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = get_parameter_double(model, parameter_index-1, array_index-1, &
    parameter_value)
end subroutine kim_model_get_parameter_double

subroutine kim_model_set_parameter_integer(model_handle, parameter_index, &
  array_index, parameter_value, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_parameter_integer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(in), value :: parameter_index
  integer(c_int), intent(in), value :: array_index
  integer(c_int), intent(in), value :: parameter_value
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = set_parameter_integer(model, parameter_index-1, array_index-1, &
    parameter_value)
end subroutine kim_model_set_parameter_integer

subroutine kim_model_set_parameter_double(model_handle, parameter_index, &
  array_index, parameter_value, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_parameter_double
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(in), value :: parameter_index
  integer(c_int), intent(in), value :: array_index
  real(c_double), intent(in), value :: parameter_value
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = set_parameter_double(model, parameter_index-1, array_index-1, &
    parameter_value)
end subroutine kim_model_set_parameter_double

subroutine kim_model_set_simulator_buffer_pointer(model_handle, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_simulator_buffer_pointer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(c_ptr), intent(in), value :: ptr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set_simulator_buffer_pointer(model, ptr)
end subroutine kim_model_set_simulator_buffer_pointer

subroutine kim_model_get_simulator_buffer_pointer(model_handle, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, get_simulator_buffer_pointer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(c_ptr), intent(out) :: ptr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call get_simulator_buffer_pointer(model, ptr)
end subroutine kim_model_get_simulator_buffer_pointer

subroutine kim_model_string(model_handle, string)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, model_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  character(len=*, kind=c_char), intent(out) :: string
  type(kim_model_type), pointer :: model

  type(c_ptr) :: p

  call c_f_pointer(model_handle%p, model)
  p = model_string(model)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_model_string

subroutine kim_model_set_log_id(model_handle, log_id)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_log_id
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  character(len=*, kind=c_char), intent(in) :: log_id
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set_log_id(model, trim(log_id)//c_null_char)
end subroutine kim_model_set_log_id

subroutine kim_model_push_log_verbosity(model_handle, log_verbosity)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_model_f_module, only : kim_model_type, push_log_verbosity
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_log_verbosity_type), intent(in) :: log_verbosity
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call push_log_verbosity(model, log_verbosity)
end subroutine kim_model_push_log_verbosity

subroutine kim_model_pop_log_verbosity(model_handle)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_model_f_module, only : kim_model_type, pop_log_verbosity
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call pop_log_verbosity(model)
end subroutine kim_model_pop_log_verbosity
