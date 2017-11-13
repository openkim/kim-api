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
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_type, &
    create, &
    destroy, &
    get_influence_distance, &
    get_neighbor_list_cutoffs_pointer, &
    get_argument_support_status, &
    get_callback_support_status, &
    get_units, &
    set_argument_pointer_integer, &
    set_argument_pointer_double, &
    set_callback_pointer, &
    compute, &
    clear_and_refresh_model, &
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

    subroutine get_neighbor_list_cutoffs_pointer(model, number_of_cutoffs, &
      cutoffs_ptr) bind(c, name="KIM_Model_GetNeighborListCutoffsPointer")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      integer(c_int), intent(out) :: number_of_cutoffs
      type(c_ptr), intent(out) :: cutoffs_ptr
    end subroutine get_neighbor_list_cutoffs_pointer

    integer(c_int) function get_argument_support_status(model, argument_name, &
      support_status) bind(c, name="KIM_Model_GetArgumentSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_support_status_type), intent(out) :: support_status
    end function get_argument_support_status

    integer(c_int) function get_callback_support_status(model, callback_name, &
      support_status) bind(c, name="KIM_Model_GetCallbackSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_callback_name_module, only : kim_callback_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
      type(kim_callback_name_type), intent(in), value :: callback_name
      type(kim_support_status_type), intent(out) :: support_status
    end function get_callback_support_status

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

    integer(c_int) function set_argument_pointer_integer(model, argument_name, &
      ptr) bind(c, name="KIM_Model_SetArgumentPointerInteger")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(in), value :: ptr
    end function set_argument_pointer_integer

    integer(c_int) function set_argument_pointer_double(model, argument_name, &
      ptr) bind(c, name="KIM_Model_SetArgumentPointerDouble")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(in), value :: ptr
    end function set_argument_pointer_double

    integer(c_int) function set_callback_pointer(model, callback_name, &
      language_name, fptr, data_object) &
      bind(c, name="KIM_Model_SetCallbackPointer")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_callback_name_module, only : kim_callback_name_type
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
      type(kim_language_name_type), intent(in), value :: language_name
      type(kim_callback_name_type), intent(in), value :: callback_name
      type(c_funptr), intent(in), value :: fptr
      type(c_ptr), intent(in), value :: data_object
    end function set_callback_pointer

    integer(c_int) function compute(model) bind(c, name="KIM_Model_Compute")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(in) :: model
    end function compute

    integer(c_int) function clear_and_refresh_model(&
      model) &
      bind(c, &
      name="KIM_Model_ClearInfluenceDistanceAndCutoffsThenRefreshModel")
      use, intrinsic :: iso_c_binding
      import kim_model_type
      implicit none
      type(kim_model_type), intent(inout) :: model
    end function clear_and_refresh_model

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
      type(kim_log_verbosity_type), intent(in) :: log_verbosity
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
  character(len=*), intent(in) :: model_name
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

subroutine kim_model_get_number_of_neighbor_list_cutoffs(model_handle, &
  number_of_cutoffs)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, &
    get_neighbor_list_cutoffs_pointer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(out) :: number_of_cutoffs
  type(kim_model_type), pointer :: model

  type(c_ptr) cutoffs_ptr

  call c_f_pointer(model_handle%p, model)
  call get_neighbor_list_cutoffs_pointer(model, number_of_cutoffs, cutoffs_ptr)
end subroutine kim_model_get_number_of_neighbor_list_cutoffs

subroutine kim_model_get_neighbor_list_cutoffs(model_handle, cutoffs, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, &
    get_neighbor_list_cutoffs_pointer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  real(c_double), intent(out) :: cutoffs(:)
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  integer(c_int) number_of_cutoffs
  real(c_double), pointer :: cutoffs_fpointer(:)
  type(c_ptr) cutoffs_ptr

  call c_f_pointer(model_handle%p, model)
  call get_neighbor_list_cutoffs_pointer(model, number_of_cutoffs, cutoffs_ptr)
  call c_f_pointer(cutoffs_ptr, cutoffs_fpointer, [number_of_cutoffs])

  if (size(cutoffs) < number_of_cutoffs) then
    ierr = 1
  else
    ierr = 0
    cutoffs = cutoffs_fpointer(1:number_of_cutoffs)
  end if
end subroutine kim_model_get_neighbor_list_cutoffs

subroutine kim_model_get_argument_support_status(model_handle, argument_name, &
  support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_support_status_module, only : kim_support_status_type
  use kim_model_f_module, only : kim_model_type, get_argument_support_status
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  type(kim_support_status_type), intent(out) :: support_status
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = get_argument_support_status(model, argument_name, support_status)
end subroutine kim_model_get_argument_support_status

subroutine kim_model_get_callback_support_status(model_handle, callback_name, &
  support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_callback_name_module, only : kim_callback_name_type
  use kim_support_status_module, only : kim_support_status_type
  use kim_model_f_module, only : kim_model_type, get_callback_support_status
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_callback_name_type), intent(in), value :: callback_name
  type(kim_support_status_type), intent(out) :: support_status
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = get_callback_support_status(model, callback_name, support_status)
end subroutine kim_model_get_callback_support_status

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

subroutine kim_model_set_argument_pointer_int0(model_handle, argument_name, &
  int0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_argument_pointer_integer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), target :: int0
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = set_argument_pointer_integer(model, argument_name, c_loc(int0))
end subroutine kim_model_set_argument_pointer_int0

subroutine kim_model_set_argument_pointer_int1(model_handle, argument_name, &
  int1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_argument_pointer_integer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), target :: int1(:)
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set(model, argument_name, size(int1,1,c_int), int1, ierr)
  return

contains
  subroutine set(model, argument_name, extent1, int1, ierr)
    use, intrinsic :: iso_c_binding
    use kim_argument_name_module, only : kim_argument_name_type
    use kim_model_f_module, only : kim_model_type
    implicit none
    type(kim_model_type), intent(inout) :: model
    type(kim_argument_name_type), intent(in), value :: argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in), target :: int1(extent1)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_integer(model, argument_name, c_loc(int1))
  end subroutine set
end subroutine kim_model_set_argument_pointer_int1

subroutine kim_model_set_argument_pointer_int2(model_handle, argument_name, &
  int2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), target :: int2(:,:)
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set(model, argument_name, size(int2, 1, c_int), size(int2, 2, c_int), &
    int2, ierr)
  return

contains
  subroutine set(model, argument_name, extent1, extent2, int2, ierr)
    use, intrinsic :: iso_c_binding
    use kim_argument_name_module, only : kim_argument_name_type
    use kim_model_f_module, only : kim_model_type, set_argument_pointer_integer
    implicit none
    type(kim_model_type), intent(inout) :: model
    type(kim_argument_name_type), intent(in), value :: argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in) :: extent2
    integer(c_int), intent(in), target :: int2(extent1,extent2)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_integer(model, argument_name, c_loc(int2))
  end subroutine set
end subroutine kim_model_set_argument_pointer_int2

subroutine kim_model_set_argument_pointer_double0(model_handle, argument_name, &
  double0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_argument_pointer_double
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  real(c_double), intent(in), target :: double0
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = set_argument_pointer_double(model, argument_name, c_loc(double0))
end subroutine kim_model_set_argument_pointer_double0

subroutine kim_model_set_argument_pointer_double1(model_handle, argument_name, &
  double1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  real(c_double), intent(in), target :: double1(:)
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set(model, argument_name, size(double1, 1, c_int), double1, ierr)
  return

contains
  subroutine set(model, argument_name, extent1, double1, ierr)
    use, intrinsic :: iso_c_binding
    use kim_argument_name_module, only : kim_argument_name_type
    use kim_model_f_module, only : kim_model_type, set_argument_pointer_double
    implicit none
    type(kim_model_type), intent(inout) :: model
    type(kim_argument_name_type), intent(in), value :: argument_name
    integer(c_int), intent(in) :: extent1
    real(c_double), intent(in), target :: double1(extent1)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_double(model, argument_name, c_loc(double1))
  end subroutine set
end subroutine kim_model_set_argument_pointer_double1

subroutine kim_model_set_argument_pointer_double2(model_handle, argument_name, &
  double2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_argument_name_type), intent(in), value :: argument_name
  real(c_double), intent(in), target :: double2(:,:)
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set(model, argument_name, size(double2, 1, c_int), &
    size(double2, 2, c_int), double2, ierr)
  return

contains
  subroutine set(model, argument_name, extent1, extent2, double2, ierr)
    use, intrinsic :: iso_c_binding
    use kim_argument_name_module, only : kim_argument_name_type
    use kim_model_f_module, only : kim_model_type, set_argument_pointer_double
    implicit none
    type(kim_model_type), intent(inout) :: model
    type(kim_argument_name_type), intent(in), value :: argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in) :: extent2
    real(c_double), intent(in), target :: double2(extent1,extent2)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_double(model, argument_name, c_loc(double2))
  end subroutine set
end subroutine kim_model_set_argument_pointer_double2

subroutine kim_model_set_callback_pointer(model_handle, callback_name, &
  language_name, fptr, data_object, ierr)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_module, only : kim_callback_name_type
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_callback_pointer
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  type(kim_callback_name_type), intent(in), value :: callback_name
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  type(c_ptr), intent(in), value :: data_object
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = set_callback_pointer(model, callback_name, language_name, fptr, &
    data_object)
end subroutine kim_model_set_callback_pointer

subroutine kim_model_compute(model_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, compute
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = compute(model)
end subroutine kim_model_compute

subroutine &
  kim_model_clear_influence_dist_and_cutoffs_then_refresh_model(model_handle, &
  ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, clear_and_refresh_model
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  ierr = clear_and_refresh_model(model)
end subroutine kim_model_clear_influence_dist_and_cutoffs_then_refresh_model

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
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  integer(c_int), intent(in), value :: parameter_index
  type(kim_data_type_type), intent(out) :: data_type
  integer(c_int), intent(out) :: extent
  character(len=*), intent(out) :: description
  integer(c_int), intent(out) :: ierr
  type(kim_model_type), pointer :: model

  type(c_ptr) :: p
  character(len=len(description)+1), pointer :: fp
  integer(c_int) :: null_index

  call c_f_pointer(model_handle%p, model)
  ierr = get_parameter_data_type_extent_and_description(model, &
    parameter_index-1, data_type, extent, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  description = fp(1:null_index)
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
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  character(len=*), intent(out) :: string
  type(kim_model_type), pointer :: model

  type(c_ptr) :: p
  character(len=len(string)+1), pointer :: fp
  integer(c_int) :: null_index

  call c_f_pointer(model_handle%p, model)
  p = model_string(model)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_model_string

subroutine kim_model_set_log_id(model_handle, log_id)
  use, intrinsic :: iso_c_binding
  use kim_model_module, only : kim_model_handle_type
  use kim_model_f_module, only : kim_model_type, set_log_id
  implicit none
  type(kim_model_handle_type), intent(in) :: model_handle
  character(len=*), intent(in) :: log_id
  type(kim_model_type), pointer :: model

  call c_f_pointer(model_handle%p, model)
  call set_log_id(model, log_id)
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
