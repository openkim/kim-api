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
! Copyright (c) 2016--2019, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api-2.1.0 package.
!


!> \brief \copybrief KIM::Model
!!
!! \sa KIM::Model, KIM_Model
!!
!! \since 2.0
module kim_model_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_handle_type, &

    ! Constants
    KIM_MODEL_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_model_create, &
    kim_model_destroy, &
    kim_is_routine_present, &
    kim_get_influence_distance, &
    kim_get_number_of_neighbor_lists, &
    kim_get_neighbor_list_values, &
    kim_get_units, &
    kim_compute_arguments_create, &
    kim_compute_arguments_destroy, &
    kim_compute, &
    kim_extension, &
    kim_clear_then_refresh, &
    kim_write_parameterized_model, &
    kim_get_species_support_and_code, &
    kim_get_number_of_parameters, &
    kim_get_parameter_metadata, &
    kim_get_parameter, &
    kim_set_parameter, &
    kim_set_simulator_buffer_pointer, &
    kim_get_simulator_buffer_pointer, &
    kim_to_string, &
    kim_set_log_id, &
    kim_push_log_verbosity, &
    kim_pop_log_verbosity


  !> \brief \copybrief KIM::Model
  !!
  !! \sa KIM::Model, KIM_Model
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_handle_type), protected, save &
    :: KIM_MODEL_NULL_HANDLE

  !> \brief Compares kim_model_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator (.eq.)
    module procedure kim_model_handle_equal
  end interface operator (.eq.)

  !> \brief Compares kim_model_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_model_handle_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::Model::IsRoutinePresent
  !!
  !! \sa KIM::Model::IsRoutinePresent, KIM_Model_IsRoutinePresent
  !!
  !! \since 2.0
  interface kim_is_routine_present
    module procedure kim_model_is_routine_present
  end interface kim_is_routine_present

  !> \brief \copybrief KIM::Model::GetInfluenceDistance
  !!
  !! \sa KIM::Model::GetInfluenceDistance, KIM_Model_GetInfluenceDistance
  !!
  !! \since 2.0
  interface kim_get_influence_distance
    module procedure kim_model_get_influence_distance
  end interface kim_get_influence_distance

  !> \brief Get Model's number of neighbor lists.
  !!
  !! \sa KIM::Model::GetNeighborListPointers, KIM_Model_GetNeighborListPointers
  !!
  !! \since 2.0
  interface kim_get_number_of_neighbor_lists
    module procedure kim_model_get_number_of_neighbor_lists
  end interface kim_get_number_of_neighbor_lists

  !> \brief Get Model's neighbor list values
  !!
  !! \sa KIM::Model::GetNeighborListPointers, KIM_Model_GetNeighborListPointers
  !!
  !! \since 2.0
  interface kim_get_neighbor_list_values
    module procedure kim_model_get_neighbor_list_values
  end interface kim_get_neighbor_list_values

  !> \brief \copybrief KIM::Model::GetUnits
  !!
  !! \sa KIM::Model::GetUnits, KIM_Model_GetUnits
  !!
  !! \since 2.0
  interface kim_get_units
    module procedure kim_model_get_units
  end interface kim_get_units

  !> \brief \copybrief KIM::Model::ComputeArgumentsCreate
  !!
  !! \sa KIM::Model::ComputeArgumentsCreate, KIM_Model_ComputeArgumentsCreate
  !!
  !! \since 2.0
  interface kim_compute_arguments_create
    module procedure kim_model_compute_arguments_create
  end interface kim_compute_arguments_create

  !> \brief \copybrief KIM::Model::ComputeArgumentsDestroy
  !!
  !! \sa KIM::Model::ComputeArgumentsDestroy, KIM_Model_ComputeArgumentsDestroy
  !!
  !! \since 2.0
  interface kim_compute_arguments_destroy
    module procedure kim_model_compute_arguments_destroy
  end interface kim_compute_arguments_destroy

  !> \brief \copybrief KIM::Model::Compute
  !!
  !! \sa KIM::Model::Compute, KIM_Model_Compute
  !!
  !! \since 2.0
  interface kim_compute
    module procedure kim_model_compute
  end interface kim_compute

  !> \brief \copybrief KIM::Model::Extension
  !!
  !! \sa KIM::Model::Extension, KIM_Model_Extension
  !!
  !! \since 2.0
  interface kim_extension
    module procedure kim_model_extension
  end interface kim_extension

  !> \brief \copybrief KIM::Model::ClearThenRefresh
  !!
  !! \sa KIM::Model::ClearThenRefresh, KIM_Model_ClearThenRefresh
  !!
  !! \since 2.0
  interface kim_clear_then_refresh
    module procedure kim_model_clear_then_refresh
  end interface kim_clear_then_refresh

  !> \brief \copybrief KIM::Model::WriteParameterizedModel
  !!
  !! \sa KIM::Model::WriteParameterizedModel, KIM_Model_WriteParameterizedModel
  !!
  !! \since 2.0
  interface kim_write_parameterized_model
    module procedure kim_model_write_parameterized_model
  end interface kim_write_parameterized_model

  !> \brief \copybrief KIM::Model::GetSpeciesSupportAndCode
  !!
  !! \sa KIM::Model::GetSpeciesSupportAndCode,
  !! KIM_Model_GetSpeciesSupportAndCode
  !!
  !! \since 2.0
  interface kim_get_species_support_and_code
    module procedure kim_model_get_species_support_and_code
  end interface kim_get_species_support_and_code

  !> \brief \copybrief KIM::Model::GetNumberOfParameters
  !!
  !! \sa KIM::Model::GetNumberOfParameters, KIM_Model_GetNumberOfParameters
  !!
  !! \since 2.0
  interface kim_get_number_of_parameters
    module procedure kim_model_get_number_of_parameters
  end interface kim_get_number_of_parameters

  !> \brief \copybrief KIM::Model::GetParameterMetadata
  !!
  !! \sa KIM::Model::GetParameterMetadata, KIM_Model_GetParameterMetadata
  !!
  !! \since 2.0
  interface kim_get_parameter_metadata
    module procedure kim_model_get_parameter_metadata
  end interface kim_get_parameter_metadata

  !> \brief \copybrief KIM::Model::GetParameter
  !!
  !! \sa KIM::Model::GetParameter, KIM_Model_GetParameterInteger,
  !! KIM_Model_GetParameterDouble
  !!
  !! \since 2.0
  interface kim_get_parameter
    module procedure kim_model_get_parameter_integer
    module procedure kim_model_get_parameter_double
  end interface kim_get_parameter

  !> \brief \copybrief KIM::Model::SetParameter
  !!
  !! \sa KIM::Model::SetParameter, KIM_Model_SetParameterInteger,
  !! KIM_Model_SetParameterDouble
  !!
  !! \since 2.0
  interface kim_set_parameter
    module procedure kim_model_set_parameter_integer
    module procedure kim_model_set_parameter_double
  end interface kim_set_parameter

  !> \brief \copybrief KIM::Model::SetSimulatorBufferPointer
  !!
  !! \sa KIM::Model::SetSimulatorBufferPointer,
  !! KIM_Model_SetSimulatorBufferPointer
  !!
  !! \since 2.0
  interface kim_set_simulator_buffer_pointer
    module procedure kim_model_set_simulator_buffer_pointer
  end interface kim_set_simulator_buffer_pointer

  !> \brief \copybrief KIM::Model::GetSimulatorBufferPointer
  !!
  !! \sa KIM::Model::GetSimulatorBufferPointer,
  !! KIM_Model_GetSimulatorBufferPointer
  !!
  !! \since 2.0
  interface kim_get_simulator_buffer_pointer
    module procedure kim_model_get_simulator_buffer_pointer
  end interface kim_get_simulator_buffer_pointer

  !> \brief \copybrief KIM::Model::ToString
  !!
  !! \sa KIM::Model::ToString, KIM_Model_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_to_string
  end interface kim_to_string

  !> \brief \copybrief KIM::Model::SetLogID
  !!
  !! \sa KIM::Model::SetLogID, KIM_Model_SetLogID
  !!
  !! \since 2.0
  interface kim_set_log_id
    module procedure kim_model_set_log_id
  end interface kim_set_log_id

  !> \brief \copybrief KIM::Model::PushLogVerbosity
  !!
  !! \sa KIM::Model::PushLogVerbosity, KIM_Model_PushLogVerbosity
  !!
  !! \since 2.0
  interface kim_push_log_verbosity
    module procedure kim_model_push_log_verbosity
  end interface kim_push_log_verbosity

  !> \brief \copybrief KIM::Model::PopLogVerbosity
  !!
  !! \sa KIM::Model::, KIM_Model_PopLogVerbosity
  !!
  !! \since 2.0
  interface kim_pop_log_verbosity
    module procedure kim_model_pop_log_verbosity
  end interface kim_pop_log_verbosity

contains
  !> \brief Compares kim_model_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_handle_type), intent(in) :: lhs
    type(kim_model_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_handle_equal = .true.
    else
      kim_model_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_handle_equal

  !> \brief Compares kim_model_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_handle_type), intent(in) :: lhs
    type(kim_model_handle_type), intent(in) :: rhs

    kim_model_handle_not_equal = .not. (lhs .eq. rhs)
  end function kim_model_handle_not_equal

  !> \brief \copybrief KIM::Model::Create
  !!
  !! A Fortran PM must provide a KIM::MODEL_ROUTINE_NAME::Create routine.  The
  !! interface for this is given here.
  !!
  !! A PM not employing a MD should use the following interface (see also
  !! KIM::ModelCreateFunction, \ref KIM_ModelCreateFunction)
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine create(model_create_handle, requested_length_unit, &
  !!     requested_energy_unit, requested_charge_unit, &
  !!     requested_temperature_unit, requested_time_unit, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_create_handle_type), intent(in) :: model_create_handle
  !!     type(kim_length_unit_type), intent(in), value :: requested_length_unit
  !!     type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
  !!     type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
  !!     type(kim_temperature_unit_type), intent(in), value :: &
  !!       requested_temperature_unit
  !!     type(kim_time_unit_type), intent(in), value :: requested_time_unit
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine create
  !! end interface
  !! \endcode
  !!
  !! A MD should use the following interface (see also
  !! KIM::ModelDriverCreateFunction, \ref KIM_ModelDriverCreateFunction)
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine create(model_driver_create_handle, &
  !!     requested_length_unit, requested_energy_unit, requested_charge_unit, &
  !!     requested_temperature_unit, requested_time_unit, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_driver_headers_module
  !!     implicit none
  !!     type(kim_model_driver_create_handle_type), intent(in) :: &
  !!       model_create_handle
  !!     type(kim_length_unit_type), intent(in), value :: requested_length_unit
  !!     type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
  !!     type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
  !!     type(kim_temperature_unit_type), intent(in), value :: &
  !!       requested_temperature_unit
  !!     type(kim_time_unit_type), intent(in), value :: requested_time_unit
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine create
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::Create, KIM_Model_Create
  !!
  !! \since 2.0
  recursive subroutine kim_model_create(numbering, requested_length_unit, &
    requested_energy_unit, requested_charge_unit, &
    requested_temperature_unit, requested_time_unit, model_name, &
    requested_units_accepted, model_handle, ierr)
    use kim_numbering_module, only : kim_numbering_type
    use kim_unit_system_module, only : kim_length_unit_type, &
      kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
      kim_time_unit_type
    implicit none
    interface
      integer(c_int) recursive function create(numbering, &
        requested_length_unit, requested_energy_unit, requested_charge_unit, &
        requested_temperature_unit, requested_time_unit, model_name, &
        requested_units_accepted, model) bind(c, name="KIM_Model_Create")
        use, intrinsic :: iso_c_binding
        use kim_numbering_module, only : kim_numbering_type
        use kim_unit_system_module, only : kim_length_unit_type, &
          kim_energy_unit_type, kim_charge_unit_type, &
          kim_temperature_unit_type, kim_time_unit_type
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
    end interface
    type(kim_numbering_type), intent(in) :: numbering
    type(kim_length_unit_type), intent(in) :: requested_length_unit
    type(kim_energy_unit_type), intent(in) :: requested_energy_unit
    type(kim_charge_unit_type), intent(in) :: requested_charge_unit
    type(kim_temperature_unit_type), intent(in) :: &
      requested_temperature_unit
    type(kim_time_unit_type), intent(in) :: requested_time_unit
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

  !> \brief \copybrief KIM::Model::Destroy
  !!
  !! A Fortran PM must provide a KIM::MODEL_ROUTINE_NAME::Destroy routine.  The
  !! interface for this is given here (see also KIM::ModelDestroyFunction, \ref
  !! KIM_ModelDestroyFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine destroy(model_destroy_handle, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_destroy_handle_type), intent(in) :: model_destroy_handle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine destroy
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::Destroy, KIM_Model_Destroy
  !!
  !! \since 2.0
  recursive subroutine kim_model_destroy(model_handle)
    implicit none
    interface
      recursive subroutine destroy(model) bind(c, name="KIM_Model_Destroy")
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(inout) :: model
      end subroutine destroy
    end interface
    type(kim_model_handle_type), intent(inout) :: model_handle

    type(c_ptr) :: pmodel
    pmodel = model_handle%p
    call destroy(pmodel)
    model_handle%p = c_null_ptr
  end subroutine kim_model_destroy

  !> \brief \copybrief KIM::Model::IsRoutinePresent
  !!
  !! \sa KIM::Model::IsRoutinePresent, KIM_Model_IsRoutinePresent
  !!
  !! \since 2.0
  recursive subroutine kim_model_is_routine_present(model_handle, &
    model_routine_name, present, required, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    use kim_model_routine_name_module, only : kim_model_routine_name_type
    implicit none
    interface
      integer(c_int) recursive function is_routine_present(model, &
        model_routine_name, present, required) &
        bind(c, name="KIM_Model_IsRoutinePresent")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        use kim_model_routine_name_module, only : kim_model_routine_name_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(kim_model_routine_name_type), intent(in), value &
          :: model_routine_name
        integer(c_int), intent(out) :: present
        integer(c_int), intent(out) :: required
      end function is_routine_present
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    type(kim_model_routine_name_type), intent(in) :: model_routine_name
    integer(c_int), intent(out) :: present
    integer(c_int), intent(out) :: required
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = is_routine_present(model, model_routine_name, present, required)
  end subroutine kim_model_is_routine_present

  !> \brief \copybrief KIM::Model::GetInfluenceDistance
  !!
  !! \sa KIM::Model::GetInfluenceDistance, KIM_Model_GetInfluenceDistance
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_influence_distance(model_handle, &
    influence_distance)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine get_influence_distance(model, influence_distance) &
        bind(c, name="KIM_Model_GetInfluenceDistance")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        real(c_double), intent(out) :: influence_distance
      end subroutine get_influence_distance
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    real(c_double), intent(out) :: influence_distance
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call get_influence_distance(model, influence_distance)
  end subroutine kim_model_get_influence_distance

  !> \brief Get Model's number of neighbor lists.
  !!
  !! \sa KIM::Model::GetNeighborListPointers, KIM_Model_GetNeighborListPointers
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_number_of_neighbor_lists(model_handle, &
    number_of_neighbor_lists)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine get_neighbor_list_pointers(model, &
        number_of_neighbor_lists, cutoffs_ptr, &
        model_will_not_request_neighbors_of_noncontributing__ptr) &
        bind(c, name="KIM_Model_GetNeighborListPointers")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(out) :: number_of_neighbor_lists
        type(c_ptr), intent(out) :: cutoffs_ptr
        type(c_ptr), intent(out) :: &
          model_will_not_request_neighbors_of_noncontributing__ptr
      end subroutine get_neighbor_list_pointers
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(out) :: number_of_neighbor_lists
    type(kim_model_type), pointer :: model

    type(c_ptr) cutoffs_ptr, hint_ptr

    call c_f_pointer(model_handle%p, model)
    call get_neighbor_list_pointers(model, number_of_neighbor_lists, &
      cutoffs_ptr, hint_ptr)
  end subroutine kim_model_get_number_of_neighbor_lists

  !> \brief Get Model's neighbor list values
  !!
  !! \sa KIM::Model::GetNeighborListPointers, KIM_Model_GetNeighborListPointers
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_neighbor_list_values(model_handle, &
    cutoffs, model_will_not_request_neighbors_of_noncontributing_particles, &
    ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine get_neighbor_list_pointers(model, &
        number_of_neighbor_lists, cutoffs_ptr, &
        model_will_not_request_neighbors_of_noncontributing__ptr) &
        bind(c, name="KIM_Model_GetNeighborListPointers")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(out) :: number_of_neighbor_lists
        type(c_ptr), intent(out) :: cutoffs_ptr
        type(c_ptr), intent(out) :: &
          model_will_not_request_neighbors_of_noncontributing__ptr
      end subroutine get_neighbor_list_pointers
    end interface
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
      call c_f_pointer(cutoffs_ptr, cutoffs_fpointer, &
        [number_of_neighbor_lists])
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

  !> \brief \copybrief KIM::Model::GetUnits
  !!
  !! \sa KIM::Model::GetUnits, KIM_Model_GetUnits
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_units(model_handle, length_unit, &
    energy_unit, charge_unit, temperature_unit, time_unit)
    use kim_unit_system_module, only : kim_length_unit_type, &
      kim_energy_unit_type, kim_charge_unit_type, kim_temperature_unit_type, &
      kim_time_unit_type
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine get_units(model, length_unit, energy_unit, &
        charge_unit, temperature_unit, time_unit) &
        bind(c, name="KIM_Model_GetUnits")
        use, intrinsic :: iso_c_binding
        use kim_unit_system_module, only : kim_length_unit_type, &
          kim_energy_unit_type, kim_charge_unit_type, &
          kim_temperature_unit_type, kim_time_unit_type
        use kim_interoperable_types_module, only : kim_model_type
        type(kim_model_type), intent(in) :: model
        type(kim_length_unit_type), intent(out) :: length_unit
        type(kim_energy_unit_type), intent(out) :: energy_unit
        type(kim_charge_unit_type), intent(out) :: charge_unit
        type(kim_temperature_unit_type), intent(out) :: temperature_unit
        type(kim_time_unit_type), intent(out) :: time_unit
      end subroutine get_units
    end interface
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

  !> \brief \copybrief KIM::Model::ComputeArgumentsCreate
  !!
  !! A Fortran PM must provide a
  !! KIM::MODEL_ROUTINE_NAME::ComputeArgumentsCreate routine.  The interface
  !! for this is given here (see also KIM::ModelComputeArgumentsCreateFunction,
  !! \ref KIM_ModelComputeArgumentsCreateFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine compute_arguments_create(model_compute_handle, &
  !!     model_compute_arguments_create_handle, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  !!     type(kim_model_compute_arguments_create_handle_type), intent(in) &
  !!       :: model_compute_arguments_create_handle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine compute_arguments_create
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::ComputeArgumentsCreate, KIM_Model_ComputeArgumentsCreate
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_create(model_handle, &
    compute_arguments_handle, ierr)
    use kim_compute_arguments_module, only : &
      kim_compute_arguments_handle_type
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function compute_arguments_create(model, &
        compute_arguments) bind(c, name="KIM_Model_ComputeArgumentsCreate")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(c_ptr), intent(out) :: compute_arguments
      end function compute_arguments_create
    end interface
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

  !> \brief \copybrief KIM::Model::ComputeArgumentsDestroy
  !!
  !! A Fortran PM must provide a
  !! KIM::MODEL_ROUTINE_NAME::ComputeArgumentsDestroy routine.  The interface
  !! for this is given here (see also
  !! KIM::ModelComputeArgumentsDestroyFunction, \ref
  !! KIM_ModelComputeArgumentsDestroyFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine compute_arguments_destroy(model_compute_handle, &
  !!     model_compute_arguments_destroy_handle, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  !!     type(kim_model_compute_arguments_destroy_handle_type), intent(in) &
  !!       :: model_compute_arguments_destroy_handle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine compute_arguments_destroy
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::ComputeArgumentsDestroy, KIM_Model_ComputeArgumentsDestroy
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_arguments_destroy(model_handle, &
    compute_arguments_handle, ierr)
    use kim_compute_arguments_module, only : &
      kim_compute_arguments_handle_type
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function compute_arguments_destroy(model, &
        compute_arguments) bind(c, name="KIM_Model_ComputeArgumentsDestroy")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(c_ptr), intent(inout) :: compute_arguments
      end function compute_arguments_destroy
    end interface
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

  !> \brief \copybrief KIM::Model::Compute
  !!
  !! A Fortran PM must provide a KIM::MODEL_ROUTINE_NAME::Compute routine.  The
  !! interface for this is given here (see also KIM::ModelComputeFunction, \ref
  !! KIM_ModelComputeFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine compute(model_compute_handle, &
  !!     model_compute_arguments_handle, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  !!     type(kim_model_compute_arguments_handle_type), intent(in) :: &
  !!       model_compute_arguments_handle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine compute
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::Compute, KIM_Model_Compute
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute(model_handle, &
    compute_arguments_handle, ierr)
    use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type, &
      kim_model_type
    implicit none
    interface
      integer(c_int) recursive function compute(model, compute_arguments) &
        bind(c, name="KIM_Model_Compute")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
      end function compute
    end interface
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

  !> \brief \copybrief KIM::Model::Extension
  !!
  !! A Fortran PM may provide a KIM::MODEL_ROUTINE_NAME::Extension routine.
  !! The interface for this is given here (see also
  !! KIM::ModelExtensionFunction, \ref KIM_ModelExtensionFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine extension(model_extension_handle, &
  !!     extension_structure, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_extension_handle_type), intent(in) :: &
  !!       model_extension_handle
  !!     type(c_ptr), intent(in), value :: extension_structure
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine extension
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::Extension, KIM_Model_Extension
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension(model_handle, extension_id, &
    extension_structure, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function extension(model, extension_id, &
        extension_structure) bind(c, name="KIM_Model_Extension")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        character(c_char), intent(in) :: extension_id(*)
        type(c_ptr), intent(in), value :: extension_structure
      end function extension
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    character(len=*, kind=c_char), intent(in) :: extension_id
    type(c_ptr), intent(in) :: extension_structure
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = extension(model, trim(extension_id)//c_null_char, &
      extension_structure)
  end subroutine kim_model_extension

  !> \brief \copybrief KIM::Model::ClearThenRefresh
  !!
  !! A Fortran PM may need to provide a KIM::MODEL_ROUTINE_NAME::Refresh
  !! routine.  The interface for this is given here (see also
  !! KIM::ModelRefreshFunction, \ref KIM_ModelRefreshFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine refresh(model_refresh_handle, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_refresh_handle_type), intent(in) :: model_refresh_handle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine refresh
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::ClearThenRefresh, KIM_Model_ClearThenRefresh
  !!
  !! \since 2.0
  recursive subroutine kim_model_clear_then_refresh(model_handle, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function clear_then_refresh(model) &
        bind(c, name="KIM_Model_ClearThenRefresh")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
      end function clear_then_refresh
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = clear_then_refresh(model)
  end subroutine kim_model_clear_then_refresh

  !> \brief \copybrief KIM::Model::WriteParameterizedModel
  !!
  !! A Fortran PM using a MD may provide a
  !! KIM::MODEL_ROUTINE_NAME::WriteParameterizedMdoel routine.  The interface
  !! for this is given here (see also
  !! KIM::ModelWriteParameterizedModelFunction, \ref
  !! KIM_ModelWriteParameterizedModelFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine write_parameterized_model( &
  !!     model_write_parameterized_model_handle, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     use kim_model_headers_module
  !!     implicit none
  !!     type(kim_model_write_parameterized_model_handle_type), intent(in) &
  !!       :: model_write_parameterized_model_handle
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine write_parameterized_model
  !! end interface
  !! \endcode
  !!
  !! \sa KIM::Model::WriteParameterizedModel, KIM_Model_WriteParameterizedModel
  !!
  !! \since 2.0
  recursive subroutine kim_model_write_parameterized_model(model_handle, &
    path, model_name, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function write_parameterized_model(model, &
        path, model_name) bind(c, name="KIM_Model_WriteParameterizedModel")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        character(c_char), intent(in) :: path(*)
        character(c_char), intent(in) :: model_name(*)
      end function write_parameterized_model
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    character(len=*, kind=c_char), intent(in) :: path
    character(len=*, kind=c_char), intent(in) :: model_name
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = write_parameterized_model(model, trim(path)//c_null_char, &
      trim(model_name)//c_null_char)
  end subroutine kim_model_write_parameterized_model

  !> \brief \copybrief KIM::Model::GetSpeciesSupportAndCode
  !!
  !! \sa KIM::Model::GetSpeciesSupportAndCode,
  !! KIM_Model_GetSpeciesSupportAndCode
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_species_support_and_code(model_handle, &
    species_name, species_is_supported, code, ierr)
    use kim_species_name_module, only : kim_species_name_type
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function get_species_support_and_code(model, &
        species_name, species_is_supported, code) &
        bind(c, name="KIM_Model_GetSpeciesSupportAndCode")
        use, intrinsic :: iso_c_binding
        use kim_species_name_module, only : kim_species_name_type
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(kim_species_name_type), intent(in), value :: species_name
        integer(c_int), intent(out) :: species_is_supported
        integer(c_int), intent(out) :: code
      end function get_species_support_and_code
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    type(kim_species_name_type), intent(in) :: species_name
    integer(c_int), intent(out) :: species_is_supported
    integer(c_int), intent(out) :: code
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = get_species_support_and_code(model, species_name, &
      species_is_supported, code)
  end subroutine kim_model_get_species_support_and_code

  !> \brief \copybrief KIM::Model::GetNumberOfParameters
  !!
  !! \sa KIM::Model::GetNumberOfParameters, KIM_Model_GetNumberOfParameters
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_number_of_parameters(model_handle, &
    number_of_parameters)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine get_number_of_parameters(model, &
        number_of_parameters) bind(c, name="KIM_Model_GetNumberOfParameters")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(out) :: number_of_parameters
      end subroutine get_number_of_parameters
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(out) :: number_of_parameters
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call get_number_of_parameters(model, number_of_parameters)
  end subroutine kim_model_get_number_of_parameters

  !> \brief \copybrief KIM::Model::GetParameterMetadata
  !!
  !! \sa KIM::Model::GetParameterMetadata, KIM_Model_GetParameterMetadata
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_parameter_metadata(model_handle, &
    parameter_index, data_type, extent, name, description, ierr)
    use kim_data_type_module, only : kim_data_type_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function get_parameter_metadata(model, &
        parameter_index, data_type, extent, name, description) &
        bind(c, name="KIM_Model_GetParameterMetadata")
        use, intrinsic :: iso_c_binding
        use kim_data_type_module, only : kim_data_type_type
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(in), value :: parameter_index
        type(kim_data_type_type), intent(out) :: data_type
        integer(c_int), intent(out) :: extent
        type(c_ptr), intent(out) :: name
        type(c_ptr), intent(out) :: description
      end function get_parameter_metadata
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(in) :: parameter_index
    type(kim_data_type_type), intent(out) :: data_type
    integer(c_int), intent(out) :: extent
    character(len=*, kind=c_char), intent(out) :: name
    character(len=*, kind=c_char), intent(out) :: description
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    type(c_ptr) :: pname, pdesc

    call c_f_pointer(model_handle%p, model)
    ierr = get_parameter_metadata(model, parameter_index-1, data_type, extent, &
      pname, pdesc)
    call kim_convert_c_char_ptr_to_string(pname, name)
    call kim_convert_c_char_ptr_to_string(pdesc, description)
  end subroutine kim_model_get_parameter_metadata

  !> \brief \copybrief KIM::Model::GetParameter
  !!
  !! \sa KIM::Model::GetParameter, KIM_Model_GetParameterInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_parameter_integer(model_handle, &
    parameter_index, array_index, parameter_value, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function get_parameter_integer(model, &
        parameter_index, array_index, parameter_value) &
        bind(c, name="KIM_Model_GetParameterInteger")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(in), value :: parameter_index
        integer(c_int), intent(in), value :: array_index
        integer(c_int), intent(out) :: parameter_value
      end function get_parameter_integer
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(in) :: parameter_index
    integer(c_int), intent(in) :: array_index
    integer(c_int), intent(out) :: parameter_value
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = get_parameter_integer(model, parameter_index-1, array_index-1, &
      parameter_value)
  end subroutine kim_model_get_parameter_integer

  !> \brief \copybrief KIM::Model::GetParameter
  !!
  !! \sa KIM::Model::GetParameter, KIM_Model_GetParameterDouble
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_parameter_double(model_handle, &
    parameter_index, array_index, parameter_value, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function get_parameter_double(model, &
        parameter_index, array_index, parameter_value) &
        bind(c, name="KIM_Model_GetParameterDouble")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(in), value :: parameter_index
        integer(c_int), intent(in), value :: array_index
        real(c_double), intent(out) :: parameter_value
      end function get_parameter_double
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(in) :: parameter_index
    integer(c_int), intent(in) :: array_index
    real(c_double), intent(out) :: parameter_value
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = get_parameter_double(model, parameter_index-1, array_index-1, &
      parameter_value)
  end subroutine kim_model_get_parameter_double

  !> \brief \copybrief KIM::Model::SetParameter
  !!
  !! \sa KIM::Model::SetParameter, KIM_Model_SetParameterInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_set_parameter_integer(model_handle, &
    parameter_index, array_index, parameter_value, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function set_parameter_integer(model, &
        parameter_index, array_index, parameter_value) &
        bind(c, name="KIM_Model_SetParameterInteger")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(in), value :: parameter_index
        integer(c_int), intent(in), value :: array_index
        integer(c_int), intent(in), value :: parameter_value
      end function set_parameter_integer
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(in) :: parameter_index
    integer(c_int), intent(in) :: array_index
    integer(c_int), intent(in) :: parameter_value
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = set_parameter_integer(model, parameter_index-1, array_index-1, &
      parameter_value)
  end subroutine kim_model_set_parameter_integer

  !> \brief \copybrief KIM::Model::SetParameter
  !!
  !! \sa KIM::Model::SetParameter, KIM_Model_SetParameterDouble
  !!
  !! \since 2.0
  recursive subroutine kim_model_set_parameter_double(model_handle, &
    parameter_index, array_index, parameter_value, ierr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      integer(c_int) recursive function set_parameter_double(model, &
        parameter_index, array_index, parameter_value) &
        bind(c, name="KIM_Model_SetParameterDouble")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        integer(c_int), intent(in), value :: parameter_index
        integer(c_int), intent(in), value :: array_index
        real(c_double), intent(in), value :: parameter_value
      end function set_parameter_double
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    integer(c_int), intent(in) :: parameter_index
    integer(c_int), intent(in) :: array_index
    real(c_double), intent(in) :: parameter_value
    integer(c_int), intent(out) :: ierr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    ierr = set_parameter_double(model, parameter_index-1, array_index-1, &
      parameter_value)
  end subroutine kim_model_set_parameter_double

  !> \brief \copybrief KIM::Model::SetSimulatorBufferPointer
  !!
  !! \sa KIM::Model::SetSimulatorBufferPointer,
  !! KIM_Model_SetSimulatorBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_set_simulator_buffer_pointer(model_handle, ptr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine set_simulator_buffer_pointer(model, ptr) &
        bind(c, name="KIM_Model_SetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_simulator_buffer_pointer
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call set_simulator_buffer_pointer(model, ptr)
  end subroutine kim_model_set_simulator_buffer_pointer

  !> \brief \copybrief KIM::Model::GetSimulatorBufferPointer
  !!
  !! \sa KIM::Model::GetSimulatorBufferPointer,
  !! KIM_Model_GetSimulatorBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_get_simulator_buffer_pointer(model_handle, ptr)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine get_simulator_buffer_pointer(model, ptr) &
        bind(c, name="KIM_Model_GetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(c_ptr), intent(out) :: ptr
      end subroutine get_simulator_buffer_pointer
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call get_simulator_buffer_pointer(model, ptr)
  end subroutine kim_model_get_simulator_buffer_pointer

  !> \brief \copybrief KIM::Model::ToString
  !!
  !! \sa KIM::Model::ToString, KIM_Model_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_to_string(model_handle, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      type(c_ptr) recursive function model_string(model) &
        bind(c, name="KIM_Model_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
      end function model_string
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_type), pointer :: model

    type(c_ptr) :: p

    call c_f_pointer(model_handle%p, model)
    p = model_string(model)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_to_string

  !> \brief \copybrief KIM::Model::SetLogID
  !!
  !! \sa KIM::Model::SetLogID, KIM_Model_SetLogID
  !!
  !! \since 2.0
  recursive subroutine kim_model_set_log_id(model_handle, log_id)
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine set_log_id(model, log_id) &
        bind(c, name="KIM_Model_SetLogID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        character(c_char), intent(in) :: log_id(*)
      end subroutine set_log_id
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    character(len=*, kind=c_char), intent(in) :: log_id
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call set_log_id(model, trim(log_id)//c_null_char)
  end subroutine kim_model_set_log_id

  !> \brief \copybrief KIM::Model::PushLogVerbosity
  !!
  !! \sa KIM::Model::PushLogVerbosity, KIM_Model_PushLogVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_model_push_log_verbosity(model_handle, log_verbosity)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine push_log_verbosity(model, log_verbosity) &
        bind(c, name="KIM_Model_PushLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_log_verbosity
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call push_log_verbosity(model, log_verbosity)
  end subroutine kim_model_push_log_verbosity

  !> \brief \copybrief KIM::Model::PopLogVerbosity
  !!
  !! \sa KIM::Model::, KIM_Model_PopLogVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_model_pop_log_verbosity(model_handle)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_model_type
    implicit none
    interface
      recursive subroutine pop_log_verbosity(model) &
        bind(c, name="KIM_Model_PopLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_model_type
        implicit none
        type(kim_model_type), intent(in) :: model
      end subroutine pop_log_verbosity
    end interface
    type(kim_model_handle_type), intent(in) :: model_handle
    type(kim_model_type), pointer :: model

    call c_f_pointer(model_handle%p, model)
    call pop_log_verbosity(model)
  end subroutine kim_model_pop_log_verbosity
end module kim_model_module
