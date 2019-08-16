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
! Release: This file is part of the kim-api-2.1.3 package.
!


!> \brief \copybrief KIM::SimulatorModel
!!
!! \sa KIM::SimulatorModel, KIM_SimulatorModel
!!
!! \since 2.1
module kim_simulator_model_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_simulator_model_handle_type, &

    ! Constants
    KIM_SIMULATOR_MODEL_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_simulator_model_create, &
    kim_simulator_model_destroy, &
    kim_get_simulator_name_and_version, &
    kim_get_number_of_supported_species, &
    kim_get_supported_species, &
    kim_open_and_initialize_template_map, &
    kim_template_map_is_open, &
    kim_add_template_map, &
    kim_close_template_map, &
    kim_get_number_of_simulator_fields, &
    kim_get_simulator_field_metadata, &
    kim_get_simulator_field_line, &
    kim_get_parameter_file_directory_name, &
    kim_get_specification_file_name, &
    kim_get_number_of_parameter_files, &
    kim_get_parameter_file_name, &
    kim_set_simulator_buffer_pointer, &
    kim_get_simulator_buffer_pointer, &
    kim_to_string, &
    kim_set_log_id, &
    kim_push_log_verbosity, &
    kim_pop_log_verbosity


  !> \brief \copybrief KIM::SimulatorModel
  !!
  !! \sa KIM::SimulatorModel, KIM_SimulatorModel
  !!
  !! \since 2.1
  type, bind(c) :: kim_simulator_model_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_simulator_model_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.1
  type(kim_simulator_model_handle_type), protected, save &
    :: KIM_SIMULATOR_MODEL_NULL_HANDLE

  !> \brief Compares kim_simulator_model_handle_type's for equality.
  !!
  !! \since 2.1
  interface operator (.eq.)
    module procedure kim_simulator_model_handle_equal
  end interface operator (.eq.)

  !> \brief Compares kim_simulator_model_handle_type's for inequality.
  !!
  !! \since 2.1
  interface operator (.ne.)
    module procedure kim_simulator_model_handle_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorNameAndVersion
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorNameAndVersion,
  !! KIM_SimulatorModel_GetSimulatorNameAndVersion
  !!
  !! \since 2.1
  interface kim_get_simulator_name_and_version
    module procedure kim_simulator_model_get_simulator_name_and_version
  end interface kim_get_simulator_name_and_version

  !> \brief \copybrief KIM::SimulatorModel::GetNumberOfSupportedSpecies
  !!
  !! \sa KIM::SimulatorModel::GetNumberOfSupportedSpecies,
  !! KIM_SimulatorModel_GetNumberOfSupportedSpecies
  !!
  !! \since 2.1
  interface kim_get_number_of_supported_species
    module procedure kim_simulator_model_get_number_of_supported_species
  end interface kim_get_number_of_supported_species

  !> \brief \copybrief KIM::SimulatorModel::GetSupportedSpecies
  !!
  !! \sa KIM::SimulatorModel::GetSupportedSpecies,
  !! KIM_SimulatorModel_GetSupportedSpecies
  !!
  !! \since 2.1
  interface kim_get_supported_species
    module procedure kim_simulator_model_get_supported_species
  end interface kim_get_supported_species

  !> \brief \copybrief KIM::SimulatorModel::OpenAndInitializeTemplateMap
  !!
  !! \sa KIM::SimulatorModel::OpenAndInitializeTemplateMap,
  !! KIM_SimulatorModel_OpenAndInitializeTemplateMap
  !!
  !! \since 2.1
  interface kim_open_and_initialize_template_map
    module procedure kim_simulator_model_open_and_initialize_template_map
  end interface kim_open_and_initialize_template_map

  !> \brief \copybrief KIM::SimulatorModel::TemplateMapIsOpen
  !!
  !! \sa KIM::SimulatorModel::TemplateMapIsOpen,
  !! KIM_SimulatorModel_TemplateMapIsOpen
  !!
  !! \since 2.1
  interface kim_template_map_is_open
    module procedure kim_simulator_model_template_map_is_open
  end interface kim_template_map_is_open

  !> \brief \copybrief KIM::SimulatorModel::AddTemplateMap
  !!
  !! \sa KIM::SimulatorModel::AddTemplateMap, KIM_SimulatorModel_AddTemplateMap
  !!
  !! \since 2.1
  interface kim_add_template_map
    module procedure kim_simulator_model_add_template_map
  end interface kim_add_template_map

  !> \brief \copybrief KIM::SimulatorModel::CloseTemplateMap
  !!
  !! \sa KIM::SimulatorModel::CloseTemplateMap,
  !! KIM_SimulatorModel_CloseTemplateMap
  !!
  !! \since 2.1
  interface kim_close_template_map
    module procedure kim_simulator_model_close_template_map
  end interface kim_close_template_map

  !> \brief \copybrief KIM::SimulatorModel::GetNumberOfSimulatorFields
  !!
  !! \sa KIM::SimulatorModel::GetNumberOfSimulatorFields,
  !! KIM_SimulatorModel_GetNumberOfSimulatorFields
  !!
  !! \since 2.1
  interface kim_get_number_of_simulator_fields
    module procedure kim_simulator_model_get_number_of_simulator_fields
  end interface kim_get_number_of_simulator_fields

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorFieldMetadata
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorFieldMetadata,
  !! KIM_SimulatorModel_GetSimulatorFieldMetadata
  !!
  !! \since 2.1
  interface kim_get_simulator_field_metadata
    module procedure kim_simulator_model_get_simulator_field_metadata
  end interface kim_get_simulator_field_metadata

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorFieldLine
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorFieldLine,
  !! KIM_SimulatorModel_GetSimulatorFieldLine
  !!
  !! \since 2.1
  interface kim_get_simulator_field_line
    module procedure kim_simulator_model_get_simulator_field_line
  end interface kim_get_simulator_field_line

  !> \brief \copybrief KIM::SimulatorModel::GetParameterFileDirectoryName
  !!
  !! \sa KIM::SimulatorModel::GetParameterFileDirectoryName,
  !! KIM_SimulatorModel_GetParameterFileDirectoryName
  !!
  !! \since 2.1
  interface kim_get_parameter_file_directory_name
    module procedure kim_simulator_model_get_parameter_file_directory_name
  end interface kim_get_parameter_file_directory_name

  !> \brief \copybrief KIM::SimulatorModel::GetSpecificationFileName
  !!
  !! \sa KIM::SimulatorModel::GetSpecificationFileName,
  !! KIM_SimulatorModel_GetSpecificationFileName
  !!
  !! \since 2.1
  interface kim_get_specification_file_name
    module procedure kim_simulator_model_get_specification_file_name
  end interface kim_get_specification_file_name

  !> \brief \copybrief KIM::SimulatorModel::GetNumberOfParameterFiles
  !!
  !! \sa KIM::SimulatorModel::GetNumberOfParameterFiles,
  !! KIM_SimulatorModel_GetNumberOfParameterFiles
  !!
  !! \since 2.1
  interface kim_get_number_of_parameter_files
    module procedure kim_simulator_model_get_number_of_parameter_files
  end interface kim_get_number_of_parameter_files

  !> \brief \copybrief KIM::SimulatorModel::GetParameterFileName
  !!
  !! \sa KIM::SimulatorModel::GetParameterFileName,
  !! KIM_SimulatorModel_GetParameterFileName
  !!
  !! \since 2.1
  interface kim_get_parameter_file_name
    module procedure kim_simulator_model_get_parameter_file_name
  end interface kim_get_parameter_file_name

  !> \brief \copybrief KIM::SimulatorModel::SetSimulatorBufferPointer
  !!
  !! \sa KIM::SimulatorModel::SetSimulatorBufferPointer,
  !! KIM_SimulatorModel_SetSimulatorBufferPointer
  !!
  !! \since 2.1
  interface kim_set_simulator_buffer_pointer
    module procedure kim_simulator_model_set_simulator_buffer_pointer
  end interface kim_set_simulator_buffer_pointer

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorBufferPointer
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorBufferPointer,
  !! KIM_SimulatorModel_GetSimulatorBufferPointer
  !!
  !! \since 2.1
  interface kim_get_simulator_buffer_pointer
    module procedure kim_simulator_model_get_simulator_buffer_pointer
  end interface kim_get_simulator_buffer_pointer

  !> \brief \copybrief KIM::SimulatorModel::ToString
  !!
  !! \sa KIM::SimulatorModel::ToString, KIM_SimulatorModel_ToString
  !!
  !! \since 2.1
  interface kim_to_string
    module procedure kim_simulator_model_to_string
  end interface kim_to_string

  !> \brief \copybrief KIM::SimulatorModel::SetLogID
  !!
  !! \sa KIM::SimulatorModel::SetLogID, KIM_SimulatorModel_SetLogID
  !!
  !! \since 2.1
  interface kim_set_log_id
    module procedure kim_simulator_model_set_log_id
  end interface kim_set_log_id

  !> \brief \copybrief KIM::SimulatorModel::PushLogVerbosity
  !!
  !! \sa KIM::SimulatorModel::PushLogVerbosity,
  !! KIM_SimulatorModel_PushLogVerbosity
  !!
  !! \since 2.1
  interface kim_push_log_verbosity
    module procedure kim_simulator_model_push_log_verbosity
  end interface kim_push_log_verbosity

  !> \brief \copybrief KIM::SimulatorModel::PopLogVerbosity
  !!
  !! \sa KIM::SimulatorModel::, KIM_SimulatorModel_PopLogVerbosity
  !!
  !! \since 2.1
  interface kim_pop_log_verbosity
    module procedure kim_simulator_model_pop_log_verbosity
  end interface kim_pop_log_verbosity

contains
  !> \brief Compares kim_simulator_model_handle_type's for equality.
  !!
  !! \since 2.1
  logical recursive function kim_simulator_model_handle_equal(lhs, rhs)
    implicit none
    type(kim_simulator_model_handle_type), intent(in) :: lhs
    type(kim_simulator_model_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_simulator_model_handle_equal = .true.
    else
      kim_simulator_model_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_simulator_model_handle_equal

  !> \brief Compares kim_simulator_model_handle_type's for inequality.
  !!
  !! \since 2.1
  logical recursive function kim_simulator_model_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_simulator_model_handle_type), intent(in) :: lhs
    type(kim_simulator_model_handle_type), intent(in) :: rhs

    kim_simulator_model_handle_not_equal = .not. (lhs .eq. rhs)
  end function kim_simulator_model_handle_not_equal

  !> \brief \copybrief KIM::SimulatorModel::Create
  !!
  !! \sa KIM::SimulatorModel::Create, KIM_SimulatorModel_Create
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_create(simulator_model_name, &
    simulator_model_handle, ierr)
    implicit none
    interface
      integer(c_int) recursive function create(simulator_model_name, &
        simulator_model) bind(c, name="KIM_SimulatorModel_Create")
        use, intrinsic :: iso_c_binding
        implicit none
        character(c_char), intent(in) :: simulator_model_name(*)
        type(c_ptr), intent(out) :: simulator_model
      end function create
    end interface
    character(len=*, kind=c_char), intent(in) :: simulator_model_name
    type(kim_simulator_model_handle_type), intent(out) :: simulator_model_handle
    integer(c_int), intent(out) :: ierr

    type(c_ptr) :: psimulator_model

    ierr = create(trim(simulator_model_name)//c_null_char, psimulator_model)
    simulator_model_handle%p = psimulator_model
  end subroutine kim_simulator_model_create

  !> \brief \copybrief KIM::SimulatorModel::Destroy
  !!
  !! \sa KIM::SimulatorModel::Destroy, KIM_SimulatorModel_Destroy
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_destroy(simulator_model_handle)
    implicit none
    interface
      recursive subroutine destroy(simulator_model) &
        bind(c, name="KIM_SimulatorModel_Destroy")
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(inout) :: simulator_model
      end subroutine destroy
    end interface
    type(kim_simulator_model_handle_type), intent(inout) :: &
      simulator_model_handle

    type(c_ptr) :: psimulator_model
    psimulator_model = simulator_model_handle%p
    call destroy(psimulator_model)
    simulator_model_handle%p = c_null_ptr
  end subroutine kim_simulator_model_destroy

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorNameAndVersion
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorNameAndVersion,
  !! KIM_SimulatorModel_GetSimulatorNameAndVersion
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_simulator_name_and_version( &
    simulator_model_handle, simulator_name, simulator_version)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_simulator_name_and_version(simulator_model, &
        simulator_name, simulator_version) &
        bind(c, name="KIM_SimulatorModel_GetSimulatorNameAndVersion")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        type(c_ptr), intent(out) :: simulator_name
        type(c_ptr), intent(out) :: simulator_version
      end subroutine get_simulator_name_and_version
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    character(len=*, kind=c_char), intent(out) :: simulator_name
    character(len=*, kind=c_char), intent(out) :: simulator_version
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) psimulator_name, psimulator_version

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_simulator_name_and_version(simulator_model, psimulator_name, &
      psimulator_version)
    call kim_convert_c_char_ptr_to_string(psimulator_name, simulator_name)
    call kim_convert_c_char_ptr_to_string(psimulator_version, simulator_version)
  end subroutine kim_simulator_model_get_simulator_name_and_version

  !> \brief \copybrief KIM::SimulatorModel::GetNumberOfSupportedSpecies
  !!
  !! \sa KIM::SimulatorModel::GetNumberOfSupportedSpecies,
  !! KIM_SimulatorModel_GetNumberOfSupportedSpecies
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_number_of_supported_species( &
    simulator_model_handle, number_of_supported_species)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine get_number_of_supported_species(simulator_model, &
        number_of_supported_species) &
        bind(c, name="KIM_SimulatorModel_GetNumberOfSupportedSpecies")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(out) :: number_of_supported_species
      end subroutine get_number_of_supported_species
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(out) :: number_of_supported_species
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_number_of_supported_species(simulator_model, &
      number_of_supported_species)
  end subroutine kim_simulator_model_get_number_of_supported_species

  !> \brief \copybrief KIM::SimulatorModel::GetSupportedSpecies
  !!
  !! \sa KIM::SimulatorModel::GetSupportedSpecies,
  !! KIM_SimulatorModel_GetSupportedSpecies
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_supported_species( &
    simulator_model_handle, index, species_name, ierr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_supported_species(simulator_model, &
        index, species_name) &
        bind(c, name="KIM_SimulatorModel_GetSupportedSpecies")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: species_name
      end function get_supported_species
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: species_name
    integer(c_int), intent(out) :: ierr
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) pspecies_name

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    ierr = get_supported_species(simulator_model, index-1, pspecies_name)
    call kim_convert_c_char_ptr_to_string(pspecies_name, species_name)
  end subroutine kim_simulator_model_get_supported_species

  !> \brief \copybrief KIM::SimulatorModel::OpenAndInitializeTemplateMap
  !!
  !! \sa KIM::SimulatorModel::OpenAndInitializeTemplateMap,
  !! KIM_SimulatorModel_OpenAndInitializeTemplateMap
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_open_and_initialize_template_map( &
    simulator_model_handle)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
        recursive subroutine open_and_initialize_template_map(simulator_model) &
        bind(c, name="KIM_SimulatorModel_OpenAndInitializeTemplateMap")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
      end subroutine open_and_initialize_template_map
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call open_and_initialize_template_map(simulator_model)
  end subroutine kim_simulator_model_open_and_initialize_template_map

  !> \brief \copybrief KIM::SimulatorModel::TemplateMapIsOpen
  !!
  !! \sa KIM::SimulatorModel::TemplateMapIsOpen,
  !! KIM_SimulatorModel_TemplateMapIsOpen
  !!
  !! \since 2.1
  integer(c_int) recursive function kim_simulator_model_template_map_is_open( &
    simulator_model_handle)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      integer(c_int) recursive function template_map_is_open(simulator_model) &
        bind(c, name="KIM_SimulatorModel_TemplateMapIsOpen")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
      end function template_map_is_open
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    kim_simulator_model_template_map_is_open = template_map_is_open( &
      simulator_model)
  end function kim_simulator_model_template_map_is_open

  !> \brief \copybrief KIM::SimulatorModel::AddTemplateMap
  !!
  !! \sa KIM::SimulatorModel::AddTemplateMap, KIM_SimulatorModel_AddTemplateMap
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_add_template_map( &
    simulator_model_handle, key, value, ierr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function add_template_map(simulator_model, key, &
        value) bind(c, name="KIM_SimulatorModel_AddTemplateMap")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        character(c_char), intent(in) :: key(*)
        character(c_char), intent(in) :: value(*)
      end function add_template_map
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    character(len=*, kind=c_char), intent(in) :: key
    character(len=*, kind=c_char), intent(in) :: value
    integer(c_int), intent(out) :: ierr
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    ierr = add_template_map(simulator_model, trim(key)//c_null_char, &
      trim(value)//c_null_char)
  end subroutine kim_simulator_model_add_template_map

  !> \brief \copybrief KIM::SimulatorModel::CloseTemplateMap
  !!
  !! \sa KIM::SimulatorModel::CloseTemplateMap,
  !! KIM_SimulatorModel_CloseTemplateMap
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_close_template_map( &
    simulator_model_handle)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
        recursive subroutine close_template_map(simulator_model) &
        bind(c, name="KIM_SimulatorModel_CloseTemplateMap")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
      end subroutine close_template_map
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call close_template_map(simulator_model)
  end subroutine kim_simulator_model_close_template_map

  !> \brief \copybrief KIM::SimulatorModel::GetNumberOfSimulatorFields
  !!
  !! \sa KIM::SimulatorModel::GetNumberOfSimulatorFields,
  !! KIM_SimulatorModel_GetNumberOfSimulatorFields
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_number_of_simulator_fields( &
    simulator_model_handle, number_of_simulator_fields)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine get_number_of_simulator_fields(simulator_model, &
        number_of_simulator_fields) &
        bind(c, name="KIM_SimulatorModel_GetNumberOfSimulatorFields")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(out) :: number_of_simulator_fields
      end subroutine get_number_of_simulator_fields
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(out) :: number_of_simulator_fields
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_number_of_simulator_fields(simulator_model, &
      number_of_simulator_fields)
  end subroutine kim_simulator_model_get_number_of_simulator_fields

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorFieldMetadata
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorFieldMetadata,
  !! KIM_SimulatorModel_GetSimulatorFieldMetadata
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_simulator_field_metadata( &
    simulator_model_handle, field_index, extent, field_name, ierr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_simulator_field_metadata( &
        simulator_model, field_index, extent, field_name) &
        bind(c, name="KIM_SimulatorModel_GetSimulatorFieldMetadata")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(in), value :: field_index
        integer(c_int), intent(out) :: extent
        type(c_ptr), intent(out) :: field_name
      end function get_simulator_field_metadata
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(in) :: field_index
    integer(c_int), intent(out) :: extent
    character(len=*, kind=c_char), intent(out) :: field_name
    integer(c_int), intent(out) :: ierr
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) pfield_name

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    ierr = get_simulator_field_metadata(simulator_model, field_index-1, &
      extent, pfield_name)
    call kim_convert_c_char_ptr_to_string(pfield_name, field_name)
  end subroutine kim_simulator_model_get_simulator_field_metadata

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorFieldLine
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorFieldLine,
  !! KIM_SimulatorModel_GetSimulatorFieldLine
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_simulator_field_line( &
    simulator_model_handle, field_index, line_index, line_value, ierr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_simulator_field_line( &
        simulator_model, field_index, line_index, line_value) &
        bind(c, name="KIM_SimulatorModel_GetSimulatorFieldLine")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(in), value :: field_index
        integer(c_int), intent(in), value :: line_index
        type(c_ptr), intent(out) :: line_value
      end function get_simulator_field_line
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(in), value :: field_index
    integer(c_int), intent(in), value :: line_index
    character(len=*, kind=c_char), intent(out) :: line_value
    integer(c_int), intent(out) :: ierr
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) pline_value

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    ierr = get_simulator_field_line(simulator_model, field_index-1, &
      line_index-1, pline_value)
    call kim_convert_c_char_ptr_to_string(pline_value, line_value)
  end subroutine kim_simulator_model_get_simulator_field_line

  !> \brief \copybrief KIM::SimulatorModel::GetParameterFileDirectoryName
  !!
  !! \sa KIM::SimulatorModel::GetParameterFileDirectoryName,
  !! KIM_SimulatorModel_GetParameterFileDirectoryName
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_parameter_file_directory_name( &
    simulator_model_handle, directory_name)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_parameter_file_directory_name(simulator_model, &
        directory_name) &
        bind(c, name="KIM_SimulatorModel_GetParameterFileDirectoryName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        type(c_ptr), intent(out) :: directory_name
      end subroutine get_parameter_file_directory_name
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    character(len=*, kind=c_char), intent(out) :: directory_name
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) pdirectory_name

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_parameter_file_directory_name(simulator_model, pdirectory_name)
    call kim_convert_c_char_ptr_to_string(pdirectory_name, directory_name)
  end subroutine kim_simulator_model_get_parameter_file_directory_name

  !> \brief \copybrief KIM::SimulatorModel::GetSpecificationFileName
  !!
  !! \sa KIM::SimulatorModel::GetSpecificationFileName,
  !! KIM_SimulatorModel_GetSpecificationFileName
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_specification_file_name( &
    simulator_model_handle, specification_file_name)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_specification_file_name(simulator_model, &
        specification_file_name) &
        bind(c, name="KIM_SimulatorModel_GetSpecificationFileName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        type(c_ptr), intent(out) :: specification_file_name
      end subroutine get_specification_file_name
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    character(len=*, kind=c_char), intent(out) :: specification_file_name
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) pspecification_file_name

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_specification_file_name(simulator_model, pspecification_file_name)
    call kim_convert_c_char_ptr_to_string(pspecification_file_name, &
      specification_file_name)
  end subroutine kim_simulator_model_get_specification_file_name

  !> \brief \copybrief KIM::SimulatorModel::GetNumberOfParameterFiles
  !!
  !! \sa KIM::SimulatorModel::GetNumberOfParameterFiles,
  !! KIM_SimulatorModel_GetNumberOfParameterFiles
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_number_of_parameter_files( &
    simulator_model_handle, number_of_parameter_files)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine get_number_of_parameter_files(simulator_model, &
        number_of_parameter_files) &
        bind(c, name="KIM_SimulatorModel_GetNumberOfParameterFiles")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(out) :: number_of_parameter_files
      end subroutine get_number_of_parameter_files
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(out) :: number_of_parameter_files
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_number_of_parameter_files(simulator_model, &
      number_of_parameter_files)
  end subroutine kim_simulator_model_get_number_of_parameter_files

  !> \brief \copybrief KIM::SimulatorModel::GetParameterFileName
  !!
  !! \sa KIM::SimulatorModel::GetParameterFileName,
  !! KIM_SimulatorModel_GetParameterFileName
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_parameter_file_name( &
    simulator_model_handle, index, parameter_file_name, ierr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      integer(c_int) recursive function get_parameter_file_name( &
        simulator_model, index, parameter_file_name) &
        bind(c, name="KIM_SimulatorModel_GetParameterFileName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: parameter_file_name
      end function get_parameter_file_name
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: parameter_file_name
    integer(c_int), intent(out) :: ierr
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) pparameter_file_name

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    ierr = get_parameter_file_name(simulator_model, index-1, &
      pparameter_file_name)
    call kim_convert_c_char_ptr_to_string(pparameter_file_name, &
      parameter_file_name)
  end subroutine kim_simulator_model_get_parameter_file_name

  !> \brief \copybrief KIM::SimulatorModel::SetSimulatorBufferPointer
  !!
  !! \sa KIM::SimulatorModel::SetSimulatorBufferPointer,
  !! KIM_SimulatorModel_SetSimulatorBufferPointer
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_set_simulator_buffer_pointer( &
    simulator_model_handle, ptr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine set_simulator_buffer_pointer(simulator_model, ptr) &
        bind(c, name="KIM_SimulatorModel_SetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_simulator_buffer_pointer
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call set_simulator_buffer_pointer(simulator_model, ptr)
  end subroutine kim_simulator_model_set_simulator_buffer_pointer

  !> \brief \copybrief KIM::SimulatorModel::GetSimulatorBufferPointer
  !!
  !! \sa KIM::SimulatorModel::GetSimulatorBufferPointer,
  !! KIM_SimulatorModel_GetSimulatorBufferPointer
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_get_simulator_buffer_pointer( &
    simulator_model_handle, ptr)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine get_simulator_buffer_pointer(simulator_model, ptr) &
        bind(c, name="KIM_SimulatorModel_GetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        type(c_ptr), intent(out) :: ptr
      end subroutine get_simulator_buffer_pointer
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call get_simulator_buffer_pointer(simulator_model, ptr)
  end subroutine kim_simulator_model_get_simulator_buffer_pointer

  !> \brief \copybrief KIM::SimulatorModel::ToString
  !!
  !! \sa KIM::SimulatorModel::ToString, KIM_SimulatorModel_ToString
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_to_string(simulator_model_handle, &
    string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      type(c_ptr) recursive function model_string(simulator_model) &
        bind(c, name="KIM_SimulatorModel_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
      end function model_string
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_simulator_model_type), pointer :: simulator_model

    type(c_ptr) :: p

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    p = model_string(simulator_model)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_simulator_model_to_string

  !> \brief \copybrief KIM::SimulatorModel::SetLogID
  !!
  !! \sa KIM::SimulatorModel::SetLogID, KIM_SimulatorModel_SetLogID
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_set_log_id(simulator_model_handle, &
    log_id)
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine set_log_id(simulator_model, log_id) &
        bind(c, name="KIM_SimulatorModel_SetLogID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        character(c_char), intent(in) :: log_id(*)
      end subroutine set_log_id
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    character(len=*, kind=c_char), intent(in) :: log_id
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call set_log_id(simulator_model, trim(log_id)//c_null_char)
  end subroutine kim_simulator_model_set_log_id

  !> \brief \copybrief KIM::SimulatorModel::PushLogVerbosity
  !!
  !! \sa KIM::SimulatorModel::PushLogVerbosity,
  !! KIM_SimulatorModel_PushLogVerbosity
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_push_log_verbosity( &
    simulator_model_handle, log_verbosity)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine push_log_verbosity(simulator_model, log_verbosity) &
        bind(c, name="KIM_SimulatorModel_PushLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_log_verbosity
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call push_log_verbosity(simulator_model, log_verbosity)
  end subroutine kim_simulator_model_push_log_verbosity

  !> \brief \copybrief KIM::SimulatorModel::PopLogVerbosity
  !!
  !! \sa KIM::SimulatorModel::, KIM_SimulatorModel_PopLogVerbosity
  !!
  !! \since 2.1
  recursive subroutine kim_simulator_model_pop_log_verbosity( &
    simulator_model_handle)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_simulator_model_type
    implicit none
    interface
      recursive subroutine pop_log_verbosity(simulator_model) &
        bind(c, name="KIM_SimulatorModel_PopLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_simulator_model_type
        implicit none
        type(kim_simulator_model_type), intent(in) :: simulator_model
      end subroutine pop_log_verbosity
    end interface
    type(kim_simulator_model_handle_type), intent(in) :: simulator_model_handle
    type(kim_simulator_model_type), pointer :: simulator_model

    call c_f_pointer(simulator_model_handle%p, simulator_model)
    call pop_log_verbosity(simulator_model)
  end subroutine kim_simulator_model_pop_log_verbosity
end module kim_simulator_model_module
