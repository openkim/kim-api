!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2022, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!
! SPDX-License-Identifier: LGPL-2.1-or-later
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!

!
! Release: This file is part of the kim-api-2.4.1 package.
!

!> \brief \copybrief KIM::ModelExtension
!!
!! \sa KIM::ModelExtension, KIM_ModelExtension
!!
!! \since 2.0
module kim_model_extension_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_extension_handle_type, &
    ! Constants
    KIM_MODEL_EXTENSION_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
    kim_get_extension_id, &
    kim_to_model, &
    kim_to_model_compute, &
    kim_to_model_create, &
    kim_to_model_destroy, &
    kim_to_model_driver_create, &
    kim_to_model_refresh, &
    kim_to_model_write_parameterized_model, &
    kim_to_model_compute_arguments, &
    kim_to_model_compute_arguments_create, &
    kim_to_model_compute_arguments_destroy, &
    kim_c_char_array_to_string, &
    kim_c_char_ptr_to_string, &
    kim_string_to_c_char_array, &
    kim_get_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string

  !> \brief \copybrief KIM::ModelExtension
  !!
  !! \sa KIM::ModelExtension, KIM_ModelExtension
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_extension_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_extension_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_extension_handle_type), protected, save &
    :: KIM_MODEL_EXTENSION_NULL_HANDLE

  !> \brief Compares kim_model_extension_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_model_extension_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_model_extension_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_model_extension_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ModelExtension::GetExtensionID
  !!
  !! \sa KIM::ModelExtension::GetExtensionID, KIM_ModelExtension_GetExtensionID
  !!
  !! \since 2.0
  interface kim_get_extension_id
    module procedure kim_model_extension_get_extension_id
  end interface kim_get_extension_id

  !> \brief \copybrief KIM::ModelExtension::Model
  !!
  !! \sa KIM::ModelExtension::Model, KIM_ModelExtension_ToModel
  !!
  !! \since 2.0
  interface kim_to_model
    module procedure kim_model_extension_to_model
  end interface kim_to_model

  !> \brief \copybrief KIM::ModelExtension::ModelCompute
  !!
  !! \sa KIM::ModelExtension::ModelCompute, KIM_ModelExtension_ToModelCompute
  !!
  !! \since 2.0
  interface kim_to_model_compute
    module procedure kim_model_extension_to_model_compute
  end interface kim_to_model_compute

  !> \brief \copybrief KIM::ModelExtension::ModelCompute
  !!
  !! \sa KIM::ModelExtension::ModelCompute, KIM_ModelExtension_ToModelCompute
  !!
  !! \since 2.0
  interface kim_to_model_create
    module procedure kim_model_extension_to_model_create
  end interface kim_to_model_create

  !> \brief \copybrief KIM::ModelExtension::ModelDestroy
  !!
  !! \sa KIM::ModelExtension::ModelDestroy, KIM_ModelExtension_ToModelDestroy
  !!
  !! \since 2.0
  interface kim_to_model_destroy
    module procedure kim_model_extension_to_model_destroy
  end interface kim_to_model_destroy

  !> \brief \copybrief KIM::ModelExtension::ModelDriverCreate
  !!
  !! \sa KIM::ModelExtension::ModelDriverCreate,
  !! KIM_ModelExtension_ToModelDriverCreate
  !!
  !! \since 2.0
  interface kim_to_model_driver_create
    module procedure kim_model_extension_to_model_driver_create
  end interface kim_to_model_driver_create

  !> \brief \copybrief KIM::ModelExtension::ModelRefresh
  !!
  !! \sa KIM::ModelExtension::ModelRefresh, KIM_ModelExtension_ToModelRefresh
  !!
  !! \since 2.0
  interface kim_to_model_refresh
    module procedure kim_model_extension_to_model_refresh
  end interface kim_to_model_refresh

  !> \brief \copybrief KIM::ModelExtension::ModelWriteParameterizedModel
  !!
  !! \sa KIM::ModelExtension::ModelWriteParameterizedModel,
  !! KIM_ModelExtension_ToModelWriteParameterizedModel
  !!
  !! \since 2.0
  interface kim_to_model_write_parameterized_model
    module procedure kim_model_extension_to_model_write_parameterized_model
  end interface kim_to_model_write_parameterized_model

  !> \brief \copybrief KIM::ModelExtension::ModelComputeArguments
  !!
  !! \sa KIM::ModelExtension::ModelComputeArguments,
  !! KIM_ModelExtension_ToModelComputeArguments
  !!
  !! \since 2.0
  interface kim_to_model_compute_arguments
    module procedure kim_model_extension_to_model_compute_arguments
  end interface kim_to_model_compute_arguments

  !> \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsCreate
  !!
  !! \sa KIM::ModelExtension::ModelComputeArgumentsCreate,
  !! KIM_ModelExtension_ToModelComputeArgumentsCreate
  !!
  !! \since 2.0
  interface kim_to_model_compute_arguments_create
    module procedure kim_model_extension_to_model_compute_arguments_create
  end interface kim_to_model_compute_arguments_create

  !> \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsDestroy
  !!
  !! \sa KIM::ModelExtension::ModelComputeArgumentsDestroy,
  !! KIM_ModelExtension_ToModelComputeArgumentsDestroy
  !!
  !! \since 2.0
  interface kim_to_model_compute_arguments_destroy
    module procedure kim_model_extension_to_model_compute_arguments_destroy
  end interface kim_to_model_compute_arguments_destroy

  !> \brief Copy C character array to Fortran string
  !!
  !! \since 2.0
  interface kim_c_char_array_to_string
    module procedure kim_model_extension_convert_c_char_array_to_string
  end interface kim_c_char_array_to_string

  !> \brief Copy C character pointer to Fortran string
  !!
  !! \since 2.0
  interface kim_c_char_ptr_to_string
    module procedure kim_model_extension_convert_c_char_ptr_to_string
  end interface kim_c_char_ptr_to_string

  !> \brief Convert Fortran string to C character array
  !!
  !! \since 2.0
  interface kim_string_to_c_char_array
    module procedure kim_model_extension_convert_string_to_c_char_array
  end interface kim_string_to_c_char_array

  !> \brief \copybrief KIM::ModelExtension::GetModelBufferPointer
  !!
  !! \sa KIM::ModelExtension::GetModelBufferPointer,
  !! KIM_ModelExtension_GetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_get_model_buffer_pointer
    module procedure kim_model_extension_get_model_buffer_pointer
  end interface kim_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelExtension::LogEntry
  !!
  !! \sa KIM::ModelExtension::LogEntry, KIM_ModelExtension_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_model_extension_log_entry
  end interface kim_log_entry

  !> \brief \copybrief KIM::ModelExtension::ToString
  !!
  !! \sa KIM::ModelExtension::ToString, KIM_ModelExtension_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_extension_to_string
  end interface kim_to_string

contains
  !> \brief Compares kim_model_extension_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_extension_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: lhs
    type(kim_model_extension_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_extension_handle_equal = .true.
    else
      kim_model_extension_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_extension_handle_equal

  !> \brief Compares kim_model_extension_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_extension_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: lhs
    type(kim_model_extension_handle_type), intent(in) :: rhs

    kim_model_extension_handle_not_equal = .not. (lhs == rhs)
  end function kim_model_extension_handle_not_equal

  !> \brief \copybrief KIM::ModelExtension::GetExtensionID
  !!
  !! \sa KIM::ModelExtension::GetExtensionID, KIM_ModelExtension_GetExtensionID
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_get_extension_id( &
    model_extension_handle, extension_id)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_extension_type
    implicit none
    interface
      recursive subroutine get_extension_id(model_extension, extension_id) &
        bind(c, name="KIM_ModelExtension_GetExtensionID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
        type(c_ptr), intent(out) :: extension_id
      end subroutine get_extension_id
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    character(len=*, kind=c_char), intent(out) :: extension_id
    type(kim_model_extension_type), pointer :: model_extension

    type(c_ptr) :: p

    call c_f_pointer(model_extension_handle%p, model_extension)
    call get_extension_id(model_extension, p)
    call kim_convert_c_char_ptr_to_string(p, extension_id)
  end subroutine kim_model_extension_get_extension_id

  !> \brief \copybrief KIM::ModelExtension::Model
  !!
  !! \sa KIM::ModelExtension::Model, KIM_ModelExtension_ToModel
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model(model_extension_handle, &
                                                    model_handle)
    use kim_model_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_handle_type), intent(out) :: model_handle

    model_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model

  !> \brief \copybrief KIM::ModelExtension::ModelCompute
  !!
  !! \sa KIM::ModelExtension::ModelCompute, KIM_ModelExtension_ToModelCompute
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_compute( &
    model_extension_handle, model_compute_handle)
    use kim_model_compute_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_compute_handle_type), intent(out) :: model_compute_handle

    model_compute_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_compute

  !> \brief \copybrief KIM::ModelExtension::ModelCompute
  !!
  !! \sa KIM::ModelExtension::ModelCompute, KIM_ModelExtension_ToModelCompute
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_create( &
    model_extension_handle, model_create_handle)
    use kim_model_create_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_create_handle_type), intent(out) :: model_create_handle

    model_create_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_create

  !> \brief \copybrief KIM::ModelExtension::ModelDestroy
  !!
  !! \sa KIM::ModelExtension::ModelDestroy, KIM_ModelExtension_ToModelDestroy
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_destroy( &
    model_extension_handle, model_destroy_handle)
    use kim_model_destroy_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_destroy_handle_type), intent(out) :: model_destroy_handle

    model_destroy_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_destroy

  !> \brief \copybrief KIM::ModelExtension::ModelDriverCreate
  !!
  !! \sa KIM::ModelExtension::ModelDriverCreate,
  !! KIM_ModelExtension_ToModelDriverCreate
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_driver_create( &
    model_extension_handle, model_driver_create_handle)
    use kim_model_driver_create_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_driver_create_handle_type), intent(out) &
      :: model_driver_create_handle

    model_driver_create_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_driver_create

  !> \brief \copybrief KIM::ModelExtension::ModelRefresh
  !!
  !! \sa KIM::ModelExtension::ModelRefresh, KIM_ModelExtension_ToModelRefresh
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_refresh( &
    model_extension_handle, model_refresh_handle)
    use kim_model_refresh_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_refresh_handle_type), intent(out) :: model_refresh_handle

    model_refresh_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_refresh

  !> \brief \copybrief KIM::ModelExtension::ModelWriteParameterizedModel
  !!
  !! \sa KIM::ModelExtension::ModelWriteParameterizedModel,
  !! KIM_ModelExtension_ToModelWriteParameterizedModel
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_write_parameterized_model( &
    model_extension_handle, model_write_parameterized_model_handle)
    use kim_model_write_parameterized_model_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_write_parameterized_model_handle_type), intent(out) &
      :: model_write_parameterized_model_handle

    model_write_parameterized_model_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_write_parameterized_model

  !> \brief \copybrief KIM::ModelExtension::ModelComputeArguments
  !!
  !! \sa KIM::ModelExtension::ModelComputeArguments,
  !! KIM_ModelExtension_ToModelComputeArguments
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_compute_arguments( &
    model_extension_handle, compute_arguments_c_ptr, &
    model_compute_arguments_handle)
    use kim_model_compute_arguments_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(in) :: compute_arguments_c_ptr
    type(kim_model_compute_arguments_handle_type), intent(out) &
      :: model_compute_arguments_handle

    ! avoid unused dummy argument warnings
    if (model_extension_handle == KIM_MODEL_EXTENSION_NULL_HANDLE) continue

    model_compute_arguments_handle%p = compute_arguments_c_ptr
  end subroutine kim_model_extension_to_model_compute_arguments

  !> \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsCreate
  !!
  !! \sa KIM::ModelExtension::ModelComputeArgumentsCreate,
  !! KIM_ModelExtension_ToModelComputeArgumentsCreate
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_compute_arguments_create( &
    model_extension_handle, compute_arguments_c_ptr, &
    model_compute_arguments_create_handle)
    use kim_model_compute_arguments_create_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(in) :: compute_arguments_c_ptr
    type(kim_model_compute_arguments_create_handle_type), intent(out) &
      :: model_compute_arguments_create_handle

    ! avoid unused dummy argument warnings
    if (model_extension_handle == KIM_MODEL_EXTENSION_NULL_HANDLE) continue

    model_compute_arguments_create_handle%p = compute_arguments_c_ptr
  end subroutine kim_model_extension_to_model_compute_arguments_create

  !> \brief \copybrief KIM::ModelExtension::ModelComputeArgumentsDestroy
  !!
  !! \sa KIM::ModelExtension::ModelComputeArgumentsDestroy,
  !! KIM_ModelExtension_ToModelComputeArgumentsDestroy
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_model_compute_arguments_destroy( &
    model_extension_handle, compute_arguments_c_ptr, &
    model_compute_arguments_destroy_handle)
    use kim_model_compute_arguments_destroy_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(in) :: compute_arguments_c_ptr
    type(kim_model_compute_arguments_destroy_handle_type), intent(out) &
      :: model_compute_arguments_destroy_handle

    ! avoid unused dummy argument warnings
    if (model_extension_handle == KIM_MODEL_EXTENSION_NULL_HANDLE) continue

    model_compute_arguments_destroy_handle%p = compute_arguments_c_ptr
  end subroutine kim_model_extension_to_model_compute_arguments_destroy

  !> \brief Copy C character array to Fortran string
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_convert_c_char_array_to_string( &
    c_char_array, string)
    use kim_convert_string_module, only: kim_convert_c_char_array_to_string
    implicit none
    character(len=1, kind=c_char), intent(in) :: c_char_array(:)
    character(len=*, kind=c_char), intent(out) :: string

    call kim_convert_c_char_array_to_string(c_char_array, string)
  end subroutine kim_model_extension_convert_c_char_array_to_string

  !> \brief Copy C character pointer to Fortran string
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_convert_c_char_ptr_to_string( &
    c_char_ptr, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    type(c_ptr), intent(in) :: c_char_ptr
    character(len=*, kind=c_char), intent(out) :: string

    call kim_convert_c_char_ptr_to_string(c_char_ptr, string)
  end subroutine kim_model_extension_convert_c_char_ptr_to_string

  !> \brief Convert Fortran string to C character array
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_convert_string_to_c_char_array( &
    string, c_char_array)
    use kim_convert_string_module, only: kim_convert_string_to_c_char_array
    implicit none
    character(len=*, kind=c_char), intent(in) :: string
    character(len=1, kind=c_char), intent(out) :: c_char_array(:)

    call kim_convert_string_to_c_char_array(string, c_char_array)
  end subroutine kim_model_extension_convert_string_to_c_char_array

  !> \brief \copybrief KIM::ModelExtension::GetModelBufferPointer
  !!
  !! \sa KIM::ModelExtension::GetModelBufferPointer,
  !! KIM_ModelExtension_GetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_get_model_buffer_pointer( &
    model_extension_handle, ptr)
    use kim_interoperable_types_module, only: kim_model_extension_type
    implicit none
    interface
      recursive subroutine get_model_buffer_pointer(model_extension, ptr) &
        bind(c, name="KIM_ModelExtension_GetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
        type(c_ptr), intent(out) :: ptr
      end subroutine get_model_buffer_pointer
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_extension_type), pointer :: model_extension

    call c_f_pointer(model_extension_handle%p, model_extension)
    call get_model_buffer_pointer(model_extension, ptr)
  end subroutine kim_model_extension_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelExtension::LogEntry
  !!
  !! \sa KIM::ModelExtension::LogEntry, KIM_ModelExtension_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_log_entry(model_extension_handle, &
                                                     log_verbosity, message)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_model_extension_type
    implicit none
    interface
      recursive subroutine log_entry( &
        model_extension, log_verbosity, message, line_number, file_name) &
        bind(c, name="KIM_ModelExtension_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_extension_type), pointer :: model_extension

    call c_f_pointer(model_extension_handle%p, model_extension)
    call log_entry(model_extension, log_verbosity, trim(message)//c_null_char, &
                   0, ""//c_null_char)
  end subroutine kim_model_extension_log_entry

  !> \brief \copybrief KIM::ModelExtension::ToString
  !!
  !! \sa KIM::ModelExtension::ToString, KIM_ModelExtension_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_extension_to_string(model_extension_handle, &
                                                     string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_extension_type
    implicit none
    interface
      type(c_ptr) recursive function model_extension_string(model_extension) &
        bind(c, name="KIM_ModelExtension_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
      end function model_extension_string
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_extension_type), pointer :: model_extension

    type(c_ptr) :: p

    call c_f_pointer(model_extension_handle%p, model_extension)
    p = model_extension_string(model_extension)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_extension_to_string
end module kim_model_extension_module
