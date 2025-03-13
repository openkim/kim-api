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
! Release: This file is part of the kim-api.git repository.
!

!> \brief \copybrief KIM::ModelCompute
!!
!! \sa KIM::ModelCompute, KIM_ModelCompute
!!
!! \since 2.0
module kim_model_compute_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_compute_handle_type, &
    ! Constants
    KIM_MODEL_COMPUTE_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
    kim_get_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string

  !> \brief \copybrief KIM::ModelCompute
  !!
  !! \sa KIM::ModelCompute, KIM_ModelCompute
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_compute_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_compute_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_compute_handle_type), protected, save &
    :: KIM_MODEL_COMPUTE_NULL_HANDLE

  !> \brief Compares kim_model_compute_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_model_compute_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_model_compute_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_model_compute_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ModelCompute::GetModelBufferPointer
  !!
  !! \sa KIM::ModelCompute::GetModelBufferPointer,
  !! KIM_ModelCompute_GetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_get_model_buffer_pointer
    module procedure kim_model_compute_get_model_buffer_pointer
  end interface kim_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelCompute::LogEntry
  !!
  !! \sa KIM::ModelCompute::LogEntry, KIM_ModelCompute_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_model_compute_log_entry
  end interface kim_log_entry

  !> \brief \copybrief KIM::ModelCompute::ToString
  !!
  !! \sa KIM::ModelCompute::ToString, KIM_ModelCompute_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_compute_to_string
  end interface kim_to_string

contains
  !> \brief Compares kim_model_compute_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_compute_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_compute_handle_type), intent(in) :: lhs
    type(kim_model_compute_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_compute_handle_equal = .true.
    else
      kim_model_compute_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_compute_handle_equal

  !> \brief Compares kim_model_compute_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_compute_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_compute_handle_type), intent(in) :: lhs
    type(kim_model_compute_handle_type), intent(in) :: rhs

    kim_model_compute_handle_not_equal = .not. (lhs == rhs)
  end function kim_model_compute_handle_not_equal

  !> \brief \copybrief KIM::ModelCompute::GetModelBufferPointer
  !!
  !! \sa KIM::ModelCompute::GetModelBufferPointer,
  !! KIM_ModelCompute_GetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_get_model_buffer_pointer( &
    model_compute_handle, ptr)
    use kim_interoperable_types_module, only: kim_model_compute_type
    implicit none
    interface
      recursive subroutine get_model_buffer_pointer(model_compute, ptr) &
        bind(c, name="KIM_ModelCompute_GetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_compute_type
        implicit none
        type(kim_model_compute_type), intent(in) :: model_compute
        type(c_ptr), intent(out) :: ptr
      end subroutine get_model_buffer_pointer
    end interface
    type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_compute_type), pointer :: model_compute

    call c_f_pointer(model_compute_handle%p, model_compute)
    call get_model_buffer_pointer(model_compute, ptr)
  end subroutine kim_model_compute_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelCompute::LogEntry
  !!
  !! \sa KIM::ModelCompute::LogEntry, KIM_ModelCompute_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_log_entry(model_compute_handle, &
                                                   log_verbosity, message)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_model_compute_type
    implicit none
    interface
      recursive subroutine log_entry( &
        model_compute, log_verbosity, message, line_number, file_name) &
        bind(c, name="KIM_ModelCompute_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_model_compute_type
        implicit none
        type(kim_model_compute_type), intent(in) :: model_compute
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_compute_type), pointer :: model_compute

    call c_f_pointer(model_compute_handle%p, model_compute)
    call log_entry(model_compute, log_verbosity, trim(message)//c_null_char, &
                   0, ""//c_null_char)
  end subroutine kim_model_compute_log_entry

  !> \brief \copybrief KIM::ModelCompute::ToString
  !!
  !! \sa KIM::ModelCompute::ToString, KIM_ModelCompute_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_compute_to_string(model_compute_handle, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_compute_type
    implicit none
    interface
      type(c_ptr) recursive function model_compute_string(model_compute) &
        bind(c, name="KIM_ModelCompute_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_compute_type
        implicit none
        type(kim_model_compute_type), intent(in) :: model_compute
      end function model_compute_string
    end interface
    type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_compute_type), pointer :: model_compute

    type(c_ptr) :: p

    call c_f_pointer(model_compute_handle%p, model_compute)
    p = model_compute_string(model_compute)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_compute_to_string
end module kim_model_compute_module
