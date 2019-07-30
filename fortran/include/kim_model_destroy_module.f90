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
! Release: This file is part of the kim-api-2.1.2 package.
!


!> \brief \copybrief KIM::ModelDestroy
!!
!! \sa KIM::ModelDestroy, KIM_ModelDestroy
!!
!! \since 2.0
module kim_model_destroy_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Destroy types
    kim_model_destroy_handle_type, &

    ! Constants
    KIM_MODEL_DESTROY_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_get_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string


  !> \brief \copybrief KIM::ModelDestroy
  !!
  !! \sa KIM::ModelDestroy, KIM_ModelDestroy
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_destroy_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_destroy_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_destroy_handle_type), protected, save &
    :: KIM_MODEL_DESTROY_NULL_HANDLE

  !> \brief Compares kim_model_destroy_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator (.eq.)
    module procedure kim_model_destroy_handle_equal
  end interface operator (.eq.)

  !> \brief Compares kim_model_destroy_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_model_destroy_handle_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::ModelDestroy::GetModelBufferPointer
  !!
  !! \sa KIM::ModelDestroy::GetModelBufferPointer,
  !! KIM_ModelDestroy_GetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_get_model_buffer_pointer
    module procedure kim_model_destroy_get_model_buffer_pointer
  end interface kim_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelDestroy::LogEntry
  !!
  !! \sa KIM::ModelDestroy::LogEntry, KIM_ModelDestroy_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_model_destroy_log_entry
  end interface kim_log_entry

  !> \brief \copybrief KIM::ModelDestroy::ToString
  !!
  !! \sa KIM::ModelDestroy::ToString, KIM_ModelDestroy_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_destroy_to_string
  end interface kim_to_string

contains
  !> \brief Compares kim_model_destroy_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_destroy_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_destroy_handle_type), intent(in) :: lhs
    type(kim_model_destroy_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_destroy_handle_equal = .true.
    else
      kim_model_destroy_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_destroy_handle_equal

  !> \brief Compares kim_model_destroy_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_destroy_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_destroy_handle_type), intent(in) :: lhs
    type(kim_model_destroy_handle_type), intent(in) :: rhs

    kim_model_destroy_handle_not_equal = .not. (lhs .eq. rhs)
  end function kim_model_destroy_handle_not_equal

  !> \brief \copybrief KIM::ModelDestroy::GetModelBufferPointer
  !!
  !! \sa KIM::ModelDestroy::GetModelBufferPointer,
  !! KIM_ModelDestroy_GetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_destroy_get_model_buffer_pointer( &
    model_destroy_handle, ptr)
    use kim_interoperable_types_module, only : kim_model_destroy_type
    implicit none
    interface
      recursive subroutine get_model_buffer_pointer(model_destroy, ptr) &
        bind(c, name="KIM_ModelDestroy_GetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_destroy_type
        implicit none
        type(kim_model_destroy_type), intent(in) :: model_destroy
        type(c_ptr), intent(out) :: ptr
      end subroutine get_model_buffer_pointer
    end interface
    type(kim_model_destroy_handle_type), intent(in) :: model_destroy_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_destroy_type), pointer :: model_destroy

    call c_f_pointer(model_destroy_handle%p, model_destroy)
    call get_model_buffer_pointer(model_destroy, ptr)
  end subroutine kim_model_destroy_get_model_buffer_pointer

  !> \brief \copybrief KIM::ModelDestroy::LogEntry
  !!
  !! \sa KIM::ModelDestroy::LogEntry, KIM_ModelDestroy_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_model_destroy_log_entry(model_destroy_handle, &
    log_verbosity, message)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_model_destroy_type
    implicit none
    interface
      recursive subroutine log_entry(model_destroy, log_verbosity, message, &
        line_number, file_name) bind(c, name="KIM_ModelDestroy_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_model_destroy_type
        implicit none
        type(kim_model_destroy_type), intent(in) :: model_destroy
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_destroy_handle_type), intent(in) :: model_destroy_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_destroy_type), pointer :: model_destroy

    call c_f_pointer(model_destroy_handle%p, model_destroy)
    call log_entry(model_destroy, log_verbosity, trim(message)//c_null_char, &
      0, ""//c_null_char)
  end subroutine kim_model_destroy_log_entry

  !> \brief \copybrief KIM::ModelDestroy::ToString
  !!
  !! \sa KIM::ModelDestroy::ToString, KIM_ModelDestroy_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_destroy_to_string(model_destroy_handle, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_model_destroy_type
    implicit none
    interface
      type(c_ptr) recursive function model_destroy_string(model_destroy) &
        bind(c, name="KIM_ModelDestroy_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_destroy_type
        implicit none
        type(kim_model_destroy_type), intent(in) :: model_destroy
      end function model_destroy_string
    end interface
    type(kim_model_destroy_handle_type), intent(in) :: model_destroy_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_destroy_type), pointer :: model_destroy

    type(c_ptr) :: p

    call c_f_pointer(model_destroy_handle%p, model_destroy)
    p = model_destroy_string(model_destroy)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_destroy_to_string
end module kim_model_destroy_module
