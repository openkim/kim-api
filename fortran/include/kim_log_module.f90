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
! Release: This file is part of the kim-api.git repository.
!


!> \brief \copybrief KIM::Log
!!
!! \sa KIM::Log, KIM_Log
!!
!! \since 2.0
module kim_log_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_log_handle_type, &

    ! Constants
    KIM_LOG_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_log_create, &
    kim_log_destroy, &
    kim_push_default_verbosity, &
    kim_pop_default_verbosity, &
    kim_get_id, &
    kim_set_id, &
    kim_push_verbosity, &
    kim_pop_verbosity, &
    kim_log_entry


  !> \brief \copybrief KIM::Log
  !!
  !! \sa KIM::Log, KIM_Log
  !!
  !! \since 2.0
  type, bind(c) :: kim_log_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_log_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_log_handle_type), protected, save &
    :: KIM_LOG_NULL_HANDLE

  !> \brief Compares kim_log_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator (.eq.)
    module procedure kim_log_handle_equal
  end interface operator (.eq.)

  !> \brief Compares kim_log_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_log_handle_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::Log::PushDefaultVerbosity
  !!
  !! \sa KIM::Log::PushDefaultVerbosity, KIM_Log_PushDefaultVerbosity
  !!
  !! \since 2.0
  interface kim_push_default_verbosity
    module procedure kim_log_push_default_verbosity
  end interface kim_push_default_verbosity

  !> \brief \copybrief KIM::Log::PopDefaultVerbosity
  !!
  !! \sa KIM::Log::PopDefaultVerbosity, KIM_Log_PopDefaultVerbosity
  !!
  !! \since 2.0
  interface kim_pop_default_verbosity
    module procedure kim_log_pop_default_verbosity
  end interface kim_pop_default_verbosity

  !> \brief \copybrief KIM::Log::GetID
  !!
  !! \sa KIM::Log::GetID, KIM_Log_GetID
  !!
  !! \since 2.0
  interface kim_get_id
    module procedure kim_log_get_id
  end interface kim_get_id

  !> \brief \copybrief KIM::Log::SetID
  !!
  !! \sa KIM::Log::SetID, KIM_Log_SetID
  !!
  !! \since 2.0
  interface kim_set_id
    module procedure kim_log_set_id
  end interface kim_set_id

  !> \brief \copybrief KIM::Log::PushVerbosity
  !!
  !! \sa KIM::Log::PushVerbosity, KIM_Log_PushVerbosity
  !!
  !! \since 2.0
  interface kim_push_verbosity
    module procedure kim_log_push_verbosity
  end interface kim_push_verbosity

  !> \brief \copybrief KIM::Log::PopVerbosity
  !!
  !! \sa KIM::Log::PopVerbosity, KIM_Log_PopVerbosity
  !!
  !! \since 2.0
  interface kim_pop_verbosity
    module procedure kim_log_pop_verbosity
  end interface kim_pop_verbosity

  !> \brief \copybrief KIM::Log::LogEntry
  !!
  !! \sa KIM::Log::LogEntry, KIM_Log_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_log_log_entry
  end interface kim_log_entry

contains
  !> \brief Compares kim_log_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_log_handle_equal(lhs, rhs)
    implicit none
    type(kim_log_handle_type), intent(in) :: lhs
    type(kim_log_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_log_handle_equal = .true.
    else
      kim_log_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_log_handle_equal

  !> \brief Compares kim_log_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_log_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_log_handle_type), intent(in) :: lhs
    type(kim_log_handle_type), intent(in) :: rhs

    kim_log_handle_not_equal = .not. (lhs .eq. rhs)
  end function kim_log_handle_not_equal

  !> \brief \copybrief KIM::Log::Create
  !!
  !! \sa KIM::Log::Create, KIM_Log_Create
  !!
  !! \since 2.0
  recursive subroutine kim_log_create(log_handle, ierr)
    implicit none
    interface
      integer(c_int) recursive function create(log) &
        bind(c, name="KIM_Log_Create")
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(out) :: log
      end function create
    end interface
    type(kim_log_handle_type), intent(out) :: log_handle
    integer(c_int), intent(out) :: ierr

    type(c_ptr) :: plog

    ierr = create(plog)
    log_handle%p = plog
  end subroutine kim_log_create

  !> \brief \copybrief KIM::Log::Destroy
  !!
  !! \sa KIM::Log::Destroy, KIM_Log_Destroy
  !!
  !! \since 2.0
  recursive subroutine kim_log_destroy(log_handle)
    implicit none
    interface
      recursive subroutine destroy(log) bind(c, name="KIM_Log_Destroy")
        use, intrinsic :: iso_c_binding
        implicit none
        type(c_ptr), intent(inout) :: log
      end subroutine destroy
    end interface
    type(kim_log_handle_type), intent(inout) :: log_handle

    type(c_ptr) :: plog
    plog = log_handle%p
    call destroy(plog)
    log_handle%p = c_null_ptr
  end subroutine kim_log_destroy

  !> \brief \copybrief KIM::Log::PushDefaultVerbosity
  !!
  !! \sa KIM::Log::PushDefaultVerbosity, KIM_Log_PushDefaultVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_log_push_default_verbosity(log_verbosity)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    implicit none
    interface
      recursive subroutine push_default_verbosity(log_verbosity) &
        bind(c, name="KIM_Log_PushDefaultVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        implicit none
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_default_verbosity
    end interface
    type(kim_log_verbosity_type), intent(in) :: log_verbosity

    call push_default_verbosity(log_verbosity)
  end subroutine kim_log_push_default_verbosity

  !> \brief \copybrief KIM::Log::PopDefaultVerbosity
  !!
  !! \sa KIM::Log::PopDefaultVerbosity, KIM_Log_PopDefaultVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_log_pop_default_verbosity()
    implicit none
    interface
      recursive subroutine pop_default_verbosity() &
        bind(c, name="KIM_Log_PopDefaultVerbosity")
        use, intrinsic :: iso_c_binding
        implicit none
      end subroutine pop_default_verbosity
    end interface

    call pop_default_verbosity()
  end subroutine kim_log_pop_default_verbosity

  !> \brief \copybrief KIM::Log::GetID
  !!
  !! \sa KIM::Log::GetID, KIM_Log_GetID
  !!
  !! \since 2.0
  recursive subroutine kim_log_get_id(log_handle, id_string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_log_type
    implicit none
    interface
      type(c_ptr) recursive function get_id(log) bind(c, name="KIM_Log_GetID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_log_type
        implicit none
        type(kim_log_type), intent(in) :: log
      end function get_id
    end interface
    type(kim_log_handle_type), intent(in) :: log_handle
    character(len=*, kind=c_char), intent(out) :: id_string
    type(kim_log_type), pointer :: log

    type(c_ptr) :: p

    call c_f_pointer(log_handle%p, log)
    p = get_id(log)
    call kim_convert_c_char_ptr_to_string(p, id_string)
  end subroutine kim_log_get_id

  !> \brief \copybrief KIM::Log::SetID
  !!
  !! \sa KIM::Log::SetID, KIM_Log_SetID
  !!
  !! \since 2.0
  recursive subroutine kim_log_set_id(log_handle, id_string)
    use kim_interoperable_types_module, only : kim_log_type
    implicit none
    interface
      recursive subroutine set_id(log, id_string) bind(c, name="KIM_Log_SetID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_log_type
        implicit none
        type(kim_log_type), intent(in) :: log
        character(c_char), intent(in) :: id_string(*)
      end subroutine set_id
    end interface
    type(kim_log_handle_type), intent(in) :: log_handle
    character(len=*, kind=c_char), intent(in) :: id_string
    type(kim_log_type), pointer :: log

    call c_f_pointer(log_handle%p, log)
    call set_id(log, trim(id_string)//c_null_char)
  end subroutine kim_log_set_id

  !> \brief \copybrief KIM::Log::PushVerbosity
  !!
  !! \sa KIM::Log::PushVerbosity, KIM_Log_PushVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_log_push_verbosity(log_handle, log_verbosity)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_log_type
    implicit none
    interface
      recursive subroutine push_verbosity(log, log_verbosity) &
        bind(c, name="KIM_Log_PushVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_log_type
        implicit none
        type(kim_log_type), intent(in) :: log
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_verbosity
    end interface
    type(kim_log_handle_type), intent(in) :: log_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    type(kim_log_type), pointer :: log

    call c_f_pointer(log_handle%p, log)
    call push_verbosity(log, log_verbosity)
  end subroutine kim_log_push_verbosity

  !> \brief \copybrief KIM::Log::PopVerbosity
  !!
  !! \sa KIM::Log::PopVerbosity, KIM_Log_PopVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_log_pop_verbosity(log_handle)
    use kim_interoperable_types_module, only : kim_log_type
    implicit none
    interface
      recursive subroutine pop_verbosity(log) &
        bind(c, name="KIM_Log_PopVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_log_type
        implicit none
        type(kim_log_type), intent(in) :: log
      end subroutine pop_verbosity
    end interface
    type(kim_log_handle_type), intent(in) :: log_handle
    type(kim_log_type), pointer :: log

    call c_f_pointer(log_handle%p, log)
    call pop_verbosity(log)
  end subroutine kim_log_pop_verbosity

  !> \brief \copybrief KIM::Log::LogEntry
  !!
  !! \sa KIM::Log::LogEntry, KIM_Log_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_log_log_entry(log_handle, log_verbosity, message)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_log_type
    implicit none
    interface
      recursive subroutine log_entry(log, log_verbosity, message, line_number, &
        file_name) bind(c, name="KIM_Log_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_log_type
        implicit none
        type(kim_log_type), intent(in) :: log
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_log_handle_type), intent(in) :: log_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_log_type), pointer :: log

    call c_f_pointer(log_handle%p, log)
    call log_entry(log, log_verbosity, trim(message)//c_null_char, &
      0, ""//c_null_char)
  end subroutine kim_log_log_entry
end module kim_log_module
