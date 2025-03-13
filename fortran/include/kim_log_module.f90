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
    operator(.eq.), &
    operator(.ne.), &
    kim_log_create, &
    kim_log_destroy, &
    kim_push_default_verbosity, &
    kim_pop_default_verbosity, &
    kim_push_default_print_function, &
    kim_pop_default_print_function, &
    kim_convert_c_string, &
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
  interface operator(.eq.)
    module procedure kim_log_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_log_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_log_handle_not_equal
  end interface operator(.ne.)

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

  !> \brief \copybrief KIM::Log::PushDefaultPrintFunction
  !!
  !! \sa KIM::Log::PushDefaultPrintFunction, KIM_Log_PushDefaultPrintFunction
  !!
  !! \since 2.2
  interface kim_push_default_print_function
    module procedure kim_log_push_default_print_function
  end interface kim_push_default_print_function

  !> \brief \copybrief KIM::Log::PopDefaultPrintFunction
  !!
  !! \sa KIM::Log::PopDefaultPrintFunction, KIM_Log_PopDefaultPrintFunction
  !!
  !! \since 2.2
  interface kim_pop_default_print_function
    module procedure kim_log_pop_default_print_function
  end interface kim_pop_default_print_function

  !> \brief \copybrief kim_log_module::kim_log_convert_c_string
  !!
  !! \since 2.2
  interface kim_convert_c_string
    module procedure kim_log_convert_c_string
  end interface kim_convert_c_string

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

    kim_log_handle_not_equal = .not. (lhs == rhs)
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
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    implicit none
    interface
      recursive subroutine push_default_verbosity(log_verbosity) &
        bind(c, name="KIM_Log_PushDefaultVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
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

  !> \brief \copybrief KIM::Log::PushDefaultPrintFunction
  !!
  !! A Fortran routine may provide a KIM::Log::PrintFunction routine.  The
  !! interface for this is given here (see also KIM::LogPrintFunction, \ref
  !! KIM_LogPrintFunction).
  !!
  !! \code{.f90}
  !! interface
  !!   recursive subroutine log_print_function(entry_string, ierr) bind(c)
  !!     use, intrinsic :: iso_c_binding
  !!     type(c_ptr), intent(in), value :: entry_string
  !!     integer(c_int), intent(out) :: ierr
  !!   end subroutine log_print_function
  !! end interface
  !! \endcode
  !!
  !! The routine must take a c_ptr pointing to a null terminated c string
  !! representing the log message.  To work with this, the kim_log_module
  !! provides a conversion routine: kim_log_module::kim_convert_c_string.
  !!
  !! An example log print function, which simply writes log messages to stdout,
  !! is given here:
  !!
  !! \code{.f90}
  !! recursive subroutine log_print_function(entry_string, ierr) bind(c)
  !!   use, intrinsic :: iso_c_binding
  !!   use kim_log_module, only : kim_convert_c_string
  !!   type(c_ptr), intent(in), value :: entry_string
  !!   integer(c_int), intent(out) :: ierr
  !!
  !!   character(len=2048, kind=c_char) :: message
  !!
  !!   call kim_convert_c_string(entry_string, message)
  !!   print *, trim(message)
  !!
  !!   ierr = 0
  !!   return
  !! end subroutine log_print_function
  !! \endcode
  !!
  !! \sa KIM::Log::PushDefaultPrintFunction, KIM_Log_PushDefaultPrintFunction
  !!
  !! \since 2.2
  recursive subroutine kim_log_push_default_print_function(language_name, fptr)
    use kim_language_name_module, only: kim_language_name_type
    implicit none
    interface
      recursive subroutine push_default_print_function(language_name, fptr) &
        bind(c, name="KIM_Log_PushDefaultPrintFunction")
        use, intrinsic :: iso_c_binding
        use kim_language_name_module, only: kim_language_name_type
        implicit none
        type(kim_language_name_type), intent(in), value :: language_name
        type(c_funptr), intent(in), value :: fptr
      end subroutine push_default_print_function
    end interface
    type(kim_language_name_type), intent(in) :: language_name
    type(c_funptr), intent(in), value :: fptr  ! must be left as "value"!?!

    call push_default_print_function(language_name, fptr)
  end subroutine kim_log_push_default_print_function

  !> \brief \copybrief KIM::Log::PopDefaultPrintFunction
  !!
  !! \sa KIM::Log::PopDefaultPrintFunction, KIM_Log_PopDefaultPrintFunction
  !!
  !! \since 2.2
  recursive subroutine kim_log_pop_default_print_function()
    implicit none
    interface
      recursive subroutine pop_default_print_function() &
        bind(c, name="KIM_Log_PopDefaultPrintFunction")
        use, intrinsic :: iso_c_binding
        implicit none
      end subroutine pop_default_print_function
    end interface

    call pop_default_print_function()
  end subroutine kim_log_pop_default_print_function

  !> \brief Convert a c sting to a Fortran string
  !!
  !! Convert a c string, given in terms of a char pointer to a Fortran string
  !! variable.
  !!
  !! \param[in]  c_char_ptr A pointer to a null terminated c string
  !! \param[out] string A Fortran string variable
  !!
  !! \since 2.2
  recursive subroutine kim_log_convert_c_string(c_char_ptr, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    type(c_ptr), intent(in), value :: c_char_ptr
    character(len=*, kind=c_char), intent(out) :: string

    call kim_convert_c_char_ptr_to_string(c_char_ptr, string)
  end subroutine kim_log_convert_c_string

  !> \brief \copybrief KIM::Log::GetID
  !!
  !! \sa KIM::Log::GetID, KIM_Log_GetID
  !!
  !! \since 2.0
  recursive subroutine kim_log_get_id(log_handle, id_string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_log_type
    implicit none
    interface
      type(c_ptr) recursive function get_id(log) bind(c, name="KIM_Log_GetID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_log_type
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
    use kim_interoperable_types_module, only: kim_log_type
    implicit none
    interface
      recursive subroutine set_id(log, id_string) bind(c, name="KIM_Log_SetID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_log_type
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
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_log_type
    implicit none
    interface
      recursive subroutine push_verbosity(log, log_verbosity) &
        bind(c, name="KIM_Log_PushVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_log_type
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
    use kim_interoperable_types_module, only: kim_log_type
    implicit none
    interface
      recursive subroutine pop_verbosity(log) &
        bind(c, name="KIM_Log_PopVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_log_type
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
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_log_type
    implicit none
    interface
      recursive subroutine log_entry(log, log_verbosity, message, line_number, &
                                     file_name) bind(c, name="KIM_Log_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_log_type
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
