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

!> \brief \copybrief KIM::ComputeArguments
!!
!! \sa KIM::ComputeArguments, KIM_ComputeArguments
!!
!! \since 2.0
module kim_compute_arguments_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_compute_arguments_handle_type, &
    ! Constants
    KIM_COMPUTE_ARGUMENTS_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
    kim_get_argument_support_status, &
    kim_get_callback_support_status, &
    kim_set_argument_pointer, &
    kim_set_callback_pointer, &
    kim_are_all_required_present, &
    kim_set_simulator_buffer_pointer, &
    kim_get_simulator_buffer_pointer, &
    kim_to_string, &
    kim_set_log_id, &
    kim_push_log_verbosity, &
    kim_pop_log_verbosity

  !> \brief \copybrief KIM::ComputeArguments
  !!
  !! \sa KIM::ComputeArguments, KIM_ComputeArguments
  !!
  !! \since 2.0
  type, bind(c) :: kim_compute_arguments_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_compute_arguments_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_compute_arguments_handle_type), protected, save &
    :: KIM_COMPUTE_ARGUMENTS_NULL_HANDLE

  !> \brief Compares kim_compute_arguments_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_compute_arguments_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_compute_arguments_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_compute_arguments_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ComputeArguments::GetArgumentSupportStatus
  !!
  !! \sa KIM::ComputeArguments::GetArgumentSupportStatus,
  !! KIM_ComputeArguments_GetArgumentSupportStatus
  !!
  !! \since 2.0
  interface kim_get_argument_support_status
    module procedure kim_compute_arguments_get_argument_support_status
  end interface kim_get_argument_support_status

  !> \brief \copybrief KIM::ComputeArguments::GetCallbackSupportStatus
  !!
  !! \sa KIM::ComputeArguments::GetCallbackSupportStatus,
  !! KIM_ComputeArguments_GetCallbackSupportStatus
  !!
  !! \since 2.0
  interface kim_get_callback_support_status
    module procedure kim_compute_arguments_get_callback_support_status
  end interface kim_get_callback_support_status

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerInteger,
  !! KIM_ComputeArguments_SetArgumentPointerDouble
  !!
  !! \since 2.0
  interface kim_set_argument_pointer
    module procedure kim_compute_arguments_set_argument_pointer_int0
    module procedure kim_compute_arguments_set_argument_pointer_int1
    module procedure kim_compute_arguments_set_argument_pointer_int2
    module procedure kim_compute_arguments_set_argument_pointer_double0
    module procedure kim_compute_arguments_set_argument_pointer_double1
    module procedure kim_compute_arguments_set_argument_pointer_double2
  end interface kim_set_argument_pointer

  !> \brief \copybrief KIM::ComputeArguments::SetCallbackPointer
  !!
  !! \sa KIM::ComputeArguments::SetCallbackPointer,
  !! KIM_ComputeArguments_SetCallbackPointer
  !!
  !! \since 2.0
  interface kim_set_callback_pointer
    module procedure kim_compute_arguments_set_callback_pointer
  end interface kim_set_callback_pointer

  !> \brief \copybrief KIM::ComputeArguments::<!--
  !! -->AreAllRequiredArgumentsAndCallbacksPresent
  !!
  !! \sa KIM::ComputeArguments::AreAllRequiredArgumentsAndCallbacksPresent,
  !! KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent
  !!
  !! \since 2.0
  interface kim_are_all_required_present
    module procedure kim_compute_arguments_are_all_required_present
  end interface kim_are_all_required_present

  !> \brief \copybrief KIM::ComputeArguments::SetSimulatorBufferPointer
  !!
  !! \sa KIM::ComputeArguments::SetSimulatorBufferPointer,
  !! KIM_ComputeArguments_SetSimulatorBufferPointer
  !!
  !! \since 2.0
  interface kim_set_simulator_buffer_pointer
    module procedure kim_compute_arguments_set_simulator_buffer_pointer
  end interface kim_set_simulator_buffer_pointer

  !> \brief \copybrief KIM::ComputeArguments::GetSimulatorBufferPointer
  !!
  !! \sa KIM::ComputeArguments::GetSimulatorBufferPointer,
  !! KIM_ComputeArguments_GetSimulatorBufferPointer
  !!
  !! \since 2.0
  interface kim_get_simulator_buffer_pointer
    module procedure kim_compute_arguments_get_simulator_buffer_pointer
  end interface kim_get_simulator_buffer_pointer

  !> \brief \copybrief KIM::ComputeArguments::ToString
  !!
  !! \sa KIM::ComputeArguments::ToString, KIM_ComputeArguments_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_compute_arguments_to_string
  end interface kim_to_string

  !> \brief \copybrief KIM::ComputeArguments::SetLogID
  !!
  !! \sa KIM::ComputeArguments::SetLogID, KIM_ComputeArguments_SetLogID
  !!
  !! \since 2.0
  interface kim_set_log_id
    module procedure kim_compute_arguments_set_log_id
  end interface kim_set_log_id

  !> \brief \copybrief KIM::ComputeArguments::PushLogVerbosity
  !!
  !! \sa KIM::ComputeArguments::PushLogVerbosity,
  !! KIM_ComputeArguments_PushLogVerbosity
  !!
  !! \since 2.0
  interface kim_push_log_verbosity
    module procedure kim_compute_arguments_push_log_verbosity
  end interface kim_push_log_verbosity

  !> \brief \copybrief KIM::ComputeArguments::PopLogVerbosity
  !!
  !! \sa KIM::ComputeArguments::PopLogVerbosity,
  !! KIM_ComputeArguments_PopLogVerbosity
  !!
  !! \since 2.0
  interface kim_pop_log_verbosity
    module procedure kim_compute_arguments_pop_log_verbosity
  end interface kim_pop_log_verbosity

contains
  !> \brief Compares kim_compute_arguments_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_compute_arguments_handle_equal(lhs, rhs)
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: lhs
    type(kim_compute_arguments_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_compute_arguments_handle_equal = .true.
    else
      kim_compute_arguments_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_compute_arguments_handle_equal

  !> \brief Compares kim_compute_arguments_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_compute_arguments_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: lhs
    type(kim_compute_arguments_handle_type), intent(in) :: rhs

    kim_compute_arguments_handle_not_equal = .not. (lhs == rhs)
  end function kim_compute_arguments_handle_not_equal

  !> \brief \copybrief KIM::ComputeArguments::GetArgumentSupportStatus
  !!
  !! \sa KIM::ComputeArguments::GetArgumentSupportStatus,
  !! KIM_ComputeArguments_GetArgumentSupportStatus
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_get_argument_support_status( &
    compute_arguments_handle, compute_argument_name, &
    support_status, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_support_status_module, only: kim_support_status_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_argument_support_status( &
        compute_arguments, compute_argument_name, support_status) &
        bind(c, name="KIM_ComputeArguments_GetArgumentSupportStatus")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_support_status_module, only: kim_support_status_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(kim_support_status_type), intent(out) :: support_status
      end function get_argument_support_status
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    type(kim_support_status_type), intent(out) :: support_status
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = get_argument_support_status(compute_arguments, &
                                       compute_argument_name, support_status)
  end subroutine kim_compute_arguments_get_argument_support_status

  !> \brief \copybrief KIM::ComputeArguments::GetCallbackSupportStatus
  !!
  !! \sa KIM::ComputeArguments::GetCallbackSupportStatus,
  !! KIM_ComputeArguments_GetCallbackSupportStatus
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_get_callback_support_status( &
    compute_arguments_handle, compute_callback_name, support_status, ierr)
    use kim_compute_callback_name_module, only: kim_compute_callback_name_type
    use kim_support_status_module, only: kim_support_status_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function get_callback_support_status( &
        compute_arguments, compute_callback_name, support_status) &
        bind(c, name="KIM_ComputeArguments_GetCallbackSupportStatus")
        use, intrinsic :: iso_c_binding
        use kim_compute_callback_name_module, only: &
          kim_compute_callback_name_type
        use kim_support_status_module, only: kim_support_status_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_callback_name_type), intent(in), value :: &
          compute_callback_name
        type(kim_support_status_type), intent(out) :: support_status
      end function get_callback_support_status
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_callback_name_type), intent(in) :: &
      compute_callback_name
    type(kim_support_status_type), intent(out) :: support_status
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = get_callback_support_status(compute_arguments, &
                                       compute_callback_name, support_status)
  end subroutine kim_compute_arguments_get_callback_support_status

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_argument_pointer_int0( &
    compute_arguments_handle, compute_argument_name, int0, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function set_argument_pointer_integer( &
        compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(in), value :: ptr
      end function set_argument_pointer_integer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in), target :: int0
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = set_argument_pointer_integer(compute_arguments, &
                                        compute_argument_name, c_loc(int0))
  end subroutine kim_compute_arguments_set_argument_pointer_int0

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_argument_pointer_int1( &
    compute_arguments_handle, compute_argument_name, int1, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in), target :: int1(:)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, size(int1, 1, c_int), &
             int1, ierr)
    return

  contains
    recursive subroutine set(compute_arguments, compute_argument_name, &
                             extent1, int1, ierr)
      use kim_compute_argument_name_module, only: &
        kim_compute_argument_name_type
      use kim_interoperable_types_module, only: kim_compute_arguments_type
      implicit none
      interface
        integer(c_int) recursive function set_argument_pointer_integer( &
          compute_arguments, compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only: &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only: kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_integer
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
        compute_argument_name
      integer(c_int), intent(in) :: extent1
      integer(c_int), intent(in), target :: int1(extent1)
      integer(c_int), intent(out) :: ierr

      ierr = set_argument_pointer_integer(compute_arguments, &
                                          compute_argument_name, c_loc(int1))
    end subroutine set
  end subroutine kim_compute_arguments_set_argument_pointer_int1

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_argument_pointer_int2( &
    compute_arguments_handle, compute_argument_name, int2, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in), target :: int2(:, :)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, size(int2, 1, c_int), &
             size(int2, 2, c_int), int2, ierr)
    return

  contains
    recursive subroutine set(compute_arguments, compute_argument_name, &
                             extent1, extent2, int2, ierr)
      use kim_compute_argument_name_module, only: &
        kim_compute_argument_name_type
      use kim_interoperable_types_module, only: kim_compute_arguments_type
      implicit none
      interface
        integer(c_int) recursive function set_argument_pointer_integer( &
          compute_arguments, compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only: &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only: kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_integer
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
        compute_argument_name
      integer(c_int), intent(in) :: extent1
      integer(c_int), intent(in) :: extent2
      integer(c_int), intent(in), target :: int2(extent1, extent2)
      integer(c_int), intent(out) :: ierr

      ierr = set_argument_pointer_integer(compute_arguments, &
                                          compute_argument_name, c_loc(int2))
    end subroutine set
  end subroutine kim_compute_arguments_set_argument_pointer_int2

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerDouble
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_argument_pointer_double0( &
    compute_arguments_handle, compute_argument_name, double0, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function set_argument_pointer_double( &
        compute_arguments, compute_argument_name, ptr) &
        bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only: &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(in), value :: ptr
      end function set_argument_pointer_double
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(in), target :: double0
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = set_argument_pointer_double(compute_arguments, &
                                       compute_argument_name, c_loc(double0))
  end subroutine kim_compute_arguments_set_argument_pointer_double0

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerDouble
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_argument_pointer_double1( &
    compute_arguments_handle, compute_argument_name, double1, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(in), target :: double1(:)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, &
             size(double1, 1, c_int), double1, ierr)
    return

  contains
    recursive subroutine set(compute_arguments, compute_argument_name, &
                             extent1, double1, ierr)
      use kim_compute_argument_name_module, only: &
        kim_compute_argument_name_type
      use kim_interoperable_types_module, only: kim_compute_arguments_type
      implicit none
      interface
        integer(c_int) recursive function set_argument_pointer_double( &
          compute_arguments, compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only: &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only: kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_double
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
        compute_argument_name
      integer(c_int), intent(in) :: extent1
      real(c_double), intent(in), target :: double1(extent1)
      integer(c_int), intent(out) :: ierr

      ierr = set_argument_pointer_double(compute_arguments, &
                                         compute_argument_name, c_loc(double1))
    end subroutine set
  end subroutine kim_compute_arguments_set_argument_pointer_double1

  !> \brief \copybrief KIM::ComputeArguments::SetArgumentPointer
  !!
  !! \sa KIM::ComputeArguments::SetArgumentPointer,
  !! KIM_ComputeArguments_SetArgumentPointerDouble
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_argument_pointer_double2( &
    compute_arguments_handle, compute_argument_name, double2, ierr)
    use kim_compute_argument_name_module, only: kim_compute_argument_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(in), target :: double2(:, :)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, &
             size(double2, 1, c_int), size(double2, 2, c_int), double2, ierr)
    return

  contains
    recursive subroutine set(compute_arguments, compute_argument_name, &
                             extent1, extent2, double2, ierr)
      use kim_compute_argument_name_module, only: &
        kim_compute_argument_name_type
      implicit none
      interface
        integer(c_int) recursive function set_argument_pointer_double( &
          compute_arguments, compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only: &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only: kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_double
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
        compute_argument_name
      integer(c_int), intent(in) :: extent1
      integer(c_int), intent(in) :: extent2
      real(c_double), intent(in), target :: double2(extent1, extent2)
      integer(c_int), intent(out) :: ierr

      ierr = set_argument_pointer_double(compute_arguments, &
                                         compute_argument_name, c_loc(double2))
    end subroutine set
  end subroutine kim_compute_arguments_set_argument_pointer_double2

  !> \brief \copybrief KIM::ComputeArguments::SetCallbackPointer
  !!
  !! \sa KIM::ComputeArguments::SetCallbackPointer,
  !! KIM_ComputeArguments_SetCallbackPointer
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_callback_pointer( &
    compute_arguments_handle, compute_callback_name, language_name, fptr, &
    data_object, ierr)
    use kim_compute_callback_name_module, only: kim_compute_callback_name_type
    use kim_language_name_module, only: kim_language_name_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function set_callback_pointer( &
        compute_arguments, compute_callback_name, language_name, fptr, &
        data_object) bind(c, name="KIM_ComputeArguments_SetCallbackPointer")
        use, intrinsic :: iso_c_binding
        use kim_language_name_module, only: kim_language_name_type
        use kim_compute_callback_name_module, only: &
          kim_compute_callback_name_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_language_name_type), intent(in), value :: language_name
        type(kim_compute_callback_name_type), intent(in), value :: &
          compute_callback_name
        type(c_funptr), intent(in), value :: fptr
        type(c_ptr), intent(in), value :: data_object
      end function set_callback_pointer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_callback_name_type), intent(in) :: &
      compute_callback_name
    type(kim_language_name_type), intent(in) :: language_name
    type(c_funptr), intent(in), value :: fptr  ! must be left as "value"!?!
    type(c_ptr), intent(in) :: data_object
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = set_callback_pointer(compute_arguments, compute_callback_name, &
                                language_name, fptr, data_object)
  end subroutine kim_compute_arguments_set_callback_pointer

  !> \brief \copybrief KIM::ComputeArguments::<!--
  !! -->AreAllRequiredArgumentsAndCallbacksPresent
  !!
  !! \sa KIM::ComputeArguments::AreAllRequiredArgumentsAndCallbacksPresent,
  !! KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_are_all_required_present( &
    compute_arguments_handle, result_value, ierr)
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) recursive function &
        are_all_required_arguments_and_callbacks_present( &
        compute_arguments, result_value) &
        bind(c, &
             name= &
             "KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        integer(c_int), intent(out) :: result_value
      end function are_all_required_arguments_and_callbacks_present
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    integer(c_int), intent(out) :: result_value
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = are_all_required_arguments_and_callbacks_present( &
           compute_arguments, result_value)
  end subroutine kim_compute_arguments_are_all_required_present

  !> \brief \copybrief KIM::ComputeArguments::SetSimulatorBufferPointer
  !!
  !! \sa KIM::ComputeArguments::SetSimulatorBufferPointer,
  !! KIM_ComputeArguments_SetSimulatorBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_simulator_buffer_pointer( &
    compute_arguments_handle, ptr)
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      recursive subroutine set_simulator_buffer_pointer( &
        compute_arguments, ptr) &
        bind(c, name="KIM_ComputeArguments_SetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_simulator_buffer_pointer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set_simulator_buffer_pointer(compute_arguments, ptr)
  end subroutine kim_compute_arguments_set_simulator_buffer_pointer

  !> \brief \copybrief KIM::ComputeArguments::GetSimulatorBufferPointer
  !!
  !! \sa KIM::ComputeArguments::GetSimulatorBufferPointer,
  !! KIM_ComputeArguments_GetSimulatorBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_get_simulator_buffer_pointer( &
    compute_arguments_handle, ptr)
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      recursive subroutine get_simulator_buffer_pointer( &
        compute_arguments, ptr) &
        bind(c, name="KIM_ComputeArguments_GetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(c_ptr), intent(out) :: ptr
      end subroutine get_simulator_buffer_pointer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call get_simulator_buffer_pointer(compute_arguments, ptr)
  end subroutine kim_compute_arguments_get_simulator_buffer_pointer

  !> \brief \copybrief KIM::ComputeArguments::ToString
  !!
  !! \sa KIM::ComputeArguments::ToString, KIM_ComputeArguments_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_to_string( &
    compute_arguments_handle, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      type(c_ptr) recursive function compute_arguments_string( &
        compute_arguments) bind(c, name="KIM_ComputeArguments_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
      end function compute_arguments_string
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_compute_arguments_type), pointer :: compute_arguments

    type(c_ptr) :: p

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    p = compute_arguments_string(compute_arguments)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_compute_arguments_to_string

  !> \brief \copybrief KIM::ComputeArguments::SetLogID
  !!
  !! \sa KIM::ComputeArguments::SetLogID, KIM_ComputeArguments_SetLogID
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_set_log_id( &
    compute_arguments_handle, log_id)
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      recursive subroutine set_log_id(compute_arguments, log_id) &
        bind(c, name="KIM_ComputeArguments_SetLogID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        character(c_char), intent(in) :: log_id(*)
      end subroutine set_log_id
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    character(len=*, kind=c_char), intent(in) :: log_id
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set_log_id(compute_arguments, trim(log_id)//c_null_char)
  end subroutine kim_compute_arguments_set_log_id

  !> \brief \copybrief KIM::ComputeArguments::PushLogVerbosity
  !!
  !! \sa KIM::ComputeArguments::PushLogVerbosity,
  !! KIM_ComputeArguments_PushLogVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_push_log_verbosity( &
    compute_arguments_handle, log_verbosity)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      recursive subroutine push_log_verbosity( &
        compute_arguments, log_verbosity) &
        bind(c, name="KIM_ComputeArguments_PushLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_log_verbosity
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call push_log_verbosity(compute_arguments, log_verbosity)
  end subroutine kim_compute_arguments_push_log_verbosity

  !> \brief \copybrief KIM::ComputeArguments::PopLogVerbosity
  !!
  !! \sa KIM::ComputeArguments::PopLogVerbosity,
  !! KIM_ComputeArguments_PopLogVerbosity
  !!
  !! \since 2.0
  recursive subroutine kim_compute_arguments_pop_log_verbosity( &
    compute_arguments_handle)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_compute_arguments_type
    implicit none
    interface
      recursive subroutine pop_log_verbosity(compute_arguments) &
        bind(c, name="KIM_ComputeArguments_PopLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
      end subroutine pop_log_verbosity
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call pop_log_verbosity(compute_arguments)
  end subroutine kim_compute_arguments_pop_log_verbosity
end module kim_compute_arguments_module
