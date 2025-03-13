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

!> \brief \copybrief KIM::SupportStatus
!!
!! \sa KIM::SupportStatus, KIM_SupportStatus
!!
!! \since 2.0
module kim_support_status_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_support_status_type, &
    ! Constants
    KIM_SUPPORT_STATUS_REQUIRED_BY_API, &
    KIM_SUPPORT_STATUS_NOT_SUPPORTED, &
    KIM_SUPPORT_STATUS_REQUIRED, &
    KIM_SUPPORT_STATUS_OPTIONAL, &
    ! Routines
    kim_known, &
    operator(.eq.), &
    operator(.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_support_statuses, &
    kim_get_support_status

  !> \brief \copybrief KIM::SupportStatus
  !!
  !! \sa KIM::SupportStatus, KIM_SupportStatus
  !!
  !! \since 2.0
  type, bind(c) :: kim_support_status_type
    !> \brief \copybrief KIM::SupportStatus::supportStatusID
    !!
    !! \sa KIM::SupportStatus::supportStatusID,
    !! KIM_SupportStatus::supportStatusID
    !!
    !! \since 2.0
    integer(c_int) :: support_status_id
  end type kim_support_status_type

  !> \brief \copybrief KIM::SUPPORT_STATUS::requiredByAPI
  !!
  !! \sa KIM::SUPPORT_STATUS::requiredByAPI, KIM_SUPPORT_STATUS_requiredByAPI
  !!
  !! \since 2.0
  type(kim_support_status_type), protected, save, &
    bind(c, name="KIM_SUPPORT_STATUS_requiredByAPI") &
    :: KIM_SUPPORT_STATUS_REQUIRED_BY_API

  !> \brief \copybrief KIM::SUPPORT_STATUS::notSupported
  !!
  !! \sa KIM::SUPPORT_STATUS::notSupported, KIM_SUPPORT_STATUS_notSupported
  !!
  !! \since 2.0
  type(kim_support_status_type), protected, save, &
    bind(c, name="KIM_SUPPORT_STATUS_notSupported") &
    :: KIM_SUPPORT_STATUS_NOT_SUPPORTED

  !> \brief \copybrief KIM::SUPPORT_STATUS::required
  !!
  !! \sa KIM::SUPPORT_STATUS::required, KIM_SUPPORT_STATUS_required
  !!
  !! \since 2.0
  type(kim_support_status_type), protected, save, &
    bind(c, name="KIM_SUPPORT_STATUS_required") &
    :: KIM_SUPPORT_STATUS_REQUIRED

  !> \brief \copybrief KIM::SUPPORT_STATUS::optional
  !!
  !! \sa KIM::SUPPORT_STATUS::optional, KIM_SUPPORT_STATUS_optional
  !!
  !! \since 2.0
  type(kim_support_status_type), protected, save, &
    bind(c, name="KIM_SUPPORT_STATUS_optional") &
    :: KIM_SUPPORT_STATUS_OPTIONAL

  !> \brief \copybrief KIM::SupportStatus::Known
  !!
  !! \sa KIM::SupportStatus::Known, KIM_SupportStatus_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_support_status_known
  end interface kim_known

  !> \brief \copybrief KIM::SupportStatus::operator==()
  !!
  !! \sa KIM::SupportStatus::operator==(), KIM_SupportStatus_Equal
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_support_status_equal
  end interface operator(.eq.)

  !> \brief \copybrief KIM::SupportStatus::operator!=()
  !!
  !! \sa KIM::SupportStatus::operator!=(), KIM_SupportStatus_NotEqual
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_support_status_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::SupportStatus::SupportStatus(std::string const &)
  !!
  !! \sa KIM::SupportStatus::SupportStatus(std::string const &),
  !! KIM_SupportStatus_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_support_status_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::SupportStatus::ToString
  !!
  !! \sa KIM::SupportStatus::ToString, KIM_SupportStatus_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_support_status_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::SupportStatus::Known
  !!
  !! \sa KIM::SupportStatus::Known, KIM_SupportStatus_Known
  !!
  !! \since 2.0
  logical recursive function kim_support_status_known(support_status)
    implicit none
    interface
      integer(c_int) recursive function known(support_status) &
        bind(c, name="KIM_SupportStatus_Known")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        type(kim_support_status_type), intent(in), value :: support_status
      end function known
    end interface
    type(kim_support_status_type), intent(in) :: support_status

    kim_support_status_known = (known(support_status) /= 0)
  end function kim_support_status_known

  !> \brief \copybrief KIM::SupportStatus::operator==()
  !!
  !! \sa KIM::SupportStatus::operator==(), KIM_SupportStatus_Equal
  !!
  !! \since 2.0
  logical recursive function kim_support_status_equal(lhs, rhs)
    implicit none
    type(kim_support_status_type), intent(in) :: lhs
    type(kim_support_status_type), intent(in) :: rhs

    kim_support_status_equal &
      = (lhs%support_status_id == rhs%support_status_id)
  end function kim_support_status_equal

  !> \brief \copybrief KIM::SupportStatus::operator!=()
  !!
  !! \sa KIM::SupportStatus::operator!=(), KIM_SupportStatus_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_support_status_not_equal(lhs, rhs)
    implicit none
    type(kim_support_status_type), intent(in) :: lhs
    type(kim_support_status_type), intent(in) :: rhs

    kim_support_status_not_equal = .not. (lhs == rhs)
  end function kim_support_status_not_equal

  !> \brief \copybrief KIM::SupportStatus::SupportStatus(std::string const &)
  !!
  !! \sa KIM::SupportStatus::SupportStatus(std::string const &),
  !! KIM_SupportStatus_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_support_status_from_string(string, support_status)
    implicit none
    interface
      type(kim_support_status_type) recursive function from_string(string) &
        bind(c, name="KIM_SupportStatus_FromString")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_support_status_type), intent(out) :: support_status

    support_status = from_string(trim(string)//c_null_char)
  end subroutine kim_support_status_from_string

  !> \brief \copybrief KIM::SupportStatus::ToString
  !!
  !! \sa KIM::SupportStatus::ToString, KIM_SupportStatus_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_support_status_to_string(support_status, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(support_status) &
        bind(c, name="KIM_SupportStatus_ToString")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        type(kim_support_status_type), intent(in), value :: support_status
      end function get_string
    end interface
    type(kim_support_status_type), intent(in) :: support_status
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(support_status)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_support_status_to_string

  !> \brief \copybrief KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses
  !!
  !! \sa KIM::SUPPORT_STATUS::GetNumberOfSupportStatuses,
  !! KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_support_statuses( &
    number_of_support_statuses)
    implicit none
    interface
      recursive subroutine get_number_of_support_statuses( &
        number_of_support_statuses) &
        bind(c, name="KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_support_statuses
      end subroutine get_number_of_support_statuses
    end interface
    integer(c_int), intent(out) :: number_of_support_statuses

    call get_number_of_support_statuses(number_of_support_statuses)
  end subroutine kim_get_number_of_support_statuses

  !> \brief \copybrief KIM::SUPPORT_STATUS::GetSupportStatus
  !!
  !! \sa KIM::SUPPORT_STATUS::GetSupportStatus,
  !! KIM_SUPPORT_STATUS_GetSupportStatus
  !!
  !! \since 2.0
  recursive subroutine kim_get_support_status(index, support_status, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_support_status(index, &
                                                           support_status) &
        bind(c, name="KIM_SUPPORT_STATUS_GetSupportStatus")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_support_status_type), intent(out) :: support_status
      end function get_support_status
    end interface
    integer(c_int), intent(in) :: index
    type(kim_support_status_type), intent(out) :: support_status
    integer(c_int), intent(out) :: ierr

    ierr = get_support_status(index - 1, support_status)
  end subroutine kim_get_support_status
end module kim_support_status_module
