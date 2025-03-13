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

!> \brief \copybrief KIM::TimeUnit
!!
!! \sa KIM::TimeUnit, KIM_TimeUnit
!!
!! \since 2.0
module kim_time_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_time_unit_type, &
    ! Constants
    KIM_TIME_UNIT_UNUSED, &
    KIM_TIME_UNIT_FS, &
    KIM_TIME_UNIT_PS, &
    KIM_TIME_UNIT_NS, &
    KIM_TIME_UNIT_S, &
    ! Routines
    kim_known, &
    operator(.eq.), &
    operator(.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_time_units, &
    kim_get_time_unit

  !> \brief \copybrief KIM::TimeUnit
  !!
  !! \sa KIM::TimeUnit, KIM_TimeUnit
  !!
  !! \since 2.0
  type, bind(c) :: kim_time_unit_type
    !> \brief \copybrief KIM::TimeUnit::timeUnitID
    !!
    !! \sa KIM::TimeUnit::timeUnitID, KIM_TimeUnit::timeUnitID
    !!
    !! \since 2.0
    integer(c_int) time_unit_id
  end type kim_time_unit_type

  !> \brief \copybrief KIM::TIME_UNIT::unused
  !!
  !! \sa KIM::TIME_UNIT::unused, KIM_TIME_UNIT_unused
  !!
  !! \since 2.0
  type(kim_time_unit_type), protected, save, &
    bind(c, name="KIM_TIME_UNIT_unused") &
    :: KIM_TIME_UNIT_UNUSED

  !> \brief \copybrief KIM::TIME_UNIT::fs
  !!
  !! \sa KIM::TIME_UNIT::fs, KIM_TIME_UNIT_fs
  !!
  !! \since 2.0
  type(kim_time_unit_type), protected, save, &
    bind(c, name="KIM_TIME_UNIT_fs") &
    :: KIM_TIME_UNIT_FS

  !> \brief \copybrief KIM::TIME_UNIT::ps
  !!
  !! \sa KIM::TIME_UNIT::ps, KIM_TIME_UNIT_ps
  !!
  !! \since 2.0
  type(kim_time_unit_type), protected, save, &
    bind(c, name="KIM_TIME_UNIT_ps") &
    :: KIM_TIME_UNIT_PS

  !> \brief \copybrief KIM::TIME_UNIT::ns
  !!
  !! \sa KIM::TIME_UNIT::ns, KIM_TIME_UNIT_ns
  !!
  !! \since 2.0
  type(kim_time_unit_type), protected, save, &
    bind(c, name="KIM_TIME_UNIT_ns") &
    :: KIM_TIME_UNIT_NS

  !> \brief \copybrief KIM::TIME_UNIT::s
  !!
  !! \sa KIM::TIME_UNIT::s, KIM_TIME_UNIT_s
  !!
  !! \since 2.0
  type(kim_time_unit_type), protected, save, &
    bind(c, name="KIM_TIME_UNIT_s") &
    :: KIM_TIME_UNIT_S

  !> \brief \copybrief KIM::TimeUnit::Known
  !!
  !! \sa KIM::TimeUnit::Known, KIM_TimeUnit_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_time_unit_known
  end interface kim_known

  !> \brief \copybrief KIM::TimeUnit::operator==()
  !!
  !! \sa KIM::TimeUnit::operator==(), KIM_TimeUnit_Equal
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_time_unit_equal
  end interface operator(.eq.)

  !> \brief \copybrief KIM::TimeUnit::operator!=()
  !!
  !! \sa KIM::TimeUnit::operator!=(), KIM_TimeUnit_NotEqual
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_time_unit_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::TimeUnit::TimeUnit(std::string const &)
  !!
  !! \sa KIM::TimeUnit::TimeUnit(std::string const &), KIM_TimeUnit_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_time_unit_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::TimeUnit::ToString
  !!
  !! \sa KIM::TimeUnit::ToString, KIM_TimeUnit_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_time_unit_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::TimeUnit::Known
  !!
  !! \sa KIM::TimeUnit::Known, KIM_TimeUnit_Known
  !!
  !! \since 2.0
  logical recursive function kim_time_unit_known(time_unit)
    implicit none
    interface
      integer(c_int) recursive function known(time_unit) &
        bind(c, name="KIM_TimeUnit_Known")
        use, intrinsic :: iso_c_binding
        import kim_time_unit_type
        implicit none
        type(kim_time_unit_type), intent(in), value :: time_unit
      end function known
    end interface
    type(kim_time_unit_type), intent(in) :: time_unit

    kim_time_unit_known = (known(time_unit) /= 0)
  end function kim_time_unit_known

  !> \brief \copybrief KIM::TimeUnit::operator==()
  !!
  !! \sa KIM::TimeUnit::operator==(), KIM_TimeUnit_Equal
  !!
  !! \since 2.0
  logical recursive function kim_time_unit_equal(lhs, rhs)
    implicit none
    type(kim_time_unit_type), intent(in) :: lhs
    type(kim_time_unit_type), intent(in) :: rhs

    kim_time_unit_equal = (lhs%time_unit_id == rhs%time_unit_id)
  end function kim_time_unit_equal

  !> \brief \copybrief KIM::TimeUnit::operator!=()
  !!
  !! \sa KIM::TimeUnit::operator!=(), KIM_TimeUnit_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_time_unit_not_equal(lhs, rhs)
    implicit none
    type(kim_time_unit_type), intent(in) :: lhs
    type(kim_time_unit_type), intent(in) :: rhs

    kim_time_unit_not_equal = .not. (lhs == rhs)
  end function kim_time_unit_not_equal

  !> \brief \copybrief KIM::TimeUnit::TimeUnit(std::string const &)
  !!
  !! \sa KIM::TimeUnit::TimeUnit(std::string const &), KIM_TimeUnit_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_time_unit_from_string(string, time_unit)
    implicit none
    interface
      type(kim_time_unit_type) recursive function from_string(string) &
        bind(c, name="KIM_TimeUnit_FromString")
        use, intrinsic :: iso_c_binding
        import kim_time_unit_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_time_unit_type), intent(out) :: time_unit

    time_unit = from_string(trim(string)//c_null_char)
  end subroutine kim_time_unit_from_string

  !> \brief \copybrief KIM::TimeUnit::ToString
  !!
  !! \sa KIM::TimeUnit::ToString, KIM_TimeUnit_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_time_unit_to_string(time_unit, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(time_unit) &
        bind(c, name="KIM_TimeUnit_ToString")
        use, intrinsic :: iso_c_binding
        import kim_time_unit_type
        implicit none
        type(kim_time_unit_type), intent(in), value :: time_unit
      end function get_string
    end interface
    type(kim_time_unit_type), intent(in) :: time_unit
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(time_unit)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_time_unit_to_string

  !> \brief \copybrief KIM::TIME_UNIT::GetNumberOfTimeUnits
  !!
  !! \sa KIM::TIME_UNIT::GetNumberOfTimeUnits,
  !! KIM_TIME_UNIT_GetNumberOfTimeUnits
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_time_units(number_of_time_units)
    implicit none
    interface
      recursive subroutine get_number_of_time_units(number_of_time_units) &
        bind(c, name="KIM_TIME_UNIT_GetNumberOfTimeUnits")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_time_units
      end subroutine get_number_of_time_units
    end interface
    integer(c_int), intent(out) :: number_of_time_units

    call get_number_of_time_units(number_of_time_units)
  end subroutine kim_get_number_of_time_units

  !> \brief \copybrief KIM::TIME_UNIT::GetTimeUnit
  !!
  !! \sa KIM::TIME_UNIT::GetTimeUnit, KIM_TIME_UNIT_GetTimeUnit
  !!
  !! \since 2.0
  recursive subroutine kim_get_time_unit(index, time_unit, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_time_unit(index, time_unit) &
        bind(c, name="KIM_TIME_UNIT_GetTimeUnit")
        use, intrinsic :: iso_c_binding
        import kim_time_unit_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_time_unit_type), intent(out) :: time_unit
      end function get_time_unit
    end interface
    integer(c_int), intent(in) :: index
    type(kim_time_unit_type), intent(out) :: time_unit
    integer(c_int), intent(out) :: ierr

    ierr = get_time_unit(index - 1, time_unit)
  end subroutine kim_get_time_unit
end module kim_time_unit_module
