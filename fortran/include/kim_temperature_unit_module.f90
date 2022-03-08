!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2021, Regents of the University of Minnesota.
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

!> \brief \copybrief KIM::TemperatureUnit
!!
!! \sa KIM::TemperatureUnit, KIM_TemperatureUnit
!!
!! \since 2.0
module kim_temperature_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived type
    kim_temperature_unit_type, &
    ! Constants
    KIM_TEMPERATURE_UNIT_UNUSED, &
    KIM_TEMPERATURE_UNIT_K, &
    ! Routines
    kim_known, &
    operator(.eq.), &
    operator(.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_temperature_units, &
    kim_get_temperature_unit

  !> \brief \copybrief KIM::TemperatureUnit
  !!
  !! \sa KIM::TemperatureUnit, KIM_TemperatureUnit
  !!
  !! \since 2.0
  type, bind(c) :: kim_temperature_unit_type
    !> \brief \copybrief KIM::TemperatureUnit::temperatureUnitID
    !!
    !! \sa KIM::TemperatureUnit::temperatureUnitID,
    !! KIM_TemperatureUnit::temperatureUnitID
    !!
    !! \since 2.0
    integer(c_int) temperature_unit_id
  end type kim_temperature_unit_type

  !> \brief \copybrief KIM::TEMPERATURE_UNIT::unused
  !!
  !! \sa KIM::TEMPERATURE_UNIT::, KIM_TEMPERATURE_UNIT_unused
  !!
  !! \since 2.0
  type(kim_temperature_unit_type), protected, save, &
    bind(c, name="KIM_TEMPERATURE_UNIT_unused") &
    :: KIM_TEMPERATURE_UNIT_UNUSED

  !> \brief \copybrief KIM::TEMPERATURE_UNIT::K
  !!
  !! \sa KIM::TEMPERATURE_UNIT::K, KIM_TEMPERATURE_UNIT_K
  !!
  !! \since 2.0
  type(kim_temperature_unit_type), protected, save, &
    bind(c, name="KIM_TEMPERATURE_UNIT_K") &
    :: KIM_TEMPERATURE_UNIT_K

  !> \brief \copybrief KIM::TemperatureUnit::Known
  !!
  !! \sa KIM::TemperatureUnit::Known, KIM_TemperatureUnit_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_temperature_unit_known
  end interface kim_known

  !> \brief \copybrief KIM::TemperatureUnit::operator==()
  !!
  !! \sa KIM::TemperatureUnit::operator==(), KIM_TemperatureUnit_Equal
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_temperature_unit_equal
  end interface operator(.eq.)

  !> \brief \copybrief KIM::TemperatureUnit::operator!=()
  !!
  !! \sa KIM::TemperatureUnit::operator!=(), KIM_TemperatureUnit_NotEqual
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_temperature_unit_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::TemperatureUnit::<!--
  !! -->TemperatureUnit(std::string const &)
  !!
  !! \sa KIM::TemperatureUnit::TemperatureUnit(std::string const &),
  !! KIM_TemperatureUnit_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_temperature_unit_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::TemperatureUnit::ToString
  !!
  !! \sa KIM::TemperatureUnit::ToString, KIM_TemperatureUnit_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_temperature_unit_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::TemperatureUnit::Known
  !!
  !! \sa KIM::TemperatureUnit::Known, KIM_TemperatureUnit_Known
  !!
  !! \since 2.0
  logical recursive function kim_temperature_unit_known(temperature_unit)
    implicit none
    interface
      integer(c_int) recursive function known(temperature_unit) &
        bind(c, name="KIM_TemperatureUnit_Known")
        use, intrinsic :: iso_c_binding
        import kim_temperature_unit_type
        implicit none
        type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      end function known
    end interface
    type(kim_temperature_unit_type), intent(in) :: temperature_unit

    kim_temperature_unit_known = (known(temperature_unit) /= 0)
  end function kim_temperature_unit_known

  !> \brief \copybrief KIM::TemperatureUnit::operator==()
  !!
  !! \sa KIM::TemperatureUnit::operator==(), KIM_TemperatureUnit_Equal
  !!
  !! \since 2.0
  logical recursive function kim_temperature_unit_equal(lhs, rhs)
    implicit none
    type(kim_temperature_unit_type), intent(in) :: lhs
    type(kim_temperature_unit_type), intent(in) :: rhs

    kim_temperature_unit_equal &
      = (lhs%temperature_unit_id == rhs%temperature_unit_id)
  end function kim_temperature_unit_equal

  !> \brief \copybrief KIM::TemperatureUnit::operator!=()
  !!
  !! \sa KIM::TemperatureUnit::operator!=(), KIM_TemperatureUnit_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_temperature_unit_not_equal(lhs, rhs)
    implicit none
    type(kim_temperature_unit_type), intent(in) :: lhs
    type(kim_temperature_unit_type), intent(in) :: rhs

    kim_temperature_unit_not_equal = .not. (lhs == rhs)
  end function kim_temperature_unit_not_equal

  !> \brief \copybrief KIM::TemperatureUnit::<!--
  !! -->TemperatureUnit(std::string const &)
  !!
  !! \sa KIM::TemperatureUnit::TemperatureUnit(std::string const &),
  !! KIM_TemperatureUnit_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_temperature_unit_from_string(string, &
                                                        temperature_unit)
    implicit none
    interface
      type(kim_temperature_unit_type) recursive function from_string(string) &
        bind(c, name="KIM_TemperatureUnit_FromString")
        use, intrinsic :: iso_c_binding
        import kim_temperature_unit_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_temperature_unit_type), intent(out) :: temperature_unit

    temperature_unit = from_string(trim(string)//c_null_char)
  end subroutine kim_temperature_unit_from_string

  !> \brief \copybrief KIM::TemperatureUnit::ToString
  !!
  !! \sa KIM::TemperatureUnit::ToString, KIM_TemperatureUnit_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_temperature_unit_to_string(temperature_unit, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(temperature_unit) &
        bind(c, name="KIM_TemperatureUnit_ToString")
        use, intrinsic :: iso_c_binding
        import kim_temperature_unit_type
        implicit none
        type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      end function get_string
    end interface
    type(kim_temperature_unit_type), intent(in) :: temperature_unit
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(temperature_unit)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_temperature_unit_to_string

  !> \brief \copybrief KIM::TEMPERATURE_UNIT::GetNumberOfTemperatureUnits
  !!
  !! \sa KIM::TEMPERATURE_UNIT::GetNumberOfTemperatureUnits,
  !! KIM_TEMPERATURE_UNIT_GetNumberOfTemperatureUnits
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_temperature_units( &
    number_of_temperature_units)
    implicit none
    interface
      recursive subroutine get_number_of_temperature_units( &
        number_of_temperature_units) &
        bind(c, name="KIM_TEMPERATURE_UNIT_GetNumberOfTemperatureUnits")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_temperature_units
      end subroutine get_number_of_temperature_units
    end interface
    integer(c_int), intent(out) :: number_of_temperature_units

    call get_number_of_temperature_units(number_of_temperature_units)
  end subroutine kim_get_number_of_temperature_units

  !> \brief \copybrief KIM::TEMPERATURE_UNIT::GetTemperatureUnit
  !!
  !! \sa KIM::TEMPERATURE_UNIT::GetTemperatureUnit,
  !! KIM_TEMPERATURE_UNIT_GetTemperatureUnit
  !!
  !! \since 2.0
  recursive subroutine kim_get_temperature_unit(index, &
                                                temperature_unit, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_temperature_unit(index, &
                                                             temperature_unit) &
        bind(c, name="KIM_TEMPERATURE_UNIT_GetTemperatureUnit")
        use, intrinsic :: iso_c_binding
        import kim_temperature_unit_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_temperature_unit_type), intent(out) :: temperature_unit
      end function get_temperature_unit
    end interface
    integer(c_int), intent(in) :: index
    type(kim_temperature_unit_type), intent(out) :: temperature_unit
    integer(c_int), intent(out) :: ierr

    ierr = get_temperature_unit(index - 1, temperature_unit)
  end subroutine kim_get_temperature_unit
end module kim_temperature_unit_module
