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
    operator (.eq.), &
    operator (.ne.), &
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
  interface operator (.eq.)
    module procedure kim_temperature_unit_equal
  end interface operator (.eq.)

  !> \brief \copybrief KIM::TemperatureUnit::operator!=()
  !!
  !! \sa KIM::TemperatureUnit::operator!=(), KIM_TemperatureUnit_NotEqual
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_temperature_unit_not_equal
  end interface operator (.ne.)

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
      = (lhs%temperature_unit_id .eq. rhs%temperature_unit_id)
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

    kim_temperature_unit_not_equal = .not. (lhs .eq. rhs)
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
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
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

    ierr = get_temperature_unit(index-1, temperature_unit)
  end subroutine kim_get_temperature_unit
end module kim_temperature_unit_module
