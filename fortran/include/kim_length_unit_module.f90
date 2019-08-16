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
! Release: This file is part of the kim-api-2.1.3 package.
!


!> \brief \copybrief KIM::LengthUnit
!!
!! \sa KIM::LengthUnit, KIM_LengthUnit
!!
!! \since 2.0
module kim_length_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derive types
    kim_length_unit_type, &

    ! Constants
    KIM_LENGTH_UNIT_UNUSED, &
    KIM_LENGTH_UNIT_A, &
    KIM_LENGTH_UNIT_BOHR, &
    KIM_LENGTH_UNIT_CM, &
    KIM_LENGTH_UNIT_M, &
    KIM_LENGTH_UNIT_NM, &

    ! Routines
    kim_known, &
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_length_units, &
    kim_get_length_unit


  !> \brief \copybrief KIM::LengthUnit
  !!
  !! \sa KIM::LengthUnit, KIM_LengthUnit
  !!
  !! \since 2.0
  type, bind(c) :: kim_length_unit_type
     !> \brief \copybrief KIM::LengthUnit::lengthUnitID
     !!
     !! \sa KIM::LengthUnit::lengthUnitID, KIM_LengthUnit::lengthUnitID
     !!
     !! \since 2.0
    integer(c_int) length_unit_id
  end type kim_length_unit_type

  !> \brief \copybrief KIM::LENGTH_UNIT::unused
  !!
  !! \sa KIM::LENGTH_UNIT::unused, KIM_LENGTH_UNIT_unused
  !!
  !! \since 2.0
  type(kim_length_unit_type), protected, save, &
    bind(c, name="KIM_LENGTH_UNIT_unused") &
    :: KIM_LENGTH_UNIT_UNUSED

  !> \brief \copybrief KIM::LENGTH_UNIT::A
  !!
  !! \sa KIM::LENGTH_UNIT::A, KIM_LENGTH_UNIT_A
  !!
  !! \since 2.0
  type(kim_length_unit_type), protected, save, &
    bind(c, name="KIM_LENGTH_UNIT_A") &
    :: KIM_LENGTH_UNIT_A

  !> \brief \copybrief KIM::LENGTH_UNIT::Bohr
  !!
  !! \sa KIM::LENGTH_UNIT::Bohr, KIM_LENGTH_UNIT_Bohr
  !!
  !! \since 2.0
  type(kim_length_unit_type), protected, save, &
    bind(c, name="KIM_LENGTH_UNIT_Bohr") &
    :: KIM_LENGTH_UNIT_BOHR

  !> \brief \copybrief KIM::LENGTH_UNIT::cm
  !!
  !! \sa KIM::LENGTH_UNIT::cm, KIM_LENGTH_UNIT_cm
  !!
  !! \since 2.0
  type(kim_length_unit_type), protected, save, &
    bind(c, name="KIM_LENGTH_UNIT_cm") &
    :: KIM_LENGTH_UNIT_CM

  !> \brief \copybrief KIM::LENGTH_UNIT::m
  !!
  !! \sa KIM::LENGTH_UNIT::m, KIM_LENGTH_UNIT_m
  !!
  !! \since 2.0
  type(kim_length_unit_type), protected, save, &
    bind(c, name="KIM_LENGTH_UNIT_m") &
    :: KIM_LENGTH_UNIT_M

  !> \brief \copybrief KIM::LENGTH_UNIT::nm
  !!
  !! \sa KIM::LENGTH_UNIT::nm, KIM_LENGTH_UNIT_nm
  !!
  !! \since 2.0
  type(kim_length_unit_type), protected, save, &
    bind(c, name="KIM_LENGTH_UNIT_nm") &
    :: KIM_LENGTH_UNIT_NM

  !> \brief \copybrief KIM::LengthUnit::Known
  !!
  !! \sa KIM::LengthUnit::Known, KIM_LengthUnit_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_length_unit_known
  end interface kim_known

  !> \brief \copybrief KIM::LengthUnit::operator==()
  !!
  !! \sa KIM::LengthUnit::operator==(), KIM_LengthUnit_Equal
  !!
  !! \since 2.0
  interface operator (.eq.)
    module procedure kim_length_unit_equal
  end interface operator (.eq.)

  !> \brief \copybrief KIM::LengthUnit::operator!=()
  !!
  !! \sa KIM::LengthUnit::operator!=(), KIM_LengthUnit_NotEqual
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_length_unit_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::LengthUnit::LengthUnit(std::string const &)
  !!
  !! \sa KIM::LengthUnit::LengthUnit(std::string const &),
  !! KIM_LengthUnit_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_length_unit_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::LengthUnit::ToString
  !!
  !! \sa KIM::LengthUnit::ToString, KIM_LengthUnit_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_length_unit_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::LengthUnit::Known
  !!
  !! \sa KIM::LengthUnit::Known, KIM_LengthUnit_Known
  !!
  !! \since 2.0
  logical recursive function kim_length_unit_known(length_unit)
    implicit none
    interface
      integer(c_int) recursive function known(length_unit) &
        bind(c, name="KIM_LengthUnit_Known")
        use, intrinsic :: iso_c_binding
        import kim_length_unit_type
        implicit none
        type(kim_length_unit_type), intent(in), value :: length_unit
      end function known
    end interface
    type(kim_length_unit_type), intent(in) :: length_unit

    kim_length_unit_known = (known(length_unit) /= 0)
  end function kim_length_unit_known

  !> \brief \copybrief KIM::LengthUnit::operator==()
  !!
  !! \sa KIM::LengthUnit::operator==(), KIM_LengthUnit_Equal
  !!
  !! \since 2.0
  logical recursive function kim_length_unit_equal(lhs, rhs)
    implicit none
    type(kim_length_unit_type), intent(in) :: lhs
    type(kim_length_unit_type), intent(in) :: rhs

    kim_length_unit_equal &
      = (lhs%length_unit_id .eq. rhs%length_unit_id)
  end function kim_length_unit_equal

  !> \brief \copybrief KIM::LengthUnit::operator!=()
  !!
  !! \sa KIM::LengthUnit::operator!=(), KIM_LengthUnit_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_length_unit_not_equal(lhs, rhs)
    implicit none
    type(kim_length_unit_type), intent(in) :: lhs
    type(kim_length_unit_type), intent(in) :: rhs

    kim_length_unit_not_equal = .not. (lhs .eq. rhs)
  end function kim_length_unit_not_equal

  !> \brief \copybrief KIM::LengthUnit::LengthUnit(std::string const &)
  !!
  !! \sa KIM::LengthUnit::LengthUnit(std::string const &),
  !! KIM_LengthUnit_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_length_unit_from_string(string, length_unit)
    implicit none
    interface
      type(kim_length_unit_type) recursive function from_string(string) &
        bind(c, name="KIM_LengthUnit_FromString")
        use, intrinsic :: iso_c_binding
        import kim_length_unit_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_length_unit_type), intent(out) :: length_unit

    length_unit = from_string(trim(string)//c_null_char)
  end subroutine kim_length_unit_from_string

  !> \brief \copybrief KIM::LengthUnit::ToString
  !!
  !! \sa KIM::LengthUnit::ToString, KIM_LengthUnit_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_length_unit_to_string(length_unit, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(length_unit) &
        bind(c, name="KIM_LengthUnit_ToString")
        use, intrinsic :: iso_c_binding
        import kim_length_unit_type
        implicit none
        type(kim_length_unit_type), intent(in), value :: length_unit
      end function get_string
    end interface
    type(kim_length_unit_type), intent(in) :: length_unit
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(length_unit)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_length_unit_to_string

  !> \brief \copybrief KIM::LENGTH_UNIT::GetNumberOfLengthUnits
  !!
  !! \sa KIM::LENGTH_UNIT::GetNumberOfLengthUnits,
  !! KIM_LENGTH_UNIT_GetNumberOfLengthUnits
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_length_units(number_of_length_units)
    implicit none
    interface
      recursive subroutine get_number_of_length_units(number_of_length_units) &
        bind(c, name="KIM_LENGTH_UNIT_GetNumberOfLengthUnits")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_length_units
      end subroutine get_number_of_length_units
    end interface
    integer(c_int), intent(out) :: number_of_length_units

    call get_number_of_length_units(number_of_length_units)
  end subroutine kim_get_number_of_length_units

  !> \brief \copybrief KIM::LENGTH_UNIT::GetLengthUnit
  !!
  !! \sa KIM::LENGTH_UNIT::GetLengthUnit, KIM_LENGTH_UNIT_GetLengthUnit
  !!
  !! \since 2.0
  recursive subroutine kim_get_length_unit(index, length_unit, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_length_unit(index, length_unit) &
        bind(c, name="KIM_LENGTH_UNIT_GetLengthUnit")
        use, intrinsic :: iso_c_binding
        import kim_length_unit_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_length_unit_type), intent(out) :: length_unit
      end function get_length_unit
    end interface
    integer(c_int), intent(in) :: index
    type(kim_length_unit_type), intent(out) :: length_unit
    integer(c_int), intent(out) :: ierr

    ierr = get_length_unit(index-1, length_unit)
  end subroutine kim_get_length_unit
end module kim_length_unit_module
