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
! Copyright (c) 2016--2018, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api-v2-2.0.0-beta.3 package.
!


module kim_charge_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_charge_unit_type, &

    ! Constants
    KIM_CHARGE_UNIT_UNUSED, &
    KIM_CHARGE_UNIT_C, &
    KIM_CHARGE_UNIT_E, &
    KIM_CHARGE_UNIT_STATC, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_charge_units, &
    kim_get_charge_unit


  type, bind(c) :: kim_charge_unit_type
    integer(c_int) charge_unit_id
  end type kim_charge_unit_type

  type(kim_charge_unit_type), protected, &
    bind(c, name="KIM_CHARGE_UNIT_unused") &
    :: KIM_CHARGE_UNIT_UNUSED
  type(kim_charge_unit_type), protected, &
    bind(c, name="KIM_CHARGE_UNIT_c") &
    :: KIM_CHARGE_UNIT_C
  type(kim_charge_unit_type), protected, &
    bind(c, name="KIM_CHARGE_UNIT_e") &
    :: KIM_CHARGE_UNIT_E
  type(kim_charge_unit_type), protected, &
    bind(c, name="KIM_CHARGE_UNIT_statC") &
    :: KIM_CHARGE_UNIT_STATC

  interface operator (.eq.)
    module procedure kim_charge_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_charge_unit_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_charge_unit_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_charge_unit_to_string
  end interface kim_to_string

contains
  logical function kim_charge_unit_equal(lhs, rhs)
    implicit none
    type(kim_charge_unit_type), intent(in) :: lhs
    type(kim_charge_unit_type), intent(in) :: rhs

    kim_charge_unit_equal &
      = (lhs%charge_unit_id .eq. rhs%charge_unit_id)
  end function kim_charge_unit_equal

  logical function kim_charge_unit_not_equal(lhs, rhs)
    implicit none
    type(kim_charge_unit_type), intent(in) :: lhs
    type(kim_charge_unit_type), intent(in) :: rhs

    kim_charge_unit_not_equal = .not. (lhs .eq. rhs)
  end function kim_charge_unit_not_equal

  subroutine kim_charge_unit_from_string(string, charge_unit)
    implicit none
    interface
      type(kim_charge_unit_type) function from_string(string) &
        bind(c, name="KIM_ChargeUnit_FromString")
        use, intrinsic :: iso_c_binding
        import kim_charge_unit_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_charge_unit_type), intent(out) :: charge_unit

    charge_unit = from_string(trim(string)//c_null_char)
  end subroutine kim_charge_unit_from_string

  subroutine kim_charge_unit_to_string(charge_unit, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) function get_string(charge_unit) &
        bind(c, name="KIM_ChargeUnit_ToString")
        use, intrinsic :: iso_c_binding
        import kim_charge_unit_type
        implicit none
        type(kim_charge_unit_type), intent(in), value :: charge_unit
      end function get_string
    end interface
    type(kim_charge_unit_type), intent(in) :: charge_unit
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(charge_unit)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_charge_unit_to_string

  subroutine kim_get_number_of_charge_units(number_of_charge_units)
    implicit none
    interface
      subroutine get_number_of_charge_units(number_of_charge_units) &
        bind(c, name="KIM_CHARGE_UNIT_GetNumberOfChargeUnits")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_charge_units
      end subroutine get_number_of_charge_units
    end interface
    integer(c_int), intent(out) :: number_of_charge_units

    call get_number_of_charge_units(number_of_charge_units)
  end subroutine kim_get_number_of_charge_units

  subroutine kim_get_charge_unit(index, charge_unit, ierr)
    implicit none
    interface
      integer(c_int) function get_charge_unit(index, charge_unit) &
        bind(c, name="KIM_CHARGE_UNIT_GetChargeUnit")
        use, intrinsic :: iso_c_binding
        import kim_charge_unit_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_charge_unit_type), intent(out) :: charge_unit
      end function get_charge_unit
    end interface
    integer(c_int), intent(in) :: index
    type(kim_charge_unit_type), intent(out) :: charge_unit
    integer(c_int), intent(out) :: ierr

    ierr = get_charge_unit(index-1, charge_unit)
  end subroutine kim_get_charge_unit
end module kim_charge_unit_module
