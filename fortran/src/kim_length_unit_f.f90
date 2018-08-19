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
! Release: This file is part of the kim-api.git repository.
!


module kim_length_unit_f_module
  implicit none
  private

  public &
    from_string, &
    get_string, &
    get_number_of_length_units, &
    get_length_unit


  interface
    type(kim_length_unit_type) function from_string(string) &
      bind(c, name="KIM_LengthUnit_FromString")
      use, intrinsic :: iso_c_binding
      use kim_length_unit_module, only : &
        kim_length_unit_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(length_unit) &
      bind(c, name="KIM_LengthUnit_String")
      use, intrinsic :: iso_c_binding
      use kim_length_unit_module, only : kim_length_unit_type
      implicit none
      type(kim_length_unit_type), intent(in), value :: length_unit
    end function get_string

    subroutine get_number_of_length_units(number_of_length_units) &
      bind(c, name="KIM_LENGTH_UNIT_GetNumberOfLengthUnits")
      use, intrinsic :: iso_c_binding
      integer(c_int), intent(out) :: number_of_length_units
    end subroutine get_number_of_length_units

    integer(c_int) function get_length_unit(index, length_unit) &
      bind(c, name="KIM_LENGTH_UNIT_GetLengthUnit")
      use, intrinsic :: iso_c_binding
      use kim_length_unit_module, only : kim_length_unit_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_length_unit_type), intent(out) :: length_unit
    end function get_length_unit
  end interface
end module kim_length_unit_f_module

! free functions to implement kim_length_unit_module

subroutine kim_length_unit_from_string(string, length_unit)
  use, intrinsic :: iso_c_binding
  use kim_length_unit_module, only : kim_length_unit_type
  use kim_length_unit_f_module, only : from_string
  implicit none
  character(len=*, kind=c_char), intent(in) :: string
  type(kim_length_unit_type), intent(out) :: length_unit

  length_unit = from_string(trim(string)//c_null_char)
end subroutine kim_length_unit_from_string

logical function kim_length_unit_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_length_unit_module, only : kim_length_unit_type
  implicit none
  type(kim_length_unit_type), intent(in) :: left
  type(kim_length_unit_type), intent(in) :: right

  kim_length_unit_equal &
    = (left%length_unit_id .eq. right%length_unit_id)
end function kim_length_unit_equal

logical function kim_length_unit_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_length_unit_module, only : kim_length_unit_type
  use kim_length_unit_module, only : operator(.eq.)
  implicit none
  type(kim_length_unit_type), intent(in) :: left
  type(kim_length_unit_type), intent(in) :: right

  kim_length_unit_not_equal = .not. (left .eq. right)
end function kim_length_unit_not_equal

subroutine kim_length_unit_string(length_unit, string)
  use, intrinsic :: iso_c_binding
  use kim_length_unit_module, only : kim_length_unit_type
  use kim_length_unit_f_module, only : get_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_length_unit_type), intent(in), value :: length_unit
  character(len=*, kind=c_char), intent(out) :: string

  type(c_ptr) :: p

  p = get_string(length_unit)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_length_unit_string

subroutine kim_length_unit_get_number_of_length_units(number_of_length_units)
  use, intrinsic :: iso_c_binding
  use kim_length_unit_f_module, only : get_number_of_length_units
  implicit none
  integer(c_int), intent(out) :: number_of_length_units

  call get_number_of_length_units(number_of_length_units)
end subroutine kim_length_unit_get_number_of_length_units

subroutine kim_length_unit_get_length_unit(index, length_unit, ierr)
  use, intrinsic :: iso_c_binding
  use kim_length_unit_module, only : kim_length_unit_type
  use kim_length_unit_f_module, only : get_length_unit
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_length_unit_type), intent(out) :: length_unit
  integer(c_int), intent(out) :: ierr

  ierr = get_length_unit(index-1, length_unit)
end subroutine kim_length_unit_get_length_unit
