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


module kim_data_type_f_module
  implicit none
  private

  public &
    from_string, &
    get_string, &
    get_number_of_data_types, &
    get_data_type


  interface
    type(kim_data_type_type) function from_string(string) &
      bind(c, name="KIM_DataType_FromString")
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : &
        kim_data_type_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(data_type) &
      bind(c, name="KIM_DataType_String")
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : kim_data_type_type
      implicit none
      type(kim_data_type_type), intent(in), value :: data_type
    end function get_string

    subroutine get_number_of_data_types(number_of_data_types) &
      bind(c, name="KIM_DATA_TYPE_GetNumberOfDataTypes")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_data_types
    end subroutine get_number_of_data_types

    integer(c_int) function get_data_type(index, data_type) &
      bind(c, name="KIM_DATA_TYPE_GetDataType")
      use, intrinsic :: iso_c_binding
      use kim_data_type_module, only : kim_data_type_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_data_type_type), intent(out) :: data_type
    end function get_data_type
  end interface
end module kim_data_type_f_module

! free functions to implement kim_data_type_module

subroutine kim_data_type_from_string(string, data_type)
  use, intrinsic :: iso_c_binding
  use kim_data_type_module, only : kim_data_type_type
  use kim_data_type_f_module, only : from_string
  implicit none
  character(len=*, kind=c_char), intent(in) :: string
  type(kim_data_type_type), intent(out) :: data_type

  data_type = from_string(trim(string)//c_null_char)
end subroutine kim_data_type_from_string

logical function kim_data_type_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_data_type_module, only : kim_data_type_type
  implicit none
  type(kim_data_type_type), intent(in) :: left
  type(kim_data_type_type), intent(in) :: right

  kim_data_type_equal &
    = (left%data_type_id .eq. right%data_type_id)
end function kim_data_type_equal

logical function kim_data_type_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_data_type_module, only : kim_data_type_type
  use kim_data_type_module, only : operator(.eq.)
  implicit none
  type(kim_data_type_type), intent(in) :: left
  type(kim_data_type_type), intent(in) :: right

  kim_data_type_not_equal = .not. (left .eq. right)
end function kim_data_type_not_equal


subroutine kim_data_type_string(data_type, string)
  use, intrinsic :: iso_c_binding
  use kim_data_type_module, only : kim_data_type_type
  use kim_data_type_f_module, only : get_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_data_type_type), intent(in), value :: data_type
  character(len=*, kind=c_char), intent(out) :: string

  type(c_ptr) :: p

  p = get_string(data_type)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_data_type_string

subroutine kim_data_type_get_number_of_data_types(number_of_data_types)
  use, intrinsic :: iso_c_binding
  use kim_data_type_f_module, only : get_number_of_data_types
  implicit none
  integer(c_int), intent(out) :: number_of_data_types

  call get_number_of_data_types(number_of_data_types)
end subroutine kim_data_type_get_number_of_data_types

subroutine kim_data_type_get_data_type(index, data_type, ierr)
  use, intrinsic :: iso_c_binding
  use kim_data_type_module, only : kim_data_type_type
  use kim_data_type_f_module, only : get_data_type
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_data_type_type), intent(out) :: data_type
  integer(c_int), intent(out) :: ierr

  ierr = get_data_type(index-1, data_type)
end subroutine kim_data_type_get_data_type
