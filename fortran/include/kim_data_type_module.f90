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


module kim_data_type_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_data_type_type, &

    ! Constants
    KIM_DATA_TYPE_INTEGER, &
    KIM_DATA_TYPE_DOUBLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_data_types, &
    kim_get_data_type


  type, bind(c) :: kim_data_type_type
    integer(c_int) :: data_type_id
  end type kim_data_type_type

  type(kim_data_type_type), protected, &
    bind(c, name="KIM_DATA_TYPE_Integer") &
    :: KIM_DATA_TYPE_INTEGER
  type(kim_data_type_type), protected, &
    bind(c, name="KIM_DATA_TYPE_Double") &
    :: KIM_DATA_TYPE_DOUBLE

  interface operator (.eq.)
    module procedure kim_data_type_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_data_type_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_data_type_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_data_type_to_string
  end interface kim_to_string

contains
  logical function kim_data_type_equal(lhs, rhs)
    implicit none
    type(kim_data_type_type), intent(in) :: lhs
    type(kim_data_type_type), intent(in) :: rhs

    kim_data_type_equal &
      = (lhs%data_type_id .eq. rhs%data_type_id)
  end function kim_data_type_equal

  logical function kim_data_type_not_equal(lhs, rhs)
    implicit none
    type(kim_data_type_type), intent(in) :: lhs
    type(kim_data_type_type), intent(in) :: rhs

    kim_data_type_not_equal = .not. (lhs .eq. rhs)
  end function kim_data_type_not_equal

  subroutine kim_data_type_from_string(string, data_type)
    implicit none
    interface
      type(kim_data_type_type) function from_string(string) &
        bind(c, name="KIM_DataType_FromString")
        use, intrinsic :: iso_c_binding
        import kim_data_type_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_data_type_type), intent(out) :: data_type

    data_type = from_string(trim(string)//c_null_char)
  end subroutine kim_data_type_from_string

  subroutine kim_data_type_to_string(data_type, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) function get_string(data_type) &
        bind(c, name="KIM_DataType_ToString")
        use, intrinsic :: iso_c_binding
        import kim_data_type_type
        implicit none
        type(kim_data_type_type), intent(in), value :: data_type
      end function get_string
    end interface
    type(kim_data_type_type), intent(in) :: data_type
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(data_type)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_data_type_to_string

  subroutine kim_get_number_of_data_types(number_of_data_types)
    implicit none
    interface
      subroutine get_number_of_data_types(number_of_data_types) &
        bind(c, name="KIM_DATA_TYPE_GetNumberOfDataTypes")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_data_types
      end subroutine get_number_of_data_types
    end interface
    integer(c_int), intent(out) :: number_of_data_types

    call get_number_of_data_types(number_of_data_types)
  end subroutine kim_get_number_of_data_types

  subroutine kim_get_data_type(index, data_type, ierr)
    implicit none
    interface
      integer(c_int) function get_data_type(index, data_type) &
        bind(c, name="KIM_DATA_TYPE_GetDataType")
        use, intrinsic :: iso_c_binding
        import kim_data_type_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_data_type_type), intent(out) :: data_type
      end function get_data_type
    end interface
    integer(c_int), intent(in) :: index
    type(kim_data_type_type), intent(out) :: data_type
    integer(c_int), intent(out) :: ierr

    ierr = get_data_type(index-1, data_type)
  end subroutine kim_get_data_type
end module kim_data_type_module
