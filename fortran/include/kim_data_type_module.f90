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


module kim_data_type_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_data_type_type, &
    kim_data_type_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_data_type_string, &

    kim_data_type_integer, &
    kim_data_type_double, &

    kim_data_type_get_number_of_data_types, &
    kim_data_type_get_data_type


  type, bind(c) :: kim_data_type_type
    integer(c_int) :: data_type_id
  end type kim_data_type_type

  type(kim_data_type_type), protected, &
    bind(c, name="KIM_DATA_TYPE_Integer") &
    :: kim_data_type_integer
  type(kim_data_type_type), protected, &
    bind(c, name="KIM_DATA_TYPE_Double") &
    :: kim_data_type_double

  interface operator (.eq.)
    logical function kim_data_type_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_data_type_type
      implicit none
      type(kim_data_type_type), intent(in) :: left
      type(kim_data_type_type), intent(in) :: right
    end function kim_data_type_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_data_type_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_data_type_type
      implicit none
      type(kim_data_type_type), intent(in) :: left
      type(kim_data_type_type), intent(in) :: right
    end function kim_data_type_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_data_type_from_string(string, data_type)
      import kim_data_type_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_data_type_type), intent(out) :: data_type
    end subroutine kim_data_type_from_string

    subroutine kim_data_type_string(data_type, string)
      import kim_data_type_type
      implicit none
      type(kim_data_type_type), intent(in), value :: data_type
      character(len=*), intent(out) :: string
    end subroutine kim_data_type_string

    subroutine kim_data_type_get_number_of_data_types(number_of_data_types)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_data_types
    end subroutine kim_data_type_get_number_of_data_types

    subroutine kim_data_type_get_data_type(index, data_type, ierr)
      use, intrinsic :: iso_c_binding
      import kim_data_type_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_data_type_type), intent(out) :: data_type
      integer(c_int), intent(out) :: ierr
    end subroutine kim_data_type_get_data_type
  end interface
end module kim_data_type_module
