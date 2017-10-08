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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
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
  use kim_data_type_id_module
  implicit none
  private

  public &
    kim_data_type_type, &
    kim_data_type_string, &
    operator (.eq.), &
    operator (.ne.), &

    kim_data_type_integer, &
    kim_data_type_double

  type, bind(c) :: kim_data_type_type
    integer(c_int) :: data_type_id
  end type kim_data_type_type

  type(kim_data_type_type), parameter :: &
    kim_data_type_integer = &
    kim_data_type_type(integer_id)
  type(kim_data_type_type), parameter :: &
    kim_data_type_double = &
    kim_data_type_type(double_id)

  interface operator (.eq.)
    module procedure kim_data_type_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_data_type_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_data_type_string(data_type, type_string)
      import kim_data_type_type
      implicit none
      type(kim_data_type_type), intent(in), value :: data_type
      character(len=*), intent(out) :: type_string
    end subroutine kim_data_type_string
  end interface

contains
  logical function kim_data_type_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_data_type_type), intent(in) :: left
    type(kim_data_type_type), intent(in) :: right

    kim_data_type_equal &
      = (left%data_type_id .eq. right%data_type_id)
  end function kim_data_type_equal

  logical function kim_data_type_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_data_type_type), intent(in) :: left
    type(kim_data_type_type), intent(in) :: right

    kim_data_type_not_equal = .not. (left .eq. right)
  end function kim_data_type_not_equal
end module kim_data_type_module
