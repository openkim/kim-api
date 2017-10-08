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


module kim_numbering_module
  use, intrinsic :: iso_c_binding
  use kim_numbering_id_module
  implicit none
  private

  public &
    kim_numbering_type, &
    kim_numbering_string, &
    operator (.eq.), &
    operator (.ne.), &

    kim_numbering_zero_based, &
    kim_numbering_one_based

  type, bind(c) :: kim_numbering_type
    integer(c_int) :: numbering_id
  end type kim_numbering_type

  type(kim_numbering_type), parameter :: &
    kim_numbering_zero_based = &
    kim_numbering_type(zero_based_id)
  type(kim_numbering_type), parameter :: &
    kim_numbering_one_based = &
    kim_numbering_type(one_based_id)

  interface operator (.eq.)
    module procedure kim_numbering_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_numbering_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_numbering_string(numbering, name_string)
      import kim_numbering_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
      character(len=*), intent(out) :: name_string
    end subroutine kim_numbering_string
  end interface

contains
  logical function kim_numbering_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_numbering_type), intent(in) :: left
    type(kim_numbering_type), intent(in) :: right

    kim_numbering_equal &
      = (left%numbering_id .eq. right%numbering_id)
  end function kim_numbering_equal

  logical function kim_numbering_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_numbering_type), intent(in) :: left
    type(kim_numbering_type), intent(in) :: right

    kim_numbering_not_equal = .not. (left .eq. right)
  end function kim_numbering_not_equal
end module kim_numbering_module
