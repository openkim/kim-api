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


module kim_numbering_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_numbering_type, &
    kim_numbering_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_numbering_string, &

    kim_numbering_zero_based, &
    kim_numbering_one_based

  type, bind(c) :: kim_numbering_type
    integer(c_int) :: numbering_id
  end type kim_numbering_type

  type(kim_numbering_type), protected, &
    bind(c, name="KIM_NUMBERING_zeroBased") &
    :: kim_numbering_zero_based
  type(kim_numbering_type), protected, &
    bind(c, name="KIM_NUMBERING_oneBased") &
    :: kim_numbering_one_based

  interface operator (.eq.)
    logical function kim_numbering_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_numbering_type
      implicit none
      type(kim_numbering_type), intent(in) :: left
      type(kim_numbering_type), intent(in) :: right
    end function kim_numbering_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_numbering_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_numbering_type
      implicit none
      type(kim_numbering_type), intent(in) :: left
      type(kim_numbering_type), intent(in) :: right
    end function kim_numbering_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_numbering_from_string(string, numbering)
      import kim_numbering_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_numbering_type), intent(out) :: numbering
    end subroutine kim_numbering_from_string

    subroutine kim_numbering_string(numbering, string)
      import kim_numbering_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
      character(len=*), intent(out) :: string
    end subroutine kim_numbering_string
  end interface
end module kim_numbering_module
