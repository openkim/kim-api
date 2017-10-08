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


module kim_language_name_module
  use, intrinsic :: iso_c_binding
  use kim_language_name_id_module
  implicit none
  private

  public &
    kim_language_name_type, &
    kim_language_name_string, &
    operator (.eq.), &
    operator (.ne.), &

    kim_language_name_cpp, &
    kim_language_name_c, &
    kim_language_name_fortran

  type, bind(c) :: kim_language_name_type
    integer(c_int) :: language_name_id
  end type kim_language_name_type

  type(kim_language_name_type), parameter :: &
    kim_language_name_cpp = &
    kim_language_name_type(cpp_id)
  type(kim_language_name_type), parameter :: &
    kim_language_name_c = &
    kim_language_name_type(c_id)
  type(kim_language_name_type), parameter :: &
    kim_language_name_fortran = &
    kim_language_name_type(fortran_id)

  interface operator (.eq.)
    module procedure kim_language_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_language_name_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_language_name_string(language_name, name_string)
      import kim_language_name_type
      implicit none
      type(kim_language_name_type), intent(in), value :: language_name
      character(len=*), intent(out) :: name_string
    end subroutine kim_language_name_string
  end interface

contains
  logical function kim_language_name_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_language_name_type), intent(in) :: left
    type(kim_language_name_type), intent(in) :: right

    kim_language_name_equal &
      = (left%language_name_id .eq. right%language_name_id)
  end function kim_language_name_equal

  logical function kim_language_name_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_language_name_type), intent(in) :: left
    type(kim_language_name_type), intent(in) :: right

    kim_language_name_not_equal = .not. (left .eq. right)
  end function kim_language_name_not_equal
end module kim_language_name_module
