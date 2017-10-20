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
  implicit none
  private

  public &
    kim_language_name_type, &
    kim_language_name_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_language_name_string, &

    kim_language_name_cpp, &
    kim_language_name_c, &
    kim_language_name_fortran

  type, bind(c) :: kim_language_name_type
    integer(c_int) :: language_name_id
  end type kim_language_name_type

  type(kim_language_name_type), protected, &
    bind(c, name="KIM_LANGUAGE_NAME_cpp") &
    :: kim_language_name_cpp
  type(kim_language_name_type), protected, &
    bind(c, name="KIM_LANGUAGE_NAME_c") &
    :: kim_language_name_c
  type(kim_language_name_type), protected, &
    bind(c, name="KIM_LANGUAGE_NAME_fortran") &
    :: kim_language_name_fortran

  interface operator (.eq.)
    logical function kim_language_name_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_language_name_type
      implicit none
      type(kim_language_name_type), intent(in) :: left
      type(kim_language_name_type), intent(in) :: right
    end function kim_language_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_language_name_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_language_name_type
      implicit none
      type(kim_language_name_type), intent(in) :: left
      type(kim_language_name_type), intent(in) :: right
    end function kim_language_name_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_language_name_from_string(string, language_name)
      import kim_language_name_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_language_name_type), intent(out) :: language_name
    end subroutine kim_language_name_from_string

    subroutine kim_language_name_string(language_name, string)
      import kim_language_name_type
      implicit none
      type(kim_language_name_type), intent(in), value :: language_name
      character(len=*), intent(out) :: string
    end subroutine kim_language_name_string
  end interface
end module kim_language_name_module
