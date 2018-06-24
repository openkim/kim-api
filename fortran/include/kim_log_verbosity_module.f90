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


module kim_log_verbosity_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_log_verbosity_type, &
    kim_log_verbosity_from_string, &
    operator (.lt.), &
    operator (.gt.), &
    operator (.le.), &
    operator (.ge.), &
    operator (.eq.), &
    operator (.ne.), &
    kim_log_verbosity_string, &

    kim_log_verbosity_silent, &
    kim_log_verbosity_fatal, &
    kim_log_verbosity_error, &
    kim_log_verbosity_warning, &
    kim_log_verbosity_information, &
    kim_log_verbosity_debug, &

    kim_log_verbosity_get_number_of_log_verbosities, &
    kim_log_verbosity_get_log_verbosity, &

    kim_log_file, &
    kim_log_message


  type, bind(c) :: kim_log_verbosity_type
    integer(c_int) :: log_verbosity_id
  end type kim_log_verbosity_type

  type(kim_log_verbosity_type), protected, &
    bind(c, name="KIM_LOG_VERBOSITY_silent") &
    :: kim_log_verbosity_silent
  type(kim_log_verbosity_type), protected, &
    bind(c, name="KIM_LOG_VERBOSITY_fatal") &
    :: kim_log_verbosity_fatal
  type(kim_log_verbosity_type), protected, &
    bind(c, name="KIM_LOG_VERBOSITY_error") &
    :: kim_log_verbosity_error
  type(kim_log_verbosity_type), protected, &
    bind(c, name="KIM_LOG_VERBOSITY_warning") &
    :: kim_log_verbosity_warning
  type(kim_log_verbosity_type), protected, &
    bind(c, name="KIM_LOG_VERBOSITY_information") &
    :: kim_log_verbosity_information
  type(kim_log_verbosity_type), protected, &
    bind(c, name="KIM_LOG_VERBOSITY_debug") &
    :: kim_log_verbosity_debug

  interface operator (.lt.)
    logical function kim_log_verbosity_less_than(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in) :: left
      type(kim_log_verbosity_type), intent(in) :: right
    end function kim_log_verbosity_less_than
  end interface operator (.lt.)

  interface operator (.gt.)
    logical function kim_log_verbosity_greater_than(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in) :: left
      type(kim_log_verbosity_type), intent(in) :: right
    end function kim_log_verbosity_greater_than
  end interface operator (.gt.)

  interface operator (.le.)
    logical function kim_log_verbosity_less_than_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in) :: left
      type(kim_log_verbosity_type), intent(in) :: right
    end function kim_log_verbosity_less_than_equal
  end interface operator (.le.)

  interface operator (.ge.)
    logical function kim_log_verbosity_greater_than_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in) :: left
      type(kim_log_verbosity_type), intent(in) :: right
    end function kim_log_verbosity_greater_than_equal
  end interface operator (.ge.)

  interface operator (.eq.)
    logical function kim_log_verbosity_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in) :: left
      type(kim_log_verbosity_type), intent(in) :: right
    end function kim_log_verbosity_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_log_verbosity_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in) :: left
      type(kim_log_verbosity_type), intent(in) :: right
    end function kim_log_verbosity_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_log_verbosity_from_string(string, log_verbosity)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      character(len=*, kind=c_char), intent(in) :: string
      type(kim_log_verbosity_type), intent(out) :: log_verbosity
    end subroutine kim_log_verbosity_from_string

    subroutine kim_log_verbosity_string(log_verbosity, string)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*, kind=c_char), intent(out) :: string
    end subroutine kim_log_verbosity_string

    subroutine kim_log_verbosity_get_number_of_log_verbosities( &
      number_of_log_verbosities)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_log_verbosities
    end subroutine kim_log_verbosity_get_number_of_log_verbosities

    subroutine kim_log_verbosity_get_log_verbosity(index, log_verbosity, ierr)
      use, intrinsic :: iso_c_binding
      import kim_log_verbosity_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_log_verbosity_type), intent(out) :: log_verbosity
      integer(c_int), intent(out) :: ierr
    end subroutine kim_log_verbosity_get_log_verbosity
  end interface

  character(len=4096, kind=c_char) :: kim_log_file
  character(len=65536, kind=c_char) :: kim_log_message
end module kim_log_verbosity_module
