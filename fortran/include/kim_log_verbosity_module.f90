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


module kim_log_verbosity_module
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_id_module
  implicit none
  private

  public &
    kim_log_verbosity_type, &
    kim_log_verbosity_string, &
    operator (.lt.), &
    operator (.gt.), &
    operator (.le.), &
    operator (.ge.), &
    operator (.eq.), &
    operator (.ne.), &

    kim_log_verbosity_silent, &
    kim_log_verbosity_fatal, &
    kim_log_verbosity_error, &
    kim_log_verbosity_warning, &
    kim_log_verbosity_information, &
    kim_log_verbosity_debug,&

    kim_log_file, &
    kim_log_message

  type, bind(c) :: kim_log_verbosity_type
    integer(c_int) :: log_verbosity_id
  end type kim_log_verbosity_type

  type(kim_log_verbosity_type), parameter :: &
    kim_log_verbosity_silent = kim_log_verbosity_type(silent_id)
  type(kim_log_verbosity_type), parameter :: &
    kim_log_verbosity_fatal = kim_log_verbosity_type(fatal_id)
  type(kim_log_verbosity_type), parameter :: &
    kim_log_verbosity_error = kim_log_verbosity_type(error_id)
  type(kim_log_verbosity_type), parameter :: &
    kim_log_verbosity_warning = kim_log_verbosity_type(warning_id)
  type(kim_log_verbosity_type), parameter :: &
    kim_log_verbosity_information = kim_log_verbosity_type(information_id)
  type(kim_log_verbosity_type), parameter :: &
    kim_log_verbosity_debug = kim_log_verbosity_type(debug_id)

  interface operator (.lt.)
    module procedure kim_log_verbosity_less_than
  end interface operator (.lt.)

  interface operator (.gt.)
    module procedure kim_log_verbosity_greater_than
  end interface operator (.gt.)

  interface operator (.le.)
    module procedure kim_log_verbosity_less_than_equal
  end interface operator (.le.)

  interface operator (.ge.)
    module procedure kim_log_verbosity_greater_than_equal
  end interface operator (.ge.)

  interface operator (.eq.)
    module procedure kim_log_verbosity_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_log_verbosity_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_log_verbosity_string(log_verbosity, type_string)
      import kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*), intent(out) :: type_string
    end subroutine kim_log_verbosity_string
  end interface

  character(len=4096) :: kim_log_file
  character(len=65536) :: kim_log_message

contains
  logical function kim_log_verbosity_less_than(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_log_verbosity_type), intent(in) :: left
    type(kim_log_verbosity_type), intent(in) :: right

    kim_log_verbosity_less_than &
      = (left%log_verbosity_id .lt. right%log_verbosity_id)
  end function kim_log_verbosity_less_than

  logical function kim_log_verbosity_greater_than(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_log_verbosity_type), intent(in) :: left
    type(kim_log_verbosity_type), intent(in) :: right

    kim_log_verbosity_greater_than &
      = (left%log_verbosity_id .ge. right%log_verbosity_id)
  end function kim_log_verbosity_greater_than

  logical function kim_log_verbosity_less_than_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_log_verbosity_type), intent(in) :: left
    type(kim_log_verbosity_type), intent(in) :: right

    kim_log_verbosity_less_than_equal &
      = (left%log_verbosity_id .le. right%log_verbosity_id)
  end function kim_log_verbosity_less_than_equal

  logical function kim_log_verbosity_greater_than_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_log_verbosity_type), intent(in) :: left
    type(kim_log_verbosity_type), intent(in) :: right

    kim_log_verbosity_greater_than_equal &
      = (left%log_verbosity_id .ge. right%log_verbosity_id)
  end function kim_log_verbosity_greater_than_equal

  logical function kim_log_verbosity_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_log_verbosity_type), intent(in) :: left
    type(kim_log_verbosity_type), intent(in) :: right

    kim_log_verbosity_equal &
      = (left%log_verbosity_id .eq. right%log_verbosity_id)
  end function kim_log_verbosity_equal

  logical function kim_log_verbosity_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_log_verbosity_type), intent(in) :: left
    type(kim_log_verbosity_type), intent(in) :: right

    kim_log_verbosity_not_equal = .not. (left .eq. right)
  end function kim_log_verbosity_not_equal
end module kim_log_verbosity_module
