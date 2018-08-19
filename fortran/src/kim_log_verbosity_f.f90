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


module kim_log_verbosity_f_module
  implicit none
  private

  public &
    from_string, &
    get_string, &
    get_number_of_log_verbosities, &
    get_log_verbosity


  interface
    type(kim_log_verbosity_type) function from_string(string) &
      bind(c, name="KIM_LogVerbosity_FromString")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : &
        kim_log_verbosity_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(log_verbosity) &
      bind(c, name="KIM_LogVerbosity_String")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      implicit none
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
    end function get_string

    subroutine get_number_of_log_verbosities(number_of_log_verbosities) &
      bind(c, name="KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_log_verbosities
    end subroutine get_number_of_log_verbosities

    integer(c_int) function get_log_verbosity(index, log_verbosity) &
      bind(c, name="KIM_LOG_VERBOSITY_GetLogVerbosity")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_log_verbosity_type), intent(out) :: log_verbosity
    end function get_log_verbosity
  end interface
end module kim_log_verbosity_f_module

! free functions to implement kim_log_verbosity_module

subroutine kim_log_verbosity_from_string(string, log_verbosity)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_log_verbosity_f_module, only : from_string
  implicit none
  character(len=*, kind=c_char), intent(in) :: string
  type(kim_log_verbosity_type), intent(out) :: log_verbosity

  log_verbosity = from_string(trim(string)//c_null_char)
end subroutine kim_log_verbosity_from_string

logical function kim_log_verbosity_less_than(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_verbosity_type), intent(in) :: left
  type(kim_log_verbosity_type), intent(in) :: right

  kim_log_verbosity_less_than &
    = (left%log_verbosity_id .lt. right%log_verbosity_id)
end function kim_log_verbosity_less_than

logical function kim_log_verbosity_greater_than(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_verbosity_type), intent(in) :: left
  type(kim_log_verbosity_type), intent(in) :: right

  kim_log_verbosity_greater_than &
    = (left%log_verbosity_id .ge. right%log_verbosity_id)
end function kim_log_verbosity_greater_than

logical function kim_log_verbosity_less_than_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_verbosity_type), intent(in) :: left
  type(kim_log_verbosity_type), intent(in) :: right

  kim_log_verbosity_less_than_equal &
    = (left%log_verbosity_id .le. right%log_verbosity_id)
end function kim_log_verbosity_less_than_equal

logical function kim_log_verbosity_greater_than_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_verbosity_type), intent(in) :: left
  type(kim_log_verbosity_type), intent(in) :: right

  kim_log_verbosity_greater_than_equal &
    = (left%log_verbosity_id .ge. right%log_verbosity_id)
end function kim_log_verbosity_greater_than_equal

logical function kim_log_verbosity_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_verbosity_type), intent(in) :: left
  type(kim_log_verbosity_type), intent(in) :: right

  kim_log_verbosity_equal &
    = (left%log_verbosity_id .eq. right%log_verbosity_id)
end function kim_log_verbosity_equal

logical function kim_log_verbosity_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_log_verbosity_module, only : operator(.eq.)
  implicit none
  type(kim_log_verbosity_type), intent(in) :: left
  type(kim_log_verbosity_type), intent(in) :: right

  kim_log_verbosity_not_equal = .not. (left .eq. right)
end function kim_log_verbosity_not_equal

subroutine kim_log_verbosity_string(log_verbosity, string)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_log_verbosity_f_module, only : get_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  character(len=*, kind=c_char), intent(out) :: string

  type(c_ptr) :: p

  p = get_string(log_verbosity)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_log_verbosity_string

subroutine kim_log_verbosity_get_number_of_log_verbosities( &
  number_of_log_verbosities)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_f_module, only : get_number_of_log_verbosities
  implicit none
  integer(c_int), intent(out) :: number_of_log_verbosities

  call get_number_of_log_verbosities(number_of_log_verbosities)
end subroutine kim_log_verbosity_get_number_of_log_verbosities

subroutine kim_log_verbosities_get_log_verbosity(index, log_verbosity, ierr)
  use, intrinsic :: iso_c_binding
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_log_verbosity_f_module, only : get_log_verbosity
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_log_verbosity_type), intent(out) :: log_verbosity
  integer(c_int), intent(out) :: ierr

  ierr = get_log_verbosity(index-1, log_verbosity)
end subroutine kim_log_verbosities_get_log_verbosity
