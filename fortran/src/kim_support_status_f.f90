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


module kim_support_status_f_module
  implicit none
  private

  public &
    from_string, &
    get_string, &
    get_number_of_support_statuses, &
    get_support_status


  interface
    type(kim_support_status_type) function from_string(string) &
      bind(c, name="KIM_SupportStatus_FromString")
      use, intrinsic :: iso_c_binding
      use kim_support_status_module, only : &
        kim_support_status_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(support_status) &
      bind(c, name="KIM_SupportStatus_String")
      use, intrinsic :: iso_c_binding
      use kim_support_status_module, only : kim_support_status_type
      implicit none
      type(kim_support_status_type), intent(in), value :: support_status
    end function get_string

    subroutine get_number_of_support_statuses(number_of_support_statuses) &
      bind(c, name="KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_support_statuses
    end subroutine get_number_of_support_statuses

    integer(c_int) function get_support_status(index, support_status) &
      bind(c, name="KIM_SUPPORT_STATUS_GetSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_support_status_module, only : kim_support_status_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_support_status_type), intent(out) :: support_status
    end function get_support_status
  end interface
end module kim_support_status_f_module

! free functions to implement kim_support_status_module

subroutine kim_support_status_from_string(string, support_status)
  use, intrinsic :: iso_c_binding
  use kim_support_status_module, only : kim_support_status_type
  use kim_support_status_f_module, only : from_string
  implicit none
  character(len=*, kind=c_char), intent(in) :: string
  type(kim_support_status_type), intent(out) :: support_status

  support_status = from_string(trim(string)//c_null_char)
end subroutine kim_support_status_from_string

logical function kim_support_status_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_support_status_module, only : kim_support_status_type
  implicit none
  type(kim_support_status_type), intent(in) :: left
  type(kim_support_status_type), intent(in) :: right

  kim_support_status_equal &
    = (left%support_status_id .eq. right%support_status_id)
end function kim_support_status_equal

logical function kim_support_status_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_support_status_module, only : kim_support_status_type
  use kim_support_status_module, only : operator(.eq.)
  implicit none
  type(kim_support_status_type), intent(in) :: left
  type(kim_support_status_type), intent(in) :: right

  kim_support_status_not_equal = .not. (left .eq. right)
end function kim_support_status_not_equal

subroutine kim_support_status_string(support_status, string)
  use, intrinsic :: iso_c_binding
  use kim_support_status_module, only : kim_support_status_type
  use kim_support_status_f_module, only : get_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_support_status_type), intent(in), value :: support_status
  character(len=*, kind=c_char), intent(out) :: string

  type(c_ptr) :: p

  p = get_string(support_status)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_support_status_string

subroutine kim_support_status_get_number_of_support_statuses( &
  number_of_support_statuses)
  use, intrinsic :: iso_c_binding
  use kim_support_status_f_module, only : get_number_of_support_statuses
  implicit none
  integer(c_int), intent(out) :: number_of_support_statuses

  call get_number_of_support_statuses(number_of_support_statuses)
end subroutine kim_support_status_get_number_of_support_statuses

subroutine kim_support_status_get_support_status(index, support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_support_status_module, only : kim_support_status_type
  use kim_support_status_f_module, only : get_support_status
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_support_status_type), intent(out) :: support_status
  integer(c_int), intent(out) :: ierr

  ierr = get_support_status(index-1, support_status)
end subroutine kim_support_status_get_support_status
