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


module kim_logger_f_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    get_status_msg, &
    report_error

  interface
    subroutine get_status_msg(status_code, status_message) &
      bind(c, name="KIM_get_status_msg")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: status_code
      type(c_ptr), intent(out) :: status_message
    end subroutine get_status_msg

    subroutine report_error(line, file, user_message, status_code) &
      bind(c, name="KIM_report_error")
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: line
      character(c_char), intent(in) :: file(*)
      character(c_char), intent(in) :: user_message(*)
      integer(c_int), intent(in), value :: status_code
    end subroutine report_error
  end interface
end module kim_logger_f_module

subroutine kim_get_status_msg(status_code, status_message)
  use, intrinsic :: iso_c_binding
  use :: kim_logger_f_module, only : get_status_msg
  implicit none
  integer(c_int), intent(in), value :: status_code
  character(len=*), intent(out) :: status_message

  type(c_ptr) :: p
  character(len=len(status_message)), pointer :: fp
  integer(c_int) :: null_index

  call get_status_msg(status_code, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  status_message = fp(1:null_index)
end subroutine kim_get_status_msg

subroutine kim_report_error(line, file, user_message, status_code)
  use, intrinsic :: iso_c_binding
  use :: kim_logger_f_module, only : report_error
  implicit none
  integer(c_int), intent(in) :: line
  character(len=*), intent(in) :: file
  character(len=*), intent(in) :: user_message
  integer(c_int), intent(in), value :: status_code

  call report_error(line, file//c_null_char, user_message//c_null_char, status_code)
end subroutine kim_report_error
