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


module kim_logger_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_get_status_msg, &
    kim_report_error

  interface
    subroutine kim_get_status_msg(status_code, status_message)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in), value :: status_code
      character(len=*), intent(out) :: status_message
    end subroutine kim_get_status_msg

    subroutine kim_report_error(line, file, user_message, status_code)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(in) :: line
      character(len=*), intent(in) :: file
      character(len=*), intent(in) :: user_message
      integer(c_int), intent(in), value :: status_code
    end subroutine kim_report_error
  end interface
end module kim_logger_module
