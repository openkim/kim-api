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


module kim_support_status_f_module
  implicit none
  private

  public &
    support_status_str

  interface
    type(c_ptr) function support_status_str(support_status) &
      bind(c, name="KIM_SupportStatusString")
      use, intrinsic :: iso_c_binding
      use kim_support_status_module, only : kim_support_status_type
      implicit none
      type(kim_support_status_type), intent(in), value :: support_status
    end function support_status_str
  end interface
end module kim_support_status_f_module

! free functions to implement kim_support_status_module

subroutine kim_support_status_string(support_status, support_status_string)
  use, intrinsic :: iso_c_binding
  use kim_support_status_module, only : kim_support_status_type
  use kim_support_status_f_module, only : support_status_str
  implicit none
  type(kim_support_status_type), intent(in), value :: support_status
  character(len=*), intent(out) :: support_status_string

  type(c_ptr) :: p
  character(len=len(support_status_string)+1), pointer :: fp
  integer(c_int) :: null_index

  p = support_status_str(support_status)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  support_status_string = fp(1:null_index)
end subroutine kim_support_status_string
