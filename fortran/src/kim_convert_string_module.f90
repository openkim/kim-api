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


module kim_convert_string_module
  implicit none
  private

  public &
    kim_convert_string

contains
  subroutine kim_convert_string(p, string)
    use, intrinsic :: iso_c_binding
    implicit none
    type(c_ptr), intent(in) :: p
    character(len=*, kind=c_char), intent(out) :: string

    character(len=1, kind=c_char), pointer :: fp(:)
    integer(c_int) :: i
    integer(c_int) :: null_index

    call c_f_pointer(p, fp, [len(string)+1])
    do null_index=1,(len(string)+1)
      if (fp(null_index) .eq. char(0)) exit
    end do
    if (null_index .eq. len(string)+1) then
      null_index = len(string)
    else
      null_index = null_index - 1
    end if
    string = ""
    do i=1,null_index
      string(i:i) = fp(i)
    end do
  end subroutine kim_convert_string
end module kim_convert_string_module
