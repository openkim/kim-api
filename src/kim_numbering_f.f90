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


module kim_numbering_f_module
  implicit none
  private

  public &
    numbering_string

  interface
    type(c_ptr) function numbering_string(numbering) &
      bind(c, name="KIM_NumberingString")
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      implicit none
      type(kim_numbering_type), intent(in), value :: numbering
    end function numbering_string
  end interface
end module kim_numbering_f_module

! free functions to implement kim_numbering_module

subroutine kim_numbering_string(numbering, name_string)
  use, intrinsic :: iso_c_binding
  use kim_numbering_module, only : kim_numbering_type
  use kim_numbering_f_module, only : numbering_string
  implicit none
  type(kim_numbering_type), intent(in), value :: numbering
  character(len=*), intent(out) :: name_string

  type(c_ptr) :: p
  character(len=len(name_string)), pointer :: fp
  integer(c_int) :: null_index

  p = numbering_string(numbering)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  name_string = fp(1:null_index)
end subroutine kim_numbering_string
