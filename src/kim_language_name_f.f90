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


module kim_language_name_f_module
  implicit none
  private

  public &
    language_name_string

  interface
    type(c_ptr) function language_name_string(language_name) &
      bind(c, name="KIM_LanguageNameString")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      implicit none
      type(kim_language_name_type), intent(in), value :: language_name
    end function language_name_string
  end interface
end module kim_language_name_f_module

! free functions to implement kim_language_name_module

subroutine kim_language_name_string(language_name, name_string)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_language_name_f_module, only : language_name_string
  implicit none
  type(kim_language_name_type), intent(in), value :: language_name
  character(len=*), intent(out) :: name_string

  type(c_ptr) :: p
  character(len=len(name_string)), pointer :: fp
  integer(c_int) :: null_index

  p = language_name_string(language_name)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  name_string = fp(1:null_index)
end subroutine kim_language_name_string
