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
    from_string, &
    get_string

  interface
    type(kim_language_name_type) function from_string(string) &
      bind(c, name="KIM_LanguageNameFromString")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : &
        kim_language_name_type
      implicit none
      character(c_char), intent(in) :: string(*)
    end function from_string

    type(c_ptr) function get_string(language_name) &
      bind(c, name="KIM_LanguageNameString")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      implicit none
      type(kim_language_name_type), intent(in), value :: language_name
    end function get_string
  end interface
end module kim_language_name_f_module

! free functions to implement kim_language_name_module

subroutine kim_language_name_from_string(string, language_name)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_language_name_f_module, only : from_string
  implicit none
  character(len=*), intent(in) :: string
  type(kim_language_name_type), intent(out) :: language_name

  language_name = from_string(trim(string)//c_null_char)
end subroutine kim_language_name_from_string

logical function kim_language_name_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  implicit none
  type(kim_language_name_type), intent(in) :: left
  type(kim_language_name_type), intent(in) :: right

  kim_language_name_equal &
    = (left%language_name_id .eq. right%language_name_id)
end function kim_language_name_equal

logical function kim_language_name_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_language_name_module, only : operator(.eq.)
  implicit none
  type(kim_language_name_type), intent(in) :: left
  type(kim_language_name_type), intent(in) :: right

  kim_language_name_not_equal = .not. (left .eq. right)
end function kim_language_name_not_equal

subroutine kim_language_name_string(language_name, string)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_language_name_f_module, only : get_string
  implicit none
  type(kim_language_name_type), intent(in), value :: language_name
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)+1), pointer :: fp
  integer(c_int) :: null_index

  p = get_string(language_name)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_language_name_string
