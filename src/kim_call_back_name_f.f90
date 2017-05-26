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


module kim_call_back_name_f_module
  implicit none
  private

  public &
    call_back_name_string, &
    get_number_of_call_backs, &
    get_call_back_name

  interface
    type(c_ptr) function call_back_name_string(call_back_name) &
      bind(c, name="KIM_CallBackNameString")
      use, intrinsic :: iso_c_binding
      use kim_call_back_name_module, only : kim_call_back_name_type
      implicit none
      type(kim_call_back_name_type), intent(in), value :: call_back_name
    end function call_back_name_string

    subroutine get_number_of_call_backs(number_of_call_backs) &
      bind(c, name="KIM_CALL_BACK_NAME_get_number_of_call_backs")
      use, intrinsic :: iso_c_binding
      integer(c_int), intent(out) :: number_of_call_backs
    end subroutine get_number_of_call_backs

    integer(c_int) function get_call_back_name(index, call_back_name) &
      bind(c, name="KIM_CALL_BACK_NAME_get_call_back_name")
      use, intrinsic :: iso_c_binding
      use kim_call_back_name_module, only : kim_call_back_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_call_back_name_type), intent(out) :: call_back_name
    end function get_call_back_name
  end interface
end module kim_call_back_name_f_module

! free functions to implement kim_call_back_name_module

subroutine kim_call_back_name_string(call_back_name, name_string)
  use, intrinsic :: iso_c_binding
  use kim_call_back_name_module, only : kim_call_back_name_type
  use kim_call_back_name_f_module, only : call_back_name_string
  implicit none
  type(kim_call_back_name_type), intent(in), value :: call_back_name
  character(len=*), intent(out) :: name_string

  type(c_ptr) :: p
  character(len=len(name_string)), pointer :: fp
  integer(c_int) :: null_index

  p = call_back_name_string(call_back_name)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  name_string = fp(1:null_index)
end subroutine kim_call_back_name_string

subroutine kim_call_back_name_get_number_of_call_backs(number_of_call_backs)
  use, intrinsic :: iso_c_binding
  use kim_call_back_name_f_module, only : get_number_of_call_backs
  implicit none
  integer(c_int), intent(out) :: number_of_call_backs

  call get_number_of_call_backs(number_of_call_backs)
end subroutine kim_call_back_name_get_number_of_call_backs

subroutine kim_call_back_name_get_call_back_name(index, call_back_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_call_back_name_module, only : kim_call_back_name_type
  use kim_call_back_name_f_module, only : get_call_back_name
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_call_back_name_type), intent(out) :: call_back_name
  integer(c_int), intent(out) :: ierr

  ierr = get_call_back_name(index-1, call_back_name)
end subroutine kim_call_back_name_get_call_back_name
