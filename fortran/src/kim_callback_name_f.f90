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


module kim_callback_name_f_module
  implicit none
  private

  public &
    callback_name_string, &
    get_number_of_callbacks, &
    get_callback_name

  interface
    type(c_ptr) function callback_name_string(callback_name) &
      bind(c, name="KIM_CallbackNameString")
      use, intrinsic :: iso_c_binding
      use kim_callback_name_module, only : kim_callback_name_type
      implicit none
      type(kim_callback_name_type), intent(in), value :: callback_name
    end function callback_name_string

    subroutine get_number_of_callbacks(number_of_callbacks) &
      bind(c, name="KIM_CALLBACK_NAME_GetNumberOfCallbacks")
      use, intrinsic :: iso_c_binding
      integer(c_int), intent(out) :: number_of_callbacks
    end subroutine get_number_of_callbacks

    integer(c_int) function get_callback_name(index, callback_name) &
      bind(c, name="KIM_CALLBACK_NAME_GetCallbackName")
      use, intrinsic :: iso_c_binding
      use kim_callback_name_module, only : kim_callback_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_callback_name_type), intent(out) :: callback_name
    end function get_callback_name
  end interface
end module kim_callback_name_f_module

! free functions to implement kim_callback_name_module

logical function kim_callback_name_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_module, only : kim_callback_name_type
  implicit none
  type(kim_callback_name_type), intent(in) :: left
  type(kim_callback_name_type), intent(in) :: right

  kim_callback_name_equal &
    = (left%callback_name_id .eq. right%callback_name_id)
end function kim_callback_name_equal

logical function kim_callback_name_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_module, only : kim_callback_name_type
  use kim_callback_name_module, only : operator(.eq.)
  implicit none
  type(kim_callback_name_type), intent(in) :: left
  type(kim_callback_name_type), intent(in) :: right

  kim_callback_name_not_equal = .not. (left .eq. right)
end function kim_callback_name_not_equal

subroutine kim_callback_name_string(callback_name, string)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_module, only : kim_callback_name_type
  use kim_callback_name_f_module, only : callback_name_string
  implicit none
  type(kim_callback_name_type), intent(in), value :: callback_name
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)+1), pointer :: fp
  integer(c_int) :: null_index

  p = callback_name_string(callback_name)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_callback_name_string

subroutine kim_callback_name_get_number_of_callbacks(number_of_callbacks)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_f_module, only : get_number_of_callbacks
  implicit none
  integer(c_int), intent(out) :: number_of_callbacks

  call get_number_of_callbacks(number_of_callbacks)
end subroutine kim_callback_name_get_number_of_callbacks

subroutine kim_callback_name_get_callback_name(index, callback_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_module, only : kim_callback_name_type
  use kim_callback_name_f_module, only : get_callback_name
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_callback_name_type), intent(out) :: callback_name
  integer(c_int), intent(out) :: ierr

  ierr = get_callback_name(index-1, callback_name)
end subroutine kim_callback_name_get_callback_name
