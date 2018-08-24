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


module kim_log_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_log_handle_type, &
    kim_log_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_log_create, &
    kim_log_destroy, &
    kim_log_get_id, &
    kim_log_set_id, &
    kim_log_push_verbosity, &
    kim_log_pop_verbosity, &
    kim_log_log_entry

  type, bind(c) :: kim_log_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_log_handle_type

  type(kim_log_handle_type), protected, save &
    :: kim_log_null_handle

  interface operator (.eq.)
    logical function kim_log_handle_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: left
      type(kim_log_handle_type), intent(in) :: right
    end function kim_log_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_log_handle_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: left
      type(kim_log_handle_type), intent(in) :: right
    end function kim_log_handle_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_log_create(log_handle, ierr)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(out) :: log_handle
      integer(c_int), intent(out) :: ierr
    end subroutine kim_log_create

    subroutine kim_log_destroy(log_handle)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(inout) :: log_handle
    end subroutine kim_log_destroy

    subroutine kim_log_get_id(log_handle, id_string)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: log_handle
      character(len=*, kind=c_char), intent(out) :: id_string
    end subroutine kim_log_get_id

    subroutine kim_log_set_id(log_handle, id_string)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: log_handle
      character(len=*, kind=c_char), intent(in) :: id_string
    end subroutine kim_log_set_id

    subroutine kim_log_push_verbosity(log_handle, log_verbosity)
      use, intrinsic :: iso_c_binding
      use :: kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: log_handle
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
    end subroutine kim_log_push_verbosity

    subroutine kim_log_pop_verbosity(log_handle)
      use, intrinsic :: iso_c_binding
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: log_handle
    end subroutine kim_log_pop_verbosity

    subroutine kim_log_log_entry(log_handle, log_verbosity, message, &
      line_number, file_name)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_log_handle_type
      implicit none
      type(kim_log_handle_type), intent(in) :: log_handle
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*, kind=c_char), intent(in) :: message
      integer(c_int), intent(in), value :: line_number
      character(len=*, kind=c_char), intent(in) :: file_name
    end subroutine kim_log_log_entry
  end interface
end module kim_log_module
