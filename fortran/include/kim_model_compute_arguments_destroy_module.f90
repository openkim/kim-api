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


module kim_model_compute_arguments_destroy_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_compute_arguments_destroy_handle_type, &
    kim_model_compute_arguments_destroy_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_compute_arguments_destroy_get_model_buffer_pointer, &
    kim_model_compute_arguments_destroy_log_entry, &
    kim_model_compute_arguments_destroy_string

  type, bind(c) :: kim_model_compute_arguments_destroy_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_compute_arguments_destroy_handle_type

  type(kim_model_compute_arguments_destroy_handle_type), protected, save &
    :: kim_model_compute_arguments_destroy_null_handle

  interface operator (.eq.)
    logical function kim_model_compute_arguments_destroy_handle_equal(left, &
      right)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_destroy_handle_type
      implicit none
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: left
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: right
    end function kim_model_compute_arguments_destroy_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_model_compute_arguments_destroy_handle_not_equal( &
      left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_destroy_handle_type
      implicit none
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: left
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: right
    end function kim_model_compute_arguments_destroy_handle_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_model_compute_arguments_destroy_get_model_buffer_pointer( &
      model_destroy_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_destroy_handle_type
      implicit none
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: &
        model_destroy_handle
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_compute_arguments_destroy_get_model_buffer_pointer

    subroutine kim_model_compute_arguments_destroy_log_entry( &
      model_destroy_handle, log_verbosity, message, line_number, file_name)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_compute_arguments_destroy_handle_type
      implicit none
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: &
        model_destroy_handle
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*, kind=c_char), intent(in) :: message
      integer(c_int), intent(in), value :: line_number
      character(len=*, kind=c_char), intent(in) :: file_name
    end subroutine kim_model_compute_arguments_destroy_log_entry

    subroutine kim_model_compute_arguments_destroy_string( &
      model_destroy_handle, string)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_destroy_handle_type
      implicit none
      type(kim_model_compute_arguments_destroy_handle_type), intent(in) :: &
        model_destroy_handle
      character(len=*, kind=c_char), intent(out) :: string
    end subroutine kim_model_compute_arguments_destroy_string
end interface
end module kim_model_compute_arguments_destroy_module
