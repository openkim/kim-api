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


module kim_compute_arguments_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_compute_arguments_handle_type, &
    kim_compute_arguments_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_compute_arguments_get_argument_support_status, &
    kim_compute_arguments_get_callback_support_status, &
    kim_compute_arguments_set_argument_pointer, &
    kim_compute_arguments_set_callback_pointer, &
    kim_compute_arguments_are_all_required_present, &
    kim_compute_arguments_set_simulator_buffer_pointer, &
    kim_compute_arguments_get_simulator_buffer_pointer, &
    kim_compute_arguments_string, &
    kim_compute_arguments_set_log_id, &
    kim_compute_arguments_push_log_verbosity, &
    kim_compute_arguments_pop_log_verbosity

  type, bind(c) :: kim_compute_arguments_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_compute_arguments_handle_type

  type(kim_compute_arguments_handle_type), protected, save &
    :: kim_compute_arguments_null_handle

  interface operator (.eq.)
    logical function kim_compute_arguments_handle_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: left
      type(kim_compute_arguments_handle_type), intent(in) :: right
    end function kim_compute_arguments_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_compute_arguments_handle_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: left
      type(kim_compute_arguments_handle_type), intent(in) :: right
    end function kim_compute_arguments_handle_not_equal
  end interface operator (.ne.)

  interface kim_compute_arguments_set_argument_pointer
    subroutine kim_compute_arguments_set_argument_pointer_int0( &
      compute_arguments_handle, compute_argument_name, int0, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), target :: int0
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_argument_pointer_int0

    subroutine kim_compute_arguments_set_argument_pointer_int1( &
      compute_arguments_handle, &
      compute_argument_name, int1, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), target :: int1(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_argument_pointer_int1

    subroutine kim_compute_arguments_set_argument_pointer_int2( &
      compute_arguments_handle, compute_argument_name, int2, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), target :: int2(:,:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_argument_pointer_int2

    subroutine kim_compute_arguments_set_argument_pointer_double0( &
      compute_arguments_handle, compute_argument_name, double0, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      real(c_double), intent(in), target :: double0
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_argument_pointer_double0

    subroutine kim_compute_arguments_set_argument_pointer_double1( &
      compute_arguments_handle, compute_argument_name, double1, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      real(c_double), intent(in), target :: double1(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_argument_pointer_double1

    subroutine kim_compute_arguments_set_argument_pointer_double2( &
      compute_arguments_handle, compute_argument_name, double2, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      real(c_double), intent(in), target :: double2(:,:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_argument_pointer_double2
  end interface kim_compute_arguments_set_argument_pointer

  interface
    subroutine kim_compute_arguments_get_argument_support_status( &
      compute_arguments_handle, compute_argument_name, support_status, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      type(kim_support_status_type), intent(out) :: support_status
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_get_argument_support_status

    subroutine kim_compute_arguments_get_callback_support_status( &
      compute_arguments_handle, compute_callback_name, support_status, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_callback_name_module, only : &
        kim_compute_callback_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_callback_name_type), intent(in), value :: &
        compute_callback_name
      type(kim_support_status_type), intent(out) :: support_status
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_get_callback_support_status

    subroutine kim_compute_arguments_set_callback_pointer( &
      compute_arguments_handle, compute_callback_name, language_name, fptr, &
      data_object, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_callback_name_module, only : &
        kim_compute_callback_name_type
      use kim_language_name_module, only : kim_language_name_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_compute_callback_name_type), intent(in), value :: &
        compute_callback_name
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      type(c_ptr), intent(in), value :: data_object
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_set_callback_pointer

    subroutine kim_compute_arguments_are_all_required_present( &
      compute_arguments_handle, result_value, ierr)
      ! name is not consistent with c/c++ due to Fortran 2003
      ! identifier length limits
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      integer(c_int), intent(out) :: result_value
      integer(c_int), intent(out) :: ierr
    end subroutine kim_compute_arguments_are_all_required_present

    subroutine kim_compute_arguments_set_simulator_buffer_pointer( &
      compute_arguments_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_compute_arguments_set_simulator_buffer_pointer

    subroutine kim_compute_arguments_get_simulator_buffer_pointer( &
      compute_arguments_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_compute_arguments_get_simulator_buffer_pointer

    subroutine kim_compute_arguments_string(compute_arguments_handle, string)
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      character(len=*, kind=c_char), intent(out) :: string
    end subroutine kim_compute_arguments_string

    subroutine kim_compute_arguments_set_log_id(compute_arguments_handle, &
      log_id)
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      character(len=*, kind=c_char), intent(in) :: log_id
    end subroutine kim_compute_arguments_set_log_id

    subroutine kim_compute_arguments_push_log_verbosity( &
      compute_arguments_handle, log_verbosity)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
      type(kim_log_verbosity_type), intent(in) :: log_verbosity
    end subroutine kim_compute_arguments_push_log_verbosity

    subroutine kim_compute_arguments_pop_log_verbosity(compute_arguments_handle)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_compute_arguments_handle_type
      implicit none
      type(kim_compute_arguments_handle_type), intent(in) :: &
        compute_arguments_handle
    end subroutine kim_compute_arguments_pop_log_verbosity
  end interface
end module kim_compute_arguments_module
