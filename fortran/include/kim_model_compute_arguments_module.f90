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


module kim_model_compute_arguments_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_compute_arguments_handle_type, &
    kim_model_compute_arguments_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_compute_arguments_get_neighbor_list, &
    kim_model_compute_arguments_process_dedr_term, &
    kim_model_compute_arguments_process_d2edr2_term, &
    kim_model_compute_arguments_get_argument_pointer, &
    kim_model_compute_arguments_is_callback_present, &
    kim_model_compute_arguments_set_model_buffer_pointer, &
    kim_model_compute_arguments_get_model_buffer_pointer, &
    kim_model_compute_arguments_log_entry, &
    kim_model_compute_arguments_string

  type, bind(c) :: kim_model_compute_arguments_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_compute_arguments_handle_type

  type(kim_model_compute_arguments_handle_type), protected, save &
    :: kim_model_compute_arguments_null_handle

  interface operator (.eq.)
    logical function kim_model_compute_arguments_handle_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: left
      type(kim_model_compute_arguments_handle_type), intent(in) :: right
    end function kim_model_compute_arguments_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_model_compute_arguments_handle_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: left
      type(kim_model_compute_arguments_handle_type), intent(in) :: right
    end function kim_model_compute_arguments_handle_not_equal
  end interface operator (.ne.)

  interface kim_model_compute_arguments_get_argument_pointer
    subroutine kim_model_compute_arguments_get_argument_pointer_int0( &
      model_compute_arguments_handle, compute_argument_name, int0, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(out), pointer :: int0
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_argument_pointer_int0

    subroutine kim_model_compute_arguments_get_argument_pointer_int1( &
      model_compute_arguments_handle, compute_argument_name, extent1, int1, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), value :: extent1
      integer(c_int), intent(out), pointer :: int1(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_argument_pointer_int1

    subroutine kim_model_compute_arguments_get_argument_pointer_int2( &
      model_compute_arguments_handle, compute_argument_name, extent1, extent2, &
      int2, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), value :: extent1
      integer(c_int), intent(in), value :: extent2
      integer(c_int), intent(out), pointer :: int2(:,:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_argument_pointer_int2

    subroutine kim_model_compute_arguments_get_argument_pointer_double0( &
      model_compute_arguments_handle, compute_argument_name, double0, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      real(c_double), intent(out), pointer :: double0
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_argument_pointer_double0

    subroutine kim_model_compute_arguments_get_argument_pointer_double1( &
      model_compute_arguments_handle, compute_argument_name, extent1, double1, &
      ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), value :: extent1
      real(c_double), intent(out), pointer :: double1(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_argument_pointer_double1

    subroutine kim_model_compute_arguments_get_argument_pointer_double2( &
      model_compute_arguments_handle, compute_argument_name, extent1, extent2, &
      double2, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      integer(c_int), intent(in), value :: extent1
      integer(c_int), intent(in), value :: extent2
      real(c_double), intent(out), pointer :: double2(:,:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_argument_pointer_double2
  end interface kim_model_compute_arguments_get_argument_pointer

  interface
    subroutine kim_model_compute_arguments_get_neighbor_list( &
      model_compute_arguments_handle, neighbor_list_index, particle_number, &
      number_of_neighbors, neighbors_of_particle, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      integer(c_int), intent(in), value :: neighbor_list_index
      integer(c_int), intent(in), value :: particle_number
      integer(c_int), intent(out) :: number_of_neighbors
      integer(c_int), intent(out), pointer :: neighbors_of_particle(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_get_neighbor_list

    subroutine kim_model_compute_arguments_process_dedr_term( &
      model_compute_arguments_handle, de, r, dx, i, j, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      real(c_double), intent(in), value :: de
      real(c_double), intent(in), value :: r
      real(c_double), intent(in) :: dx(:)
      integer(c_int), intent(in), value :: i
      integer(c_int), intent(in), value :: j
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_process_dedr_term

    subroutine kim_model_compute_arguments_process_d2edr2_term( &
      model_compute_arguments_handle, de, r, dx, i, j, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      real(c_double), intent(in), value :: de
      real(c_double), intent(in) :: r(:)
      real(c_double), intent(in) :: dx(:,:)
      integer(c_int), intent(in) :: i(:)
      integer(c_int), intent(in) :: j(:)
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_process_d2edr2_term

    subroutine kim_model_compute_arguments_is_callback_present( &
      model_compute_arguments_handle, compute_callback_name, present, ierr)
      use, intrinsic :: iso_c_binding
      use kim_compute_callback_name_module, only : &
        kim_compute_callback_name_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_compute_callback_name_type), intent(in), value :: &
        compute_callback_name
      integer(c_int), intent(out) :: present
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_compute_arguments_is_callback_present

    subroutine kim_model_compute_arguments_set_model_buffer_pointer( &
      model_compute_arguments_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(inout) :: &
        model_compute_arguments_handle
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_compute_arguments_set_model_buffer_pointer

    subroutine kim_model_compute_arguments_get_model_buffer_pointer( &
      model_compute_arguments_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(c_ptr), intent(out) :: ptr
    end subroutine kim_model_compute_arguments_get_model_buffer_pointer

    subroutine kim_model_compute_arguments_log_entry( &
      model_compute_arguments_handle, log_verbosity, message, line_number, &
      file_name)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*, kind=c_char), intent(in) :: message
      integer(c_int), intent(in), value :: line_number
      character(len=*, kind=c_char), intent(in) :: file_name
    end subroutine kim_model_compute_arguments_log_entry

    subroutine kim_model_compute_arguments_string( &
      model_compute_arguments_handle, string)
      use, intrinsic :: iso_c_binding
      import kim_model_compute_arguments_handle_type
      implicit none
      type(kim_model_compute_arguments_handle_type), intent(in) :: &
        model_compute_arguments_handle
      character(len=*, kind=c_char), intent(out) :: string
    end subroutine kim_model_compute_arguments_string
end interface
end module kim_model_compute_arguments_module
