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


module kim_compute_model_compute_arguments_f_module
  implicit none
  private

  public &
    set_get_neigh, &
    set_neigh_object, &
    set_process_dedr, &
    set_process_d2edr2, &
    set_data, &
    set_compute, &
    set_process_dedr_compute, &
    set_process_d2edr2_compute, &
    get_size

  interface
    subroutine set_get_neigh(arguments, language_name, fptr) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_get_neigh")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_compute_model_compute_arguments_module, only :&
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_get_neigh

    subroutine set_neigh_object(arguments, ptr) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_neighObject")
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_neigh_object

    subroutine set_process_dedr(arguments, fptr) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_process_dEdr")
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_process_dedr

    subroutine set_process_d2edr2(arguments, fptr) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_process_d2Edr2")
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(c_funptr), intent(in), value :: fptr
    end subroutine set_process_d2edr2

    integer(c_int) function set_data(arguments, argument_name, extent, ptr) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_data")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
    end function set_data

    integer(c_int) function set_compute(arguments, argument_name, flag) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(in), value :: flag
    end function set_compute

    subroutine set_process_dedr_compute(arguments, flag) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_process_dEdr_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      integer(c_int), intent(in), value :: flag
    end subroutine set_process_dedr_compute

    subroutine set_process_d2edr2_compute(arguments, flag) &
      bind(c, &
      name="KIM_COMPUTE_ModelComputeArguments_set_process_d2Edr2_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      integer(c_int), intent(in), value :: flag
    end subroutine set_process_d2edr2_compute

    integer(c_int) function get_size(arguments, argument_name, size) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_get_size")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_model_compute_arguments_module, only : &
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(in) :: arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: size
    end function get_size

  end interface
end module kim_compute_model_compute_arguments_f_module


! free functions to implement kim_compute_model_compute_arguments_module

subroutine kim_compute_model_compute_arguments_set_get_neigh(arguments, &
  language_name, fptr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_get_neigh
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr

  call set_get_neigh(arguments, language_name, fptr)
end subroutine kim_compute_model_compute_arguments_set_get_neigh

subroutine kim_compute_model_compute_arguments_set_neigh_object(arguments, ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_neigh_object
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(c_ptr), intent(in), value :: ptr

  call set_neigh_object(arguments, ptr)
end subroutine kim_compute_model_compute_arguments_set_neigh_object

subroutine kim_compute_model_compute_arguments_set_process_dedr(arguments, &
  ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_process_dedr
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(c_ptr), intent(in), value :: ptr

  call set_process_dedr(arguments, ptr)
end subroutine kim_compute_model_compute_arguments_set_process_dedr

subroutine kim_compute_model_compute_arguments_set_process_d2edr2(arguments, &
  ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_process_d2edr2
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(c_ptr), intent(in), value :: ptr

  call set_process_d2edr2(arguments, ptr)
end subroutine kim_compute_model_compute_arguments_set_process_d2edr2

subroutine kim_compute_model_compute_arguments_set_data(arguments, &
  argument_name, extent, ptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent
  type(c_ptr), intent(in), value :: ptr
  integer(c_int), intent(out) :: ierr

  ierr = set_data(arguments, argument_name, extent, ptr)
end subroutine kim_compute_model_compute_arguments_set_data

subroutine kim_compute_model_compute_arguments_set_compute(arguments, &
  argument_name, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_compute
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: flag
  integer(c_int), intent(out) :: ierr

  ierr = set_compute(arguments, argument_name, flag)
end subroutine kim_compute_model_compute_arguments_set_compute

subroutine kim_compute_model_compute_arguments_set_process_dedr_compute( &
  arguments, flag)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : &
    set_process_dedr_compute
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  integer(c_int), intent(in), value :: flag

  call set_process_dedr_compute(arguments, flag)
end subroutine kim_compute_model_compute_arguments_set_process_dedr_compute

subroutine kim_compute_model_compute_arguments_set_process_d2edr2_compute( &
  arguments, flag)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : &
    set_process_d2edr2_compute
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  integer(c_int), intent(in), value :: flag

  call set_process_d2edr2_compute(arguments, flag)
end subroutine kim_compute_model_compute_arguments_set_process_d2edr2_compute

subroutine kim_compute_model_compute_arguments_get_size(arguments, &
  argument_name, size, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : get_size
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out) :: size
  integer(c_int), intent(out) :: ierr

  ierr = get_size(arguments, argument_name, size)
end subroutine kim_compute_model_compute_arguments_get_size
