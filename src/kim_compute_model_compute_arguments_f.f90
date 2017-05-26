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
    get_argument_attribute, &
    set_neigh, &
    set_process_dedr, &
    set_process_d2edr2, &
    set_data_int, &
    set_data_double, &
    set_compute, &
    set_process_dedr_compute, &
    set_process_d2edr2_compute, &
    get_size

  interface
    subroutine get_argument_attribute(arguments, argument_name, &
      argument_attribute) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_get_argument_attribute")
      use, intrinsic :: iso_c_binding
      use kim_compute_model_compute_arguments_module, only :&
        kim_compute_model_compute_arguments_type
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_argument_attribute_module, only : &
        kim_compute_argument_attribute_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(in) :: arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(kim_compute_argument_attribute_type), intent(out) :: &
        argument_attribute
    end subroutine get_argument_attribute

    subroutine set_neigh(arguments, language_name, fptr, data_object) &
      bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_neigh")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_compute_model_compute_arguments_module, only :&
        kim_compute_model_compute_arguments_type
      implicit none
      type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      type(c_ptr), intent(in), value :: data_object
    end subroutine set_neigh

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

    integer(c_int) function set_data_int(arguments, argument_name, extent, &
      ptr) bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_data_int")
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
    end function set_data_int

    integer(c_int) function set_data_double(arguments, argument_name, extent, &
      ptr) bind(c, name="KIM_COMPUTE_ModelComputeArguments_set_data_double")
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
    end function set_data_double

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

subroutine kim_compute_model_compute_arguments_get_argument_attribute( &
  arguments, argument_name, argument_attribute)
  use, intrinsic :: iso_c_binding
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_argument_attribute_module, only : &
    kim_compute_argument_attribute_type
  use kim_compute_model_compute_arguments_f_module, only : &
    get_argument_attribute
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(in) :: &
    arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  type(kim_compute_argument_attribute_type), intent(out) :: &
    argument_attribute

  call get_argument_attribute(arguments, argument_name, argument_attribute)
end subroutine kim_compute_model_compute_arguments_get_argument_attribute

subroutine kim_compute_model_compute_arguments_set_neigh(arguments, &
  language_name, fptr, data_object)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : &
    kim_language_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_neigh
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  type(c_ptr), intent(in), value :: data_object

  call set_neigh(arguments, language_name, fptr, data_object)
end subroutine kim_compute_model_compute_arguments_set_neigh

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

subroutine kim_compute_model_compute_arguments_set_data_int0(arguments, &
  argument_name, int0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data_int
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), target :: int0
  integer(c_int), intent(out) :: ierr

  ierr = set_data_int(arguments, argument_name, 1, c_loc(int0))
end subroutine kim_compute_model_compute_arguments_set_data_int0

subroutine kim_compute_model_compute_arguments_set_data_int1(arguments, &
  argument_name, extent1, int1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data_int
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), target :: int1(extent1)
  integer(c_int), intent(out) :: ierr

  ierr = set_data_int(arguments, argument_name, extent1, c_loc(int1))
end subroutine kim_compute_model_compute_arguments_set_data_int1

subroutine kim_compute_model_compute_arguments_set_data_int2(arguments, &
  argument_name, extent1, extent2, int2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data_int
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), value :: extent2
  integer(c_int), intent(in), target :: int2(extent1,extent2)
  integer(c_int), intent(out) :: ierr

  ierr = set_data_int(arguments, argument_name, extent1*extent2, c_loc(int2))
end subroutine kim_compute_model_compute_arguments_set_data_int2

subroutine kim_compute_model_compute_arguments_set_data_double0(arguments, &
  argument_name, double0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data_double
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  real(c_double), intent(in), target :: double0
  integer(c_int), intent(out) :: ierr

  ierr = set_data_double(arguments, argument_name, 1, c_loc(double0))
end subroutine kim_compute_model_compute_arguments_set_data_double0

subroutine kim_compute_model_compute_arguments_set_data_double1(arguments, &
  argument_name, extent1, double1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data_double
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  real(c_double), intent(in), target :: double1(extent1)
  integer(c_int), intent(out) :: ierr

  ierr = set_data_double(arguments, argument_name, extent1, c_loc(double1))
end subroutine kim_compute_model_compute_arguments_set_data_double1

subroutine kim_compute_model_compute_arguments_set_data_double2(arguments, &
  argument_name, extent1, extent2, double2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_model_compute_arguments_module, only : &
    kim_compute_model_compute_arguments_type
  use kim_compute_model_compute_arguments_f_module, only : set_data_double
  implicit none
  type(kim_compute_model_compute_arguments_type), intent(inout) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), value :: extent2
  real(c_double), intent(in), target :: double2(extent1, extent2)
  integer(c_int), intent(out) :: ierr

  ierr = set_data_double(arguments, argument_name, extent1*extent2, &
    c_loc(double2))
end subroutine kim_compute_model_compute_arguments_set_data_double2

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
