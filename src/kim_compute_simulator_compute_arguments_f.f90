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


module kim_compute_simulator_compute_arguments_f_module
  implicit none
  private

  public &
    get_neigh_object, &
    get_neigh, &
    process_dedr, &
    process_d2edr2, &
    get_data_int, &
    get_data_double, &
    get_compute, &
    get_process_dedr_compute, &
    get_process_d2edr2_compute, &
    get_size

  interface
    subroutine get_neigh_object(arguments, ptr) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_get_neighObject")
      use, intrinsic :: iso_c_binding
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(c_ptr), intent(out) :: ptr
    end subroutine get_neigh_object

    integer(c_int) function get_neigh(arguments, neighbor_list_index, &
      particle_number, number_of_neighbors, neighbors_of_particle) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_get_neigh")
      use, intrinsic :: iso_c_binding
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      integer(c_int), intent(in), value :: neighbor_list_index
      integer(c_int), intent(in), value :: particle_number
      integer(c_int), intent(out) :: number_of_neighbors
      type(c_ptr), intent(out) :: neighbors_of_particle
    end function get_neigh

    integer(c_int) function process_dedr(arguments, de, r, dx, i, j) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_process_dEdr")
      use, intrinsic :: iso_c_binding
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      real(c_double), intent(in), value :: de
      real(c_double), intent(in), value :: r
      real(c_double), intent(in) :: dx
      real(c_double), intent(in), value :: i
      real(c_double), intent(in), value :: j
    end function process_dedr

    integer(c_int) function process_d2edr2(arguments, de, r, dx, i, j) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_process_d2Edr2")
      use, intrinsic :: iso_c_binding
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      real(c_double), intent(in), value :: de
      type(c_ptr), intent(in), value :: r
      type(c_ptr), intent(in), value :: dx
      type(c_ptr), intent(in), value :: i
      type(c_ptr), intent(in), value :: j
    end function process_d2edr2

    integer(c_int) function get_data_int(arguments, argument_name, ptr) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_get_data_int")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
    end function get_data_int

    integer(c_int) function get_data_double(arguments, argument_name, ptr) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_get_data_double")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
    end function get_data_double

    integer(c_int) function get_compute(arguments, argument_name, flag) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_get_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: flag
    end function get_compute

    subroutine get_process_dedr_compute(arguments, flag) &
      bind(c, &
      name="KIM_COMPUTE_SimulatorComputeArguments_get_process_dEdr_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      integer(c_int), intent(out) :: flag
    end subroutine get_process_dedr_compute

    subroutine get_process_d2edr2_compute(arguments, flag) &
      bind(c, &
      name="KIM_COMPUTE_SimulatorComputeArguments_get_process_d2Edr2_compute")
      use, intrinsic :: iso_c_binding
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      integer(c_int), intent(out) :: flag
    end subroutine get_process_d2edr2_compute

    integer(c_int) function get_size(arguments, argument_name, size) &
      bind(c, name="KIM_COMPUTE_SimulatorComputeArguments_get_size")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_compute_simulator_compute_arguments_module, only : &
        kim_compute_simulator_compute_arguments_type
      implicit none
      type(kim_compute_simulator_compute_arguments_type), intent(in) :: &
        arguments
      type(kim_compute_argument_name_type), intent(in), value :: argument_name
      integer(c_int), intent(out) :: size
    end function get_size

  end interface
end module kim_compute_simulator_compute_arguments_f_module


! free functions to implement kim_compute_simulator_compute_arguments_module

subroutine kim_compute_simulator_compute_arguments_get_data_int0(arguments, &
  argument_name, int0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_data_int
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out), pointer :: int0
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_int(arguments, argument_name, p)
  call c_f_pointer(p, int0)
end subroutine kim_compute_simulator_compute_arguments_get_data_int0

subroutine kim_compute_simulator_compute_arguments_get_data_int1(arguments, &
  argument_name, extent1, int1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_data_int
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(out), pointer :: int1(:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_int(arguments, argument_name, p)
  call c_f_pointer(p, int1, [extent1])
end subroutine kim_compute_simulator_compute_arguments_get_data_int1

subroutine kim_compute_simulator_compute_arguments_get_data_int2(arguments, &
  argument_name, extent1, extent2, int2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_data_int
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), value :: extent2
  integer(c_int), intent(out), pointer :: int2(:,:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_int(arguments, argument_name, p)
  call c_f_pointer(p, int2, [extent1, extent2])
end subroutine kim_compute_simulator_compute_arguments_get_data_int2

subroutine kim_compute_simulator_compute_arguments_get_data_double0(arguments, &
  argument_name, double0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_data_double
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  real(c_double), intent(out), pointer :: double0
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_double(arguments, argument_name, p)
  call c_f_pointer(p, double0)
end subroutine kim_compute_simulator_compute_arguments_get_data_double0

subroutine kim_compute_simulator_compute_arguments_get_data_double1(arguments, &
  argument_name, extent1, double1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_data_double
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  real(c_double), intent(out), pointer :: double1(:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_double(arguments, argument_name, p)
  call c_f_pointer(p, double1, [extent1])
end subroutine kim_compute_simulator_compute_arguments_get_data_double1

subroutine kim_compute_simulator_compute_arguments_get_data_double2(arguments, &
  argument_name, extent1, extent2, double2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_data_double
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), value :: extent2
  real(c_double), intent(out), pointer :: double2(:,:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_double(arguments, argument_name, p)
  call c_f_pointer(p, double2, [extent1, extent2])
end subroutine kim_compute_simulator_compute_arguments_get_data_double2

subroutine kim_compute_simulator_compute_arguments_get_compute(arguments, &
  argument_name, flag, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_compute
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out) :: flag
  integer(c_int), intent(out) :: ierr

  ierr = get_compute(arguments, argument_name, flag)
end subroutine kim_compute_simulator_compute_arguments_get_compute

subroutine kim_compute_simulator_compute_arguments_get_p_dedr_compute( &
  arguments, flag)
  use, intrinsic :: iso_c_binding
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : &
    get_process_dedr_compute
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  integer(c_int), intent(out) :: flag

  call get_process_dedr_compute(arguments, flag)
end subroutine kim_compute_simulator_compute_arguments_get_p_dedr_compute

subroutine kim_compute_simulator_compute_arguments_get_p_d2edr2_compute( &
  arguments, flag)
  use, intrinsic :: iso_c_binding
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : &
    get_process_d2edr2_compute
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  integer(c_int), intent(out) :: flag

  call get_process_d2edr2_compute(arguments, flag)
end subroutine kim_compute_simulator_compute_arguments_get_p_d2edr2_compute

subroutine kim_compute_simulator_compute_arguments_get_size(arguments, &
  argument_name, size, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : &
    kim_compute_argument_name_type
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_size
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(kim_compute_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out) :: size
  integer(c_int), intent(out) :: ierr

  ierr = get_size(arguments, argument_name, size)
end subroutine kim_compute_simulator_compute_arguments_get_size

subroutine kim_compute_simulator_compute_arguments_get_neigh_object(arguments, &
  ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_neigh_object
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  type(c_ptr), intent(out) :: ptr

  call get_neigh_object(arguments, ptr)
end subroutine kim_compute_simulator_compute_arguments_get_neigh_object

subroutine kim_compute_simulator_compute_arguments_get_neigh(arguments, &
  neighbor_list_index, &
  particle_number, number_of_neighbors, neighbors_of_particle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : get_neigh
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  integer(c_int), intent(in), value :: neighbor_list_index
  integer(c_int), intent(in), value :: particle_number
  integer(c_int), intent(out) :: number_of_neighbors
  integer(c_int), intent(out), pointer :: neighbors_of_particle(:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_neigh(arguments, neighbor_list_index-1, particle_number, &
    number_of_neighbors, p)
  call c_f_pointer(p, neighbors_of_particle, [number_of_neighbors])
end subroutine kim_compute_simulator_compute_arguments_get_neigh

subroutine kim_compute_simulator_compute_arguments_process_dedr(arguments, &
  de, r, dx, i, j, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : process_dedr
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  real(c_double), intent(in), value :: de
  real(c_double), intent(in), value :: r
  real(c_double), intent(in) :: dx
  real(c_double), intent(in), value :: i
  real(c_double), intent(in), value :: j
  integer(c_int), intent(out) :: ierr

  ierr = process_dedr(arguments, de, r, dx, i, j)
end subroutine kim_compute_simulator_compute_arguments_process_dedr

subroutine kim_compute_simulator_compute_arguments_process_d2edr2(arguments, &
  de, r, dx, i, j, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_simulator_compute_arguments_module, only : &
    kim_compute_simulator_compute_arguments_type
  use kim_compute_simulator_compute_arguments_f_module, only : process_d2edr2
  implicit none
  type(kim_compute_simulator_compute_arguments_type), intent(in) :: arguments
  real(c_double), intent(in), value :: de
  type(c_ptr), intent(in), value :: r
  type(c_ptr), intent(in), value :: dx
  type(c_ptr), intent(in), value :: i
  type(c_ptr), intent(in), value :: j
  integer(c_int), intent(out) :: ierr

  ierr = process_d2edr2(arguments, de, r, dx, i, j)
end subroutine kim_compute_simulator_compute_arguments_process_d2edr2
