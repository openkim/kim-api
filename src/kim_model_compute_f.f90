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


module kim_model_compute_f_module
  implicit none
  private

  public &
    get_neigh, &
    process_dedr, &
    process_d2edr2, &
    get_data_int, &
    get_data_double, &
    is_call_back_present, &
    get_model_buffer, &
    log, &
    model_compute_string

  interface
    integer(c_int) function get_neigh(model_compute, neighbor_list_index, &
      particle_number, number_of_neighbors, neighbors_of_particle) &
      bind(c, name="KIM_ModelCompute_get_neigh")
      use, intrinsic :: iso_c_binding
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      integer(c_int), intent(in), value :: neighbor_list_index
      integer(c_int), intent(in), value :: particle_number
      integer(c_int), intent(out) :: number_of_neighbors
      type(c_ptr), intent(out) :: neighbors_of_particle
    end function get_neigh

    integer(c_int) function process_dedr(model_compute, de, r, dx, i, j) &
      bind(c, name="KIM_ModelCompute_process_dEdr")
      use, intrinsic :: iso_c_binding
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      real(c_double), intent(in), value :: de
      real(c_double), intent(in), value :: r
      real(c_double), intent(in) :: dx
      real(c_double), intent(in), value :: i
      real(c_double), intent(in), value :: j
    end function process_dedr

    integer(c_int) function process_d2edr2(model_compute, de, r, dx, i, j) &
      bind(c, name="KIM_ModelCompute_process_d2Edr2")
      use, intrinsic :: iso_c_binding
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      real(c_double), intent(in), value :: de
      type(c_ptr), intent(in), value :: r
      type(c_ptr), intent(in), value :: dx
      type(c_ptr), intent(in), value :: i
      type(c_ptr), intent(in), value :: j
    end function process_d2edr2

    integer(c_int) function get_data_int(model_compute, argument_name, ptr) &
      bind(c, name="KIM_ModelCompute_get_data_int")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
    end function get_data_int

    integer(c_int) function get_data_double(model_compute, argument_name, ptr) &
      bind(c, name="KIM_ModelCompute_get_data_double")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(c_ptr), intent(out) :: ptr
    end function get_data_double

    integer(c_int) function is_call_back_present(model_compute, &
      call_back_name, present) &
      bind(c, name="KIM_ModelCompute_is_call_back_present")
      use, intrinsic :: iso_c_binding
      use kim_call_back_name_module, only : kim_call_back_name_type
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      type(kim_call_back_name_type), intent(in), value :: call_back_name
      integer(c_int), intent(out) :: present
    end function is_call_back_present

    subroutine get_model_buffer(model_compute, ptr) &
      bind(c, name="KIM_ModelCompute_get_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      type(c_ptr), intent(out) :: ptr
    end subroutine get_model_buffer

    subroutine log(model_compute, log_verbosity, message, line_number, &
      file_name) bind(c, name="KIM_ModelCompute_Log")
      use, intrinsic :: iso_c_binding
      use kim_model_compute_module, only : kim_model_compute_type
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(c_char), intent(in) :: message(*)
      integer(c_int), intent(in), value :: line_number
      character(c_char), intent(in) :: file_name(*)
    end subroutine log

    type(c_ptr) function model_compute_string(model_compute) &
      bind(c, name="KIM_ModelCompute_string")
      use, intrinsic :: iso_c_binding
      use kim_model_compute_module, only : kim_model_compute_type
      implicit none
      type(kim_model_compute_type), intent(in) :: model_compute
    end function model_compute_string
  end interface
end module kim_model_compute_f_module


! free functions to implement kim_model_compute_module

subroutine kim_model_compute_get_data_int0(model_compute, argument_name, &
  int0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_data_int
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(out), pointer :: int0
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_int(model_compute, argument_name, p)
  call c_f_pointer(p, int0)
end subroutine kim_model_compute_get_data_int0

subroutine kim_model_compute_get_data_int1(model_compute, argument_name, &
  extent1, int1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_data_int
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(out), pointer :: int1(:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_int(model_compute, argument_name, p)
  call c_f_pointer(p, int1, [extent1])
end subroutine kim_model_compute_get_data_int1

subroutine kim_model_compute_get_data_int2(model_compute, argument_name, &
  extent1, extent2, int2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_data_int
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), value :: extent2
  integer(c_int), intent(out), pointer :: int2(:,:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_int(model_compute, argument_name, p)
  call c_f_pointer(p, int2, [extent1, extent2])
end subroutine kim_model_compute_get_data_int2

subroutine kim_model_compute_get_data_double0(model_compute, argument_name, &
  double0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_data_double
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_argument_name_type), intent(in), value :: argument_name
  real(c_double), intent(out), pointer :: double0
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_double(model_compute, argument_name, p)
  call c_f_pointer(p, double0)
end subroutine kim_model_compute_get_data_double0

subroutine kim_model_compute_get_data_double1(model_compute, argument_name, &
  extent1, double1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_data_double
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  real(c_double), intent(out), pointer :: double1(:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_double(model_compute, argument_name, p)
  call c_f_pointer(p, double1, [extent1])
end subroutine kim_model_compute_get_data_double1

subroutine kim_model_compute_get_data_double2(model_compute, argument_name, &
  extent1, extent2, double2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_data_double
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_argument_name_type), intent(in), value :: argument_name
  integer(c_int), intent(in), value :: extent1
  integer(c_int), intent(in), value :: extent2
  real(c_double), intent(out), pointer :: double2(:,:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_data_double(model_compute, argument_name, p)
  call c_f_pointer(p, double2, [extent1, extent2])
end subroutine kim_model_compute_get_data_double2

subroutine kim_model_compute_is_call_back_present(model_compute, &
  call_back_name, present, ierr)
  use, intrinsic :: iso_c_binding
  use kim_call_back_name_module, only : kim_call_back_name_type
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : is_call_back_present
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_call_back_name_type), intent(in), value :: call_back_name
  integer(c_int), intent(out) :: present
  integer(c_int), intent(out) :: ierr

  ierr = is_call_back_present(model_compute, call_back_name, present)
end subroutine kim_model_compute_is_call_back_present

subroutine kim_model_compute_get_neigh(model_compute, neighbor_list_index, &
  particle_number, number_of_neighbors, neighbors_of_particle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_neigh
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  integer(c_int), intent(in), value :: neighbor_list_index
  integer(c_int), intent(in), value :: particle_number
  integer(c_int), intent(out) :: number_of_neighbors
  integer(c_int), intent(out), pointer :: neighbors_of_particle(:)
  integer(c_int), intent(out) :: ierr

  type(c_ptr) p

  ierr = get_neigh(model_compute, neighbor_list_index-1, particle_number, &
    number_of_neighbors, p)
  call c_f_pointer(p, neighbors_of_particle, [number_of_neighbors])
end subroutine kim_model_compute_get_neigh

subroutine kim_model_compute_process_dedr(model_compute, de, r, dx, i, j, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : process_dedr
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  real(c_double), intent(in), value :: de
  real(c_double), intent(in), value :: r
  real(c_double), intent(in) :: dx
  real(c_double), intent(in), value :: i
  real(c_double), intent(in), value :: j
  integer(c_int), intent(out) :: ierr

  ierr = process_dedr(model_compute, de, r, dx, i, j)
end subroutine kim_model_compute_process_dedr

subroutine kim_model_compute_process_d2edr2(model_compute, de, r, dx, i, j, &
  ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : process_d2edr2
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  real(c_double), intent(in), value :: de
  type(c_ptr), intent(in), value :: r
  type(c_ptr), intent(in), value :: dx
  type(c_ptr), intent(in), value :: i
  type(c_ptr), intent(in), value :: j
  integer(c_int), intent(out) :: ierr

  ierr = process_d2edr2(model_compute, de, r, dx, i, j)
end subroutine kim_model_compute_process_d2edr2

subroutine kim_model_compute_get_model_buffer(model_compute, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : get_model_buffer
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(c_ptr), intent(out) :: ptr

  call get_model_buffer(model_compute, ptr)
end subroutine kim_model_compute_get_model_buffer

subroutine kim_model_compute_log(model_compute, log_verbosity, message, &
  line_number, file_name)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_model_compute_f_module, only : log
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  character(len=*), intent(in) :: message
  integer(c_int), intent(in), value :: line_number
  character(len=*), intent(in) :: file_name

  call log(model_compute, log_verbosity, trim(message)//c_null_char, &
    line_number, trim(file_name)//c_null_char)
end subroutine kim_model_compute_log

subroutine kim_model_compute_string(model_compute, string)
  use, intrinsic :: iso_c_binding
  use kim_model_compute_module, only : kim_model_compute_type
  use kim_model_compute_f_module, only : model_compute_string
  implicit none
  type(kim_model_compute_type), intent(in) :: model_compute
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)), pointer :: fp
  integer(c_int) :: null_index

  p = model_compute_string(model_compute)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_model_compute_string
