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


module kim_model_reinitialization_f_module
  implicit none
  private

  public &
    set_influence_distance, &
    set_cutoffs, &
    get_model_buffer, &
    log, &
    model_reinitialization_string

  interface
    subroutine set_influence_distance(model_reinitialization, &
      influence_distance) &
      bind(c, name="KIM_ModelReinitialization_set_influence_distance")
      use, intrinsic :: iso_c_binding
      use kim_model_reinitialization_module, only : &
        kim_model_reinitialization_type
      implicit none
      type(kim_model_reinitialization_type), intent(inout) :: &
        model_reinitialization
      type(c_ptr), intent(in), value :: influence_distance
    end subroutine set_influence_distance

    subroutine set_cutoffs(model_reinitialization, number_of_cutoffs, &
      cutoffs_ptr) bind(c, name="KIM_ModelReinitialization_set_cutoffs")
      use, intrinsic :: iso_c_binding
      use kim_model_reinitialization_module, only : &
        kim_model_reinitialization_type
      implicit none
      type(kim_model_reinitialization_type), intent(inout) :: &
        model_reinitialization
      integer(c_int), intent(in), value :: number_of_cutoffs
      type(c_ptr), intent(in), value :: cutoffs_ptr
    end subroutine set_cutoffs

    subroutine get_model_buffer(model_reinitialization, ptr) &
      bind(c, name="KIM_ModelReinitialization_get_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_reinitialization_module, only : &
        kim_model_reinitialization_type
      implicit none
      type(kim_model_reinitialization_type), intent(in) :: &
        model_reinitialization
      type(c_ptr), intent(out) :: ptr
    end subroutine get_model_buffer

    subroutine log(model_reinitialization, log_verbosity, message, &
      line_number, file_name) bind(c, name="KIM_ModelReinitialization_Log")
      use, intrinsic :: iso_c_binding
      use kim_model_reinitialization_module, only : &
        kim_model_reinitialization_type
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      implicit none
      type(kim_model_reinitialization_type), intent(in) :: &
        model_reinitialization
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(c_char), intent(in) :: message(*)
      integer(c_int), intent(in), value :: line_number
      character(c_char), intent(in) :: file_name(*)
    end subroutine log

    type(c_ptr) function model_reinitialization_string(model_reinitialization) &
      bind(c, name="KIM_ModelReinitialization_string")
      use, intrinsic :: iso_c_binding
      use kim_model_reinitialization_module, only : &
        kim_model_reinitialization_type
      implicit none
      type(kim_model_reinitialization_type), intent(in) :: &
        model_reinitialization
    end function model_reinitialization_string
  end interface
end module kim_model_reinitialization_f_module


! free functions to implement kim_model_reinitialization_module

subroutine kim_model_reinitialization_set_influence_distance( &
  model_reinitialization, influence_distance)
  use, intrinsic :: iso_c_binding
  use kim_model_reinitialization_module, only : kim_model_reinitialization_type
  use kim_model_reinitialization_f_module, only : set_influence_distance
  implicit none
  type(kim_model_reinitialization_type), intent(inout) :: model_reinitialization
  real(c_double), intent(in), target :: influence_distance

  call set_influence_distance(model_reinitialization, c_loc(influence_distance))
end subroutine kim_model_reinitialization_set_influence_distance

subroutine kim_model_reinitialization_set_cutoffs(model_reinitialization, &
  number_of_cutoffs, cutoffs)
  use, intrinsic :: iso_c_binding
  use kim_model_reinitialization_module, only : kim_model_reinitialization_type
  use kim_model_reinitialization_f_module, only : set_cutoffs
  implicit none
  type(kim_model_reinitialization_type), intent(inout) :: model_reinitialization
  integer(c_int), intent(in), value :: number_of_cutoffs
  real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)

  call set_cutoffs(model_reinitialization, number_of_cutoffs, c_loc(cutoffs))
end subroutine kim_model_reinitialization_set_cutoffs

subroutine kim_model_reinitialization_get_model_buffer(model_reinitialization, &
  ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_reinitialization_module, only : kim_model_reinitialization_type
  use kim_model_reinitialization_f_module, only : get_model_buffer
  implicit none
  type(kim_model_reinitialization_type), intent(in) :: model_reinitialization
  type(c_ptr), intent(out) :: ptr

  call get_model_buffer(model_reinitialization, ptr)
end subroutine kim_model_reinitialization_get_model_buffer

subroutine kim_model_reinitialization_log(model_reinitialization, &
  log_verbosity, message, line_number, file_name)
  use, intrinsic :: iso_c_binding
  use kim_model_reinitialization_module, only : kim_model_reinitialization_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_model_reinitialization_f_module, only : log
  implicit none
  type(kim_model_reinitialization_type), intent(in) :: model_reinitialization
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  character(len=*), intent(in) :: message
  integer(c_int), intent(in), value :: line_number
  character(len=*), intent(in) :: file_name

  call log(model_reinitialization, log_verbosity, trim(message)//c_null_char, &
    line_number, trim(file_name)//c_null_char)
end subroutine kim_model_reinitialization_log

subroutine kim_model_reinitialization_string(model_reinitialization, string)
  use, intrinsic :: iso_c_binding
  use kim_model_reinitialization_module, only : kim_model_reinitialization_type
  use kim_model_reinitialization_f_module, only : model_reinitialization_string
  implicit none
  type(kim_model_reinitialization_type), intent(in) :: model_reinitialization
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)), pointer :: fp
  integer(c_int) :: null_index

  p = model_reinitialization_string(model_reinitialization)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_model_reinitialization_string
