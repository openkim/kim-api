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


module kim_argument_name_f_module
  implicit none
  private

  public &
    compute_argument_name_string, &
    get_number_of_arguments, &
    get_argument_name, &
    get_argument_data_type

  interface
    type(c_ptr) function compute_argument_name_string(compute_argument_name) &
      bind(c, name="KIM_ArgumentNameString")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : &
        kim_argument_name_type
      implicit none
      type(kim_argument_name_type), intent(in), value :: &
        compute_argument_name
    end function compute_argument_name_string

    subroutine get_number_of_arguments(number_of_arguments) &
      bind(c, name="KIM_ARGUMENT_NAME_get_number_of_arguments")
      use, intrinsic :: iso_c_binding
      integer(c_int), intent(out) :: number_of_arguments
    end subroutine get_number_of_arguments

    integer(c_int) function get_argument_name(index, argument_name) &
      bind(c, name="KIM_ARGUMENT_NAME_get_argument_name")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : &
        kim_argument_name_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_argument_name_type), intent(out) :: argument_name
    end function get_argument_name

    integer(c_int) function get_argument_data_type(argument_name, data_type) &
      bind(c, name="KIM_ARGUMENT_NAME_get_argument_data_type")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : &
        kim_argument_name_type
      use kim_data_type_module, only : kim_data_type_type
      implicit none
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_data_type_type), intent(out) :: data_type
    end function get_argument_data_type
  end interface
end module kim_argument_name_f_module

! free functions to implement kim_argument_name_module

subroutine kim_argument_name_string(argument_name, name_string)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_argument_name_f_module, only : compute_argument_name_string
  implicit none
  type(kim_argument_name_type), intent(in), value :: argument_name
  character(len=*), intent(out) :: name_string

  type(c_ptr) :: p
  character(len=len(name_string)), pointer :: fp
  integer(c_int) :: null_index

  p = compute_argument_name_string(argument_name)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  name_string = fp(1:null_index)
end subroutine kim_argument_name_string

subroutine kim_argument_name_get_number_of_arguments( &
  number_of_arguments)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_f_module, only : get_number_of_arguments
  implicit none
  integer(c_int), intent(out) :: number_of_arguments

  call get_number_of_arguments(number_of_arguments)
end subroutine kim_argument_name_get_number_of_arguments

subroutine kim_argument_name_get_argument_name(index, argument_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_argument_name_f_module, only : get_argument_name
  implicit none
  integer(c_int), intent(in), value :: index
  type(kim_argument_name_type), intent(out) :: argument_name
  integer(c_int), intent(out) :: ierr

  ierr = get_argument_name(index-1, argument_name)
end subroutine kim_argument_name_get_argument_name

subroutine kim_argument_name_get_argument_data_type(argument_name, &
  data_type, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_data_type_module, only : kim_data_type_type
  use kim_argument_name_f_module, only : get_argument_data_type
  implicit none
  type(kim_argument_name_type), intent(in), value :: argument_name
  type(kim_data_type_type), intent(out) :: data_type
  integer(c_int), intent(out) :: ierr

  ierr = get_argument_data_type(argument_name, data_type)
end subroutine kim_argument_name_get_argument_data_type
