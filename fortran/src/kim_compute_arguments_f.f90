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


module kim_compute_arguments_f_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_compute_arguments_type, &
    get_argument_support_status, &
    get_callback_support_status, &
    set_argument_pointer_integer, &
    set_argument_pointer_double, &
    set_callback_pointer, &
    are_all_required_arguments_and_callbacks_present, &
    set_simulator_buffer_pointer, &
    get_simulator_buffer_pointer, &
    compute_arguments_string, &
    set_log_id, &
    push_log_verbosity, &
    pop_log_verbosity

  type, bind(c) :: kim_compute_arguments_type
    private
    type(c_ptr) :: p
  end type kim_compute_arguments_type

  interface
    integer(c_int) function get_argument_support_status(compute_arguments, &
      compute_argument_name, support_status) &
      bind(c, name="KIM_ComputeArguments_GetArgumentSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      type(kim_support_status_type), intent(out) :: support_status
    end function get_argument_support_status

    integer(c_int) function get_callback_support_status(compute_arguments, &
      compute_callback_name, support_status) &
      bind(c, name="KIM_ComputeArguments_GetCallbackSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_compute_callback_name_module, only : &
        kim_compute_callback_name_type
      use kim_support_status_module, only : kim_support_status_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_callback_name_type), intent(in), value :: &
        compute_callback_name
      type(kim_support_status_type), intent(out) :: support_status
    end function get_callback_support_status

    integer(c_int) function set_argument_pointer_integer(compute_arguments, &
      compute_argument_name, ptr) &
      bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      type(c_ptr), intent(in), value :: ptr
    end function set_argument_pointer_integer

    integer(c_int) function set_argument_pointer_double(compute_arguments, &
      compute_argument_name, ptr) &
      bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
      use, intrinsic :: iso_c_binding
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in), value :: &
        compute_argument_name
      type(c_ptr), intent(in), value :: ptr
    end function set_argument_pointer_double

    integer(c_int) function set_callback_pointer(compute_arguments, &
      compute_callback_name, language_name, fptr, data_object) &
      bind(c, name="KIM_ComputeArguments_SetCallbackPointer")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_compute_callback_name_module, only : &
        kim_compute_callback_name_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
      type(kim_language_name_type), intent(in), value :: language_name
      type(kim_compute_callback_name_type), intent(in), value :: &
        compute_callback_name
      type(c_funptr), intent(in), value :: fptr
      type(c_ptr), intent(in), value :: data_object
    end function set_callback_pointer

    integer(c_int) function are_all_required_arguments_and_callbacks_present( &
      compute_arguments, result_value) bind(c, &
      name="KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent")
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      integer(c_int), intent(out) :: result_value
    end function are_all_required_arguments_and_callbacks_present

    subroutine set_simulator_buffer_pointer(compute_arguments, ptr) &
      bind(c, name="KIM_ComputeArguments_SetSimulatorBufferPointer")
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_simulator_buffer_pointer

    subroutine get_simulator_buffer_pointer(compute_arguments, ptr) &
      bind(c, name="KIM_ComputeArguments_GetSimulatorBufferPointer")
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(c_ptr), intent(out) :: ptr
    end subroutine get_simulator_buffer_pointer

    type(c_ptr) function compute_arguments_string(compute_arguments) &
      bind(c, name="KIM_ComputeArguments_String")
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
    end function compute_arguments_string

    subroutine set_log_id(compute_arguments, log_id) &
      bind(c, name="KIM_ComputeArguments_SetLogID")
      use, intrinsic :: iso_c_binding
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
      character(c_char), intent(in) :: log_id(*)
    end subroutine set_log_id

    subroutine push_log_verbosity(compute_arguments, log_verbosity) &
      bind(c, name="KIM_ComputeArguments_PushLogVerbosity")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
    end subroutine push_log_verbosity

    subroutine pop_log_verbosity(compute_arguments) &
      bind(c, name="KIM_ComputeArguments_PopLogVerbosity")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_compute_arguments_type
      implicit none
      type(kim_compute_arguments_type), intent(inout) :: compute_arguments
    end subroutine pop_log_verbosity
  end interface
end module kim_compute_arguments_f_module


! free functions to implement kim_compute_arguments_module

logical function kim_compute_arguments_handle_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: left
  type(kim_compute_arguments_handle_type), intent(in) :: right

  if ((.not. c_associated(left%p)) .and. (.not. c_associated(right%p))) then
    kim_compute_arguments_handle_equal = .true.
  else
    kim_compute_arguments_handle_equal = c_associated(left%p, right%p)
  end if
end function kim_compute_arguments_handle_equal

logical function kim_compute_arguments_handle_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_module, only : operator (.eq.)
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: left
  type(kim_compute_arguments_handle_type), intent(in) :: right

  kim_compute_arguments_handle_not_equal = .not. (left .eq. right)
end function kim_compute_arguments_handle_not_equal

subroutine kim_compute_arguments_get_argument_support_status( &
  compute_arguments_handle, compute_argument_name, &
  support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_support_status_module, only : kim_support_status_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    get_argument_support_status
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  type(kim_support_status_type), intent(out) :: support_status
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = get_argument_support_status(compute_arguments, compute_argument_name, &
    support_status)
end subroutine kim_compute_arguments_get_argument_support_status

subroutine kim_compute_arguments_get_callback_support_status( &
  compute_arguments_handle, compute_callback_name, support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_callback_name_module, only : kim_compute_callback_name_type
  use kim_support_status_module, only : kim_support_status_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    get_callback_support_status
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_callback_name_type), intent(in), value :: &
    compute_callback_name
  type(kim_support_status_type), intent(out) :: support_status
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = get_callback_support_status(compute_arguments, compute_callback_name, &
    support_status)
end subroutine kim_compute_arguments_get_callback_support_status

subroutine kim_compute_arguments_set_argument_pointer_int0( &
  compute_arguments_handle, compute_argument_name, int0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    set_argument_pointer_integer
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  integer(c_int), intent(in), target :: int0
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = set_argument_pointer_integer(compute_arguments, &
    compute_argument_name, c_loc(int0))
end subroutine kim_compute_arguments_set_argument_pointer_int0

subroutine kim_compute_arguments_set_argument_pointer_int1( &
  compute_arguments_handle, compute_argument_name, int1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    set_argument_pointer_integer
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  integer(c_int), intent(in), target :: int1(:)
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call set(compute_arguments, compute_argument_name, size(int1,1,c_int), int1, &
    ierr)
  return

contains
  subroutine set(compute_arguments, compute_argument_name, extent1, int1, ierr)
    use, intrinsic :: iso_c_binding
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_compute_arguments_f_module, only : kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_type), intent(inout) :: compute_arguments
    type(kim_compute_argument_name_type), intent(in), value :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in), target :: int1(extent1)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_integer(compute_arguments, &
      compute_argument_name, c_loc(int1))
  end subroutine set
end subroutine kim_compute_arguments_set_argument_pointer_int1

subroutine kim_compute_arguments_set_argument_pointer_int2( &
  compute_arguments_handle, compute_argument_name, int2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  integer(c_int), intent(in), target :: int2(:,:)
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call set(compute_arguments, compute_argument_name, size(int2, 1, c_int), &
    size(int2, 2, c_int), int2, ierr)
  return

contains
  subroutine set(compute_arguments, compute_argument_name, extent1, extent2, &
    int2, ierr)
    use, intrinsic :: iso_c_binding
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
      set_argument_pointer_integer
    implicit none
    type(kim_compute_arguments_type), intent(inout) :: compute_arguments
    type(kim_compute_argument_name_type), intent(in), value :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in) :: extent2
    integer(c_int), intent(in), target :: int2(extent1,extent2)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_integer(compute_arguments, &
      compute_argument_name, c_loc(int2))
  end subroutine set
end subroutine kim_compute_arguments_set_argument_pointer_int2

subroutine kim_compute_arguments_set_argument_pointer_double0( &
  compute_arguments_handle, compute_argument_name, double0, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    set_argument_pointer_double
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  real(c_double), intent(in), target :: double0
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = set_argument_pointer_double(compute_arguments, compute_argument_name, &
    c_loc(double0))
end subroutine kim_compute_arguments_set_argument_pointer_double0

subroutine kim_compute_arguments_set_argument_pointer_double1( &
  compute_arguments_handle, compute_argument_name, double1, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  real(c_double), intent(in), target :: double1(:)
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call set(compute_arguments, compute_argument_name, size(double1, 1, c_int), &
    double1, ierr)
  return

contains
  subroutine set(compute_arguments, compute_argument_name, extent1, double1, &
    ierr)
    use, intrinsic :: iso_c_binding
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
      set_argument_pointer_double
    implicit none
    type(kim_compute_arguments_type), intent(inout) :: compute_arguments
    type(kim_compute_argument_name_type), intent(in), value :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    real(c_double), intent(in), target :: double1(extent1)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_double(compute_arguments, &
      compute_argument_name, c_loc(double1))
  end subroutine set
end subroutine kim_compute_arguments_set_argument_pointer_double1

subroutine kim_compute_arguments_set_argument_pointer_double2( &
  compute_arguments_handle, compute_argument_name, double2, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_argument_name_module, only : kim_compute_argument_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_argument_name_type), intent(in), value :: &
    compute_argument_name
  real(c_double), intent(in), target :: double2(:,:)
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call set(compute_arguments, compute_argument_name, size(double2, 1, c_int), &
    size(double2, 2, c_int), double2, ierr)
  return

contains
  subroutine set(compute_arguments, compute_argument_name, extent1, extent2, &
    double2, ierr)
    use, intrinsic :: iso_c_binding
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
      set_argument_pointer_double
    implicit none
    type(kim_compute_arguments_type), intent(inout) :: compute_arguments
    type(kim_compute_argument_name_type), intent(in), value :: &
      compute_argument_name
    integer(c_int), intent(in) :: extent1
    integer(c_int), intent(in) :: extent2
    real(c_double), intent(in), target :: double2(extent1,extent2)
    integer(c_int), intent(out) :: ierr

    ierr = set_argument_pointer_double(compute_arguments, &
      compute_argument_name, c_loc(double2))
  end subroutine set
end subroutine kim_compute_arguments_set_argument_pointer_double2

subroutine kim_compute_arguments_set_callback_pointer( &
  compute_arguments_handle, compute_callback_name, language_name, fptr, &
  data_object, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_callback_name_module, only : kim_compute_callback_name_type
  use kim_language_name_module, only : kim_language_name_type
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    set_callback_pointer
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_callback_name_type), intent(in), value :: &
    compute_callback_name
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  type(c_ptr), intent(in), value :: data_object
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = set_callback_pointer(compute_arguments, compute_callback_name, &
    language_name, fptr, data_object)
end subroutine kim_compute_arguments_set_callback_pointer

subroutine kim_compute_arguments_are_all_required_present( &
  compute_arguments_handle, result_value, ierr)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    are_all_required_arguments_and_callbacks_present
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  integer(c_int), intent(out) :: result_value
  integer(c_int), intent(out) :: ierr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  ierr = are_all_required_arguments_and_callbacks_present(compute_arguments, &
    result_value)
end subroutine kim_compute_arguments_are_all_required_present


subroutine kim_compute_arguments_set_simulator_buffer_pointer( &
  compute_arguments_handle, ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    set_simulator_buffer_pointer
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(c_ptr), intent(in), value :: ptr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call set_simulator_buffer_pointer(compute_arguments, ptr)
end subroutine kim_compute_arguments_set_simulator_buffer_pointer

subroutine kim_compute_arguments_get_simulator_buffer_pointer( &
  compute_arguments_handle, ptr)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    get_simulator_buffer_pointer
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(c_ptr), intent(out) :: ptr
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call get_simulator_buffer_pointer(compute_arguments, ptr)
end subroutine kim_compute_arguments_get_simulator_buffer_pointer

subroutine kim_compute_arguments_string(compute_arguments_handle, string)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    compute_arguments_string
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  character(len=*, kind=c_char), intent(out) :: string
  type(kim_compute_arguments_type), pointer :: compute_arguments

  type(c_ptr) :: p

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  p = compute_arguments_string(compute_arguments)
  if (c_associated(p)) then
    call kim_convert_string(p, string)
  else
    string = ""
  end if
end subroutine kim_compute_arguments_string

subroutine kim_compute_arguments_set_log_id(compute_arguments_handle, log_id)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    set_log_id
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  character(len=*, kind=c_char), intent(in) :: log_id
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call set_log_id(compute_arguments, trim(log_id)//c_null_char)
end subroutine kim_compute_arguments_set_log_id

subroutine kim_compute_arguments_push_log_verbosity(compute_arguments_handle, &
  log_verbosity)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    push_log_verbosity
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_log_verbosity_type), intent(in) :: log_verbosity
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call push_log_verbosity(compute_arguments, log_verbosity)
end subroutine kim_compute_arguments_push_log_verbosity

subroutine kim_compute_arguments_pop_log_verbosity(compute_arguments_handle)
  use, intrinsic :: iso_c_binding
  use kim_compute_arguments_module, only : kim_compute_arguments_handle_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_compute_arguments_f_module, only : kim_compute_arguments_type, &
    pop_log_verbosity
  implicit none
  type(kim_compute_arguments_handle_type), intent(in) :: &
    compute_arguments_handle
  type(kim_compute_arguments_type), pointer :: compute_arguments

  call c_f_pointer(compute_arguments_handle%p, compute_arguments)
  call pop_log_verbosity(compute_arguments)
end subroutine kim_compute_arguments_pop_log_verbosity
