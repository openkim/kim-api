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
    ! Derived types
    kim_compute_arguments_handle_type, &

    ! Constants
    KIM_COMPUTE_ARGUMENTS_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_get_argument_support_status, &
    kim_get_callback_support_status, &
    kim_set_argument_pointer, &
    kim_set_callback_pointer, &
    kim_are_all_required_present, &
    kim_set_simulator_buffer_pointer, &
    kim_get_simulator_buffer_pointer, &
    kim_to_string, &
    kim_set_log_id, &
    kim_push_log_verbosity, &
    kim_pop_log_verbosity


  type, bind(c) :: kim_compute_arguments_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_compute_arguments_handle_type

  type(kim_compute_arguments_handle_type), protected, save &
    :: KIM_COMPUTE_ARGUMENTS_NULL_HANDLE

  interface operator (.eq.)
    module procedure kim_compute_arguments_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_compute_arguments_handle_not_equal
  end interface operator (.ne.)

  interface kim_get_argument_support_status
    module procedure kim_compute_arguments_get_argument_support_status
  end interface kim_get_argument_support_status

  interface kim_get_callback_support_status
    module procedure kim_compute_arguments_get_callback_support_status
  end interface kim_get_callback_support_status

  interface kim_set_argument_pointer
    module procedure kim_compute_arguments_set_argument_pointer_int0
    module procedure kim_compute_arguments_set_argument_pointer_int1
    module procedure kim_compute_arguments_set_argument_pointer_int2
    module procedure kim_compute_arguments_set_argument_pointer_double0
    module procedure kim_compute_arguments_set_argument_pointer_double1
    module procedure kim_compute_arguments_set_argument_pointer_double2
  end interface kim_set_argument_pointer

  interface kim_set_callback_pointer
    module procedure kim_compute_arguments_set_callback_pointer
  end interface kim_set_callback_pointer

  interface kim_are_all_required_present
    module procedure kim_compute_arguments_are_all_required_present
  end interface kim_are_all_required_present

  interface kim_set_simulator_buffer_pointer
    module procedure kim_compute_arguments_set_simulator_buffer_pointer
  end interface kim_set_simulator_buffer_pointer

  interface kim_get_simulator_buffer_pointer
    module procedure kim_compute_arguments_get_simulator_buffer_pointer
  end interface kim_get_simulator_buffer_pointer

  interface kim_to_string
    module procedure kim_compute_arguments_to_string
  end interface kim_to_string

  interface kim_set_log_id
    module procedure kim_compute_arguments_set_log_id
  end interface kim_set_log_id

  interface kim_push_log_verbosity
    module procedure kim_compute_arguments_push_log_verbosity
  end interface kim_push_log_verbosity

  interface kim_pop_log_verbosity
    module procedure kim_compute_arguments_pop_log_verbosity
  end interface kim_pop_log_verbosity

contains
  logical function kim_compute_arguments_handle_equal(lhs, rhs)
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: lhs
    type(kim_compute_arguments_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_compute_arguments_handle_equal = .true.
    else
      kim_compute_arguments_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_compute_arguments_handle_equal

  logical function kim_compute_arguments_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: lhs
    type(kim_compute_arguments_handle_type), intent(in) :: rhs

    kim_compute_arguments_handle_not_equal = .not. (lhs .eq. rhs)
  end function kim_compute_arguments_handle_not_equal

  subroutine kim_compute_arguments_get_argument_support_status( &
    compute_arguments_handle, compute_argument_name, &
    support_status, ierr)
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_support_status_module, only : kim_support_status_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) function get_argument_support_status(compute_arguments, &
        compute_argument_name, support_status) &
        bind(c, name="KIM_ComputeArguments_GetArgumentSupportStatus")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only : &
          kim_compute_argument_name_type
        use kim_support_status_module, only : kim_support_status_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(kim_support_status_type), intent(out) :: support_status
      end function get_argument_support_status
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    type(kim_support_status_type), intent(out) :: support_status
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = get_argument_support_status(compute_arguments, &
      compute_argument_name, support_status)
  end subroutine kim_compute_arguments_get_argument_support_status

  subroutine kim_compute_arguments_get_callback_support_status( &
    compute_arguments_handle, compute_callback_name, support_status, ierr)
    use kim_compute_callback_name_module, only : kim_compute_callback_name_type
    use kim_support_status_module, only : kim_support_status_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) function get_callback_support_status(compute_arguments, &
        compute_callback_name, support_status) &
        bind(c, name="KIM_ComputeArguments_GetCallbackSupportStatus")
        use, intrinsic :: iso_c_binding
        use kim_compute_callback_name_module, only : &
          kim_compute_callback_name_type
        use kim_support_status_module, only : kim_support_status_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_callback_name_type), intent(in), value :: &
          compute_callback_name
        type(kim_support_status_type), intent(out) :: support_status
      end function get_callback_support_status
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_callback_name_type), intent(in) :: &
      compute_callback_name
    type(kim_support_status_type), intent(out) :: support_status
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = get_callback_support_status(compute_arguments, &
      compute_callback_name, support_status)
  end subroutine kim_compute_arguments_get_callback_support_status

  subroutine kim_compute_arguments_set_argument_pointer_int0( &
    compute_arguments_handle, compute_argument_name, int0, ierr)
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) function set_argument_pointer_integer(compute_arguments, &
        compute_argument_name, ptr) &
        bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only : &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(in), value :: ptr
      end function set_argument_pointer_integer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
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
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    integer(c_int), intent(in), target :: int1(:)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, size(int1,1,c_int), &
      int1, ierr)
    return

  contains
    subroutine set(compute_arguments, compute_argument_name, extent1, int1, &
      ierr)
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_interoperable_types_module, only : kim_compute_arguments_type
      implicit none
      interface
        integer(c_int) function set_argument_pointer_integer( &
          compute_arguments, compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only : &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only : kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_integer
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
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
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
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
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_interoperable_types_module, only : kim_compute_arguments_type
      implicit none
      interface
        integer(c_int) function set_argument_pointer_integer( &
          compute_arguments, compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerInteger")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only : &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only : kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_integer
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
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
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) function set_argument_pointer_double(compute_arguments, &
        compute_argument_name, ptr) &
        bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only : &
          kim_compute_argument_name_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(c_ptr), intent(in), value :: ptr
      end function set_argument_pointer_double
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(in), target :: double0
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = set_argument_pointer_double(compute_arguments, &
      compute_argument_name, c_loc(double0))
  end subroutine kim_compute_arguments_set_argument_pointer_double0

  subroutine kim_compute_arguments_set_argument_pointer_double1( &
    compute_arguments_handle, compute_argument_name, double1, ierr)
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(in), target :: double1(:)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, &
      size(double1, 1, c_int), double1, ierr)
    return

  contains
    subroutine set(compute_arguments, compute_argument_name, extent1, &
      double1, ierr)
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      use kim_interoperable_types_module, only : kim_compute_arguments_type
      implicit none
      interface
        integer(c_int) function set_argument_pointer_double(compute_arguments, &
          compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only : &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only : kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_double
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
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
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    real(c_double), intent(in), target :: double2(:,:)
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set(compute_arguments, compute_argument_name, &
      size(double2, 1, c_int), size(double2, 2, c_int), double2, ierr)
    return

  contains
    subroutine set(compute_arguments, compute_argument_name, extent1, &
      extent2, double2, ierr)
      use kim_compute_argument_name_module, only : &
        kim_compute_argument_name_type
      implicit none
      interface
        integer(c_int) function set_argument_pointer_double(compute_arguments, &
          compute_argument_name, ptr) &
          bind(c, name="KIM_ComputeArguments_SetArgumentPointerDouble")
          use, intrinsic :: iso_c_binding
          use kim_compute_argument_name_module, only : &
            kim_compute_argument_name_type
          use kim_interoperable_types_module, only : kim_compute_arguments_type
          implicit none
          type(kim_compute_arguments_type), intent(in) :: compute_arguments
          type(kim_compute_argument_name_type), intent(in), value :: &
            compute_argument_name
          type(c_ptr), intent(in), value :: ptr
        end function set_argument_pointer_double
      end interface
      type(kim_compute_arguments_type), intent(in) :: compute_arguments
      type(kim_compute_argument_name_type), intent(in) :: &
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
    use kim_compute_callback_name_module, only : kim_compute_callback_name_type
    use kim_language_name_module, only : kim_language_name_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) function set_callback_pointer(compute_arguments, &
        compute_callback_name, language_name, fptr, data_object) &
        bind(c, name="KIM_ComputeArguments_SetCallbackPointer")
        use, intrinsic :: iso_c_binding
        use kim_language_name_module, only : kim_language_name_type
        use kim_compute_callback_name_module, only : &
          kim_compute_callback_name_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_language_name_type), intent(in), value :: language_name
        type(kim_compute_callback_name_type), intent(in), value :: &
          compute_callback_name
        type(c_funptr), intent(in), value :: fptr
        type(c_ptr), intent(in), value :: data_object
      end function set_callback_pointer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_callback_name_type), intent(in) :: &
      compute_callback_name
    type(kim_language_name_type), intent(in) :: language_name
    type(c_funptr), intent(in), value :: fptr  ! must be left as "value"!?!
    type(c_ptr), intent(in) :: data_object
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = set_callback_pointer(compute_arguments, compute_callback_name, &
      language_name, fptr, data_object)
  end subroutine kim_compute_arguments_set_callback_pointer

  subroutine kim_compute_arguments_are_all_required_present( &
    compute_arguments_handle, result_value, ierr)
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      integer(c_int) function &
        are_all_required_arguments_and_callbacks_present( &
        compute_arguments, result_value) bind(c, &
        name="KIM_ComputeArguments_AreAllRequiredArgumentsAndCallbacksPresent")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        integer(c_int), intent(out) :: result_value
      end function are_all_required_arguments_and_callbacks_present
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    integer(c_int), intent(out) :: result_value
    integer(c_int), intent(out) :: ierr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    ierr = are_all_required_arguments_and_callbacks_present( &
      compute_arguments, result_value)
  end subroutine kim_compute_arguments_are_all_required_present

  subroutine kim_compute_arguments_set_simulator_buffer_pointer( &
    compute_arguments_handle, ptr)
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      subroutine set_simulator_buffer_pointer(compute_arguments, ptr) &
        bind(c, name="KIM_ComputeArguments_SetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_simulator_buffer_pointer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set_simulator_buffer_pointer(compute_arguments, ptr)
  end subroutine kim_compute_arguments_set_simulator_buffer_pointer

  subroutine kim_compute_arguments_get_simulator_buffer_pointer( &
    compute_arguments_handle, ptr)
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      subroutine get_simulator_buffer_pointer(compute_arguments, ptr) &
        bind(c, name="KIM_ComputeArguments_GetSimulatorBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(c_ptr), intent(out) :: ptr
      end subroutine get_simulator_buffer_pointer
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call get_simulator_buffer_pointer(compute_arguments, ptr)
  end subroutine kim_compute_arguments_get_simulator_buffer_pointer

  subroutine kim_compute_arguments_to_string(compute_arguments_handle, string)
    use kim_convert_string_module, only : kim_convert_string
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      type(c_ptr) function compute_arguments_string(compute_arguments) &
        bind(c, name="KIM_ComputeArguments_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
      end function compute_arguments_string
    end interface
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
  end subroutine kim_compute_arguments_to_string

  subroutine kim_compute_arguments_set_log_id(compute_arguments_handle, &
    log_id)
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      subroutine set_log_id(compute_arguments, log_id) &
        bind(c, name="KIM_ComputeArguments_SetLogID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        character(c_char), intent(in) :: log_id(*)
      end subroutine set_log_id
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    character(len=*, kind=c_char), intent(in) :: log_id
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call set_log_id(compute_arguments, trim(log_id)//c_null_char)
  end subroutine kim_compute_arguments_set_log_id

  subroutine kim_compute_arguments_push_log_verbosity( &
    compute_arguments_handle, log_verbosity)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      subroutine push_log_verbosity(compute_arguments, log_verbosity) &
        bind(c, name="KIM_ComputeArguments_PushLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end subroutine push_log_verbosity
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call push_log_verbosity(compute_arguments, log_verbosity)
  end subroutine kim_compute_arguments_push_log_verbosity

  subroutine kim_compute_arguments_pop_log_verbosity(compute_arguments_handle)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_compute_arguments_type
    implicit none
    interface
      subroutine pop_log_verbosity(compute_arguments) &
        bind(c, name="KIM_ComputeArguments_PopLogVerbosity")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_compute_arguments_type
        implicit none
        type(kim_compute_arguments_type), intent(in) :: compute_arguments
      end subroutine pop_log_verbosity
    end interface
    type(kim_compute_arguments_handle_type), intent(in) :: &
      compute_arguments_handle
    type(kim_compute_arguments_type), pointer :: compute_arguments

    call c_f_pointer(compute_arguments_handle%p, compute_arguments)
    call pop_log_verbosity(compute_arguments)
  end subroutine kim_compute_arguments_pop_log_verbosity
end module kim_compute_arguments_module
