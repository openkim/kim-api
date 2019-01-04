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
! Copyright (c) 2016--2019, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api.git repository.
!


module kim_model_compute_arguments_create_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_compute_arguments_create_handle_type, &

    ! Constants
    KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_set_argument_support_status, &
    kim_set_callback_support_status, &
    kim_set_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string


  type, bind(c) :: kim_model_compute_arguments_create_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_compute_arguments_create_handle_type

  type(kim_model_compute_arguments_create_handle_type), protected, save &
    :: KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_NULL_HANDLE

  interface operator (.eq.)
    module procedure kim_model_compute_arguments_create_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_model_compute_arguments_create_handle_not_equal
  end interface operator (.ne.)

  interface kim_set_argument_support_status
    module procedure &
      kim_model_compute_arguments_create_set_argument_support_status
  end interface kim_set_argument_support_status

  interface kim_set_callback_support_status
    module procedure &
      kim_model_compute_arguments_create_set_callback_support_status
  end interface kim_set_callback_support_status

  interface kim_set_model_buffer_pointer
    module procedure kim_model_compute_arguments_create_set_model_buffer_pointer
  end interface kim_set_model_buffer_pointer

  interface kim_log_entry
    module procedure kim_model_compute_arguments_create_log_entry
  end interface kim_log_entry

  interface kim_to_string
    module procedure kim_model_compute_arguments_create_to_string
  end interface kim_to_string

contains
  logical function kim_model_compute_arguments_create_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: lhs
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_compute_arguments_create_handle_equal = .true.
    else
      kim_model_compute_arguments_create_handle_equal = c_associated(lhs%p, &
        rhs%p)
    end if
  end function kim_model_compute_arguments_create_handle_equal

  logical function kim_model_compute_arguments_create_handle_not_equal(lhs, &
    rhs)
    implicit none
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: lhs
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: rhs

    kim_model_compute_arguments_create_handle_not_equal = &
      .not. (lhs .eq. rhs)
  end function kim_model_compute_arguments_create_handle_not_equal

  subroutine kim_model_compute_arguments_create_set_argument_support_status( &
    model_commpute_arguments_create_handle, compute_argument_name, &
    support_status, ierr)
    use kim_compute_argument_name_module, only : kim_compute_argument_name_type
    use kim_support_status_module, only : kim_support_status_type
    use kim_interoperable_types_module, only : &
      kim_model_compute_arguments_create_type
    implicit none
    interface
      integer(c_int) function set_argument_support_status( &
        model_commpute_arguments_create, compute_argument_name, &
        support_status) &
        bind(c, name="KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus")
        use, intrinsic :: iso_c_binding
        use kim_compute_argument_name_module, only : &
          kim_compute_argument_name_type
        use kim_support_status_module, only : kim_support_status_type
        use kim_interoperable_types_module, only : &
          kim_model_compute_arguments_create_type
        implicit none
        type(kim_model_compute_arguments_create_type), intent(in) :: &
          model_commpute_arguments_create
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(kim_support_status_type), intent(in), value :: support_status
      end function set_argument_support_status
    end interface
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: &
      model_commpute_arguments_create_handle
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    type(kim_support_status_type), intent(in) :: support_status
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_create_type), pointer :: &
      model_commpute_arguments_create

    call c_f_pointer(model_commpute_arguments_create_handle%p, &
      model_commpute_arguments_create)
    ierr = set_argument_support_status(model_commpute_arguments_create, &
      compute_argument_name, support_status)
  end subroutine kim_model_compute_arguments_create_set_argument_support_status

  subroutine kim_model_compute_arguments_create_set_callback_support_status( &
    model_commpute_arguments_create_handle, compute_callback_name, &
    support_status, ierr)
    use kim_compute_callback_name_module, only : kim_compute_callback_name_type
    use kim_support_status_module, only : kim_support_status_type
    use kim_interoperable_types_module, only : &
      kim_model_compute_arguments_create_type
    implicit none
    interface
      integer(c_int) function set_callback_support_status( &
        model_commpute_arguments_create, compute_callback_name, &
        support_status) &
        bind(c, name="KIM_ModelComputeArgumentsCreate_SetCallbackSupportStatus")
        use, intrinsic :: iso_c_binding
        use kim_compute_callback_name_module, only : &
          kim_compute_callback_name_type
        use kim_support_status_module, only : kim_support_status_type
        use kim_interoperable_types_module, only : &
          kim_model_compute_arguments_create_type
        implicit none
        type(kim_model_compute_arguments_create_type), intent(in) :: &
          model_commpute_arguments_create
        type(kim_compute_callback_name_type), intent(in), value :: &
          compute_callback_name
        type(kim_support_status_type), intent(in), value :: support_status
      end function set_callback_support_status
    end interface
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: &
      model_commpute_arguments_create_handle
    type(kim_compute_callback_name_type), intent(in) :: &
      compute_callback_name
    type(kim_support_status_type), intent(in) :: support_status
    integer(c_int), intent(out) :: ierr
    type(kim_model_compute_arguments_create_type), pointer :: &
      model_commpute_arguments_create

    call c_f_pointer(model_commpute_arguments_create_handle%p, &
      model_commpute_arguments_create)
    ierr = set_callback_support_status(model_commpute_arguments_create, &
      compute_callback_name, support_status)
  end subroutine kim_model_compute_arguments_create_set_callback_support_status

  subroutine kim_model_compute_arguments_create_set_model_buffer_pointer( &
    model_commpute_arguments_create_handle, ptr)
    use kim_interoperable_types_module, only : &
      kim_model_compute_arguments_create_type
    implicit none
    interface
      subroutine set_model_buffer_pointer(model_commpute_arguments_create, &
        ptr) bind(c, &
        name="KIM_ModelComputeArgumentsCreate_SetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : &
          kim_model_compute_arguments_create_type
        implicit none
        type(kim_model_compute_arguments_create_type), intent(in) :: &
          model_commpute_arguments_create
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_model_buffer_pointer
    end interface
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: &
      model_commpute_arguments_create_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_model_compute_arguments_create_type), pointer :: &
      model_commpute_arguments_create

    call c_f_pointer(model_commpute_arguments_create_handle%p, &
      model_commpute_arguments_create)
    call set_model_buffer_pointer(model_commpute_arguments_create, ptr)
  end subroutine kim_model_compute_arguments_create_set_model_buffer_pointer

  subroutine kim_model_compute_arguments_create_log_entry( &
    model_commpute_arguments_create_handle, log_verbosity, message)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : &
      kim_model_compute_arguments_create_type
    implicit none
    interface
      subroutine log_entry(model_commpute_arguments_create, log_verbosity, &
        message, line_number, file_name) &
        bind(c, name="KIM_ModelComputeArgumentsCreate_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : &
          kim_model_compute_arguments_create_type
        implicit none
        type(kim_model_compute_arguments_create_type), intent(in) :: &
          model_commpute_arguments_create
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: &
      model_commpute_arguments_create_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_compute_arguments_create_type), pointer :: &
      model_commpute_arguments_create

    call c_f_pointer(model_commpute_arguments_create_handle%p, &
      model_commpute_arguments_create)
    call log_entry(model_commpute_arguments_create, log_verbosity, &
      trim(message)//c_null_char, 0, ""//c_null_char)
  end subroutine kim_model_compute_arguments_create_log_entry

  subroutine kim_model_compute_arguments_create_to_string( &
    model_commpute_arguments_create_handle, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : &
      kim_model_compute_arguments_create_type
    implicit none
    interface
      type(c_ptr) function model_commpute_arguments_create_string( &
        model_commpute_arguments_create) &
        bind(c, name="KIM_ModelComputeArgumentsCreate_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : &
          kim_model_compute_arguments_create_type
        implicit none
        type(kim_model_compute_arguments_create_type), intent(in) :: &
          model_commpute_arguments_create
      end function model_commpute_arguments_create_string
    end interface
    type(kim_model_compute_arguments_create_handle_type), intent(in) :: &
      model_commpute_arguments_create_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_compute_arguments_create_type), pointer :: &
      model_commpute_arguments_create

    type(c_ptr) :: p

    call c_f_pointer(model_commpute_arguments_create_handle%p, &
      model_commpute_arguments_create)
    p = model_commpute_arguments_create_string(model_commpute_arguments_create)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_compute_arguments_create_to_string
end module kim_model_compute_arguments_create_module
