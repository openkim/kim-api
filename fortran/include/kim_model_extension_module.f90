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
! Release: This file is part of the kim-api-v2-2.0.0-beta.3 package.
!


module kim_model_extension_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_extension_handle_type, &

    ! Constants
    KIM_MODEL_EXTENSION_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_get_extension_id, &
    kim_to_model, &
    kim_to_model_compute, &
    kim_to_model_create, &
    kim_to_model_destroy, &
    kim_to_model_driver_create, &
    kim_to_model_refresh, &
    kim_to_model_write_parameterized_model, &
    kim_to_model_compute_arguments, &
    kim_to_model_compute_arguments_create, &
    kim_to_model_compute_arguments_destroy, &
    kim_c_char_array_to_string, &
    kim_c_char_ptr_to_string, &
    kim_string_to_c_char_array, &
    kim_get_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string


  type, bind(c) :: kim_model_extension_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_extension_handle_type

  type(kim_model_extension_handle_type), protected, save &
    :: KIM_MODEL_EXTENSION_NULL_HANDLE

  interface operator (.eq.)
    module procedure kim_model_extension_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_model_extension_handle_not_equal
  end interface operator (.ne.)

  interface kim_get_extension_id
    module procedure kim_model_extension_get_extension_id
  end interface kim_get_extension_id

  interface kim_to_model
    module procedure kim_model_extension_to_model
  end interface kim_to_model

  interface kim_to_model_compute
    module procedure kim_model_extension_to_model_compute
  end interface kim_to_model_compute

  interface kim_to_model_create
    module procedure kim_model_extension_to_model_create
  end interface kim_to_model_create

  interface kim_to_model_destroy
    module procedure kim_model_extension_to_model_destroy
  end interface kim_to_model_destroy

  interface kim_to_model_driver_create
    module procedure kim_model_extension_to_model_driver_create
  end interface kim_to_model_driver_create

  interface kim_to_model_refresh
    module procedure kim_model_extension_to_model_refresh
  end interface kim_to_model_refresh

  interface kim_to_model_write_parameterized_model
    module procedure kim_model_extension_to_model_write_parameterized_model
  end interface kim_to_model_write_parameterized_model

  interface kim_to_model_compute_arguments
    module procedure kim_model_extension_to_model_compute_arguments
  end interface kim_to_model_compute_arguments

  interface kim_to_model_compute_arguments_create
    module procedure kim_model_extension_to_model_compute_arguments_create
  end interface kim_to_model_compute_arguments_create

  interface kim_to_model_compute_arguments_destroy
    module procedure kim_model_extension_to_model_compute_arguments_destroy
  end interface kim_to_model_compute_arguments_destroy

  interface kim_c_char_array_to_string
    module procedure kim_model_extension_convert_c_char_array_to_string
  end interface kim_c_char_array_to_string

  interface kim_c_char_ptr_to_string
    module procedure kim_model_extension_convert_c_char_ptr_to_string
  end interface kim_c_char_ptr_to_string

  interface kim_string_to_c_char_array
    module procedure kim_model_extension_convert_string_to_c_char_array
  end interface kim_string_to_c_char_array

  interface kim_get_model_buffer_pointer
    module procedure kim_model_extension_get_model_buffer_pointer
  end interface kim_get_model_buffer_pointer

  interface kim_log_entry
    module procedure kim_model_extension_log_entry
  end interface kim_log_entry

  interface kim_to_string
    module procedure kim_model_extension_to_string
  end interface kim_to_string

contains
  logical function kim_model_extension_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: lhs
    type(kim_model_extension_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_extension_handle_equal = .true.
    else
      kim_model_extension_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_extension_handle_equal

  logical function kim_model_extension_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: lhs
    type(kim_model_extension_handle_type), intent(in) :: rhs

    kim_model_extension_handle_not_equal = .not. (lhs .eq. rhs)
  end function kim_model_extension_handle_not_equal

  subroutine kim_model_extension_get_extension_id(model_extension_handle, &
    extension_id)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_model_extension_type
    implicit none
    interface
      subroutine get_extension_id(model_extension, extension_id) &
        bind(c, name="KIM_ModelExtension_GetExtensionID")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
        type(c_ptr), intent(out) :: extension_id
      end subroutine get_extension_id
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    character(len=*, kind=c_char), intent(out) :: extension_id
    type(kim_model_extension_type), pointer :: model_extension

    type(c_ptr) :: p

    call c_f_pointer(model_extension_handle%p, model_extension)
    call get_extension_id(model_extension, p)
    call kim_convert_c_char_ptr_to_string(p, extension_id)
  end subroutine kim_model_extension_get_extension_id

  subroutine kim_model_extension_to_model(model_extension_handle, model_handle)
    use kim_model_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_handle_type), intent(out) :: model_handle

    model_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model

  subroutine kim_model_extension_to_model_compute(model_extension_handle, &
    model_compute_handle)
    use kim_model_compute_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_compute_handle_type), intent(out) :: model_compute_handle

    model_compute_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_compute

  subroutine kim_model_extension_to_model_create(model_extension_handle, &
    model_create_handle)
    use kim_model_create_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_create_handle_type), intent(out) :: model_create_handle

    model_create_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_create

  subroutine kim_model_extension_to_model_destroy(model_extension_handle, &
    model_destroy_handle)
    use kim_model_destroy_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_destroy_handle_type), intent(out) :: model_destroy_handle

    model_destroy_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_destroy

  subroutine kim_model_extension_to_model_driver_create( &
    model_extension_handle, model_driver_create_handle)
    use kim_model_driver_create_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_driver_create_handle_type), intent(out) &
      :: model_driver_create_handle

    model_driver_create_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_driver_create

  subroutine kim_model_extension_to_model_refresh(model_extension_handle, &
    model_refresh_handle)
    use kim_model_refresh_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_refresh_handle_type), intent(out) :: model_refresh_handle

    model_refresh_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_refresh

  subroutine kim_model_extension_to_model_write_parameterized_model( &
    model_extension_handle, model_write_parameterized_model_handle)
    use kim_model_write_parameterized_model_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_model_write_parameterized_model_handle_type), intent(out) &
      :: model_write_parameterized_model_handle

    model_write_parameterized_model_handle%p = model_extension_handle%p
  end subroutine kim_model_extension_to_model_write_parameterized_model

  subroutine kim_model_extension_to_model_compute_arguments( &
    model_extension_handle, compute_arguments_c_ptr, &
    model_compute_arguments_handle)
    use kim_model_compute_arguments_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(in) :: compute_arguments_c_ptr
    type(kim_model_compute_arguments_handle_type), intent(out) &
      :: model_compute_arguments_handle

    ! avoid unused dummy argument warnings
    if (model_extension_handle .eq. KIM_MODEL_EXTENSION_NULL_HANDLE) continue

    model_compute_arguments_handle%p = compute_arguments_c_ptr
  end subroutine kim_model_extension_to_model_compute_arguments

  subroutine kim_model_extension_to_model_compute_arguments_create( &
    model_extension_handle, compute_arguments_c_ptr, &
    model_compute_arguments_create_handle)
    use kim_model_compute_arguments_create_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(in) :: compute_arguments_c_ptr
    type(kim_model_compute_arguments_create_handle_type), intent(out) &
      :: model_compute_arguments_create_handle

    ! avoid unused dummy argument warnings
    if (model_extension_handle .eq. KIM_MODEL_EXTENSION_NULL_HANDLE) continue

    model_compute_arguments_create_handle%p = compute_arguments_c_ptr
  end subroutine kim_model_extension_to_model_compute_arguments_create

  subroutine kim_model_extension_to_model_compute_arguments_destroy( &
    model_extension_handle, compute_arguments_c_ptr, &
    model_compute_arguments_destroy_handle)
    use kim_model_compute_arguments_destroy_module
    implicit none
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(in) :: compute_arguments_c_ptr
    type(kim_model_compute_arguments_destroy_handle_type), intent(out) &
      :: model_compute_arguments_destroy_handle

    ! avoid unused dummy argument warnings
    if (model_extension_handle .eq. KIM_MODEL_EXTENSION_NULL_HANDLE) continue

    model_compute_arguments_destroy_handle%p = compute_arguments_c_ptr
  end subroutine kim_model_extension_to_model_compute_arguments_destroy

  subroutine kim_model_extension_convert_c_char_array_to_string( &
    c_char_array, string)
    use kim_convert_string_module, only : kim_convert_c_char_array_to_string
    implicit none
    character(len=1, kind=c_char), intent(in) :: c_char_array(:)
    character(len=*, kind=c_char), intent(out) :: string

    call kim_convert_c_char_array_to_string(c_char_array, string)
  end subroutine kim_model_extension_convert_c_char_array_to_string

  subroutine kim_model_extension_convert_c_char_ptr_to_string( &
    c_char_ptr, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    type(c_ptr), intent(in) :: c_char_ptr
    character(len=*, kind=c_char), intent(out) :: string

    call kim_convert_c_char_ptr_to_string(c_char_ptr, string)
  end subroutine kim_model_extension_convert_c_char_ptr_to_string

  subroutine kim_model_extension_convert_string_to_c_char_array( &
    string, c_char_array)
    use kim_convert_string_module, only : kim_convert_string_to_c_char_array
    implicit none
    character(len=*, kind=c_char), intent(in) :: string
    character(len=1, kind=c_char), intent(out) :: c_char_array(:)

    call kim_convert_string_to_c_char_array(string, c_char_array)
  end subroutine kim_model_extension_convert_string_to_c_char_array

  subroutine kim_model_extension_get_model_buffer_pointer( &
    model_extension_handle, ptr)
    use kim_interoperable_types_module, only : kim_model_extension_type
    implicit none
    interface
      subroutine get_model_buffer_pointer(model_extension, ptr) &
        bind(c, name="KIM_ModelExtension_GetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
        type(c_ptr), intent(out) :: ptr
      end subroutine get_model_buffer_pointer
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_extension_type), pointer :: model_extension

    call c_f_pointer(model_extension_handle%p, model_extension)
    call get_model_buffer_pointer(model_extension, ptr)
  end subroutine kim_model_extension_get_model_buffer_pointer

  subroutine kim_model_extension_log_entry(model_extension_handle, &
    log_verbosity, message)
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    use kim_interoperable_types_module, only : kim_model_extension_type
    implicit none
    interface
      subroutine log_entry(model_extension, log_verbosity, message, &
        line_number, file_name) bind(c, name="KIM_ModelExtension_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        use kim_interoperable_types_module, only : kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_extension_type), pointer :: model_extension

    call c_f_pointer(model_extension_handle%p, model_extension)
    call log_entry(model_extension, log_verbosity, trim(message)//c_null_char, &
      0, ""//c_null_char)
  end subroutine kim_model_extension_log_entry

  subroutine kim_model_extension_to_string(model_extension_handle, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only : kim_model_extension_type
    implicit none
    interface
      type(c_ptr) function model_extension_string(model_extension) &
        bind(c, name="KIM_ModelExtension_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only : kim_model_extension_type
        implicit none
        type(kim_model_extension_type), intent(in) :: model_extension
      end function model_extension_string
    end interface
    type(kim_model_extension_handle_type), intent(in) :: model_extension_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_extension_type), pointer :: model_extension

    type(c_ptr) :: p

    call c_f_pointer(model_extension_handle%p, model_extension)
    p = model_extension_string(model_extension)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_extension_to_string
end module kim_model_extension_module
