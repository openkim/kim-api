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


module kim_model_routine_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_routine_name_type, &

    ! Constants
    KIM_MODEL_ROUTINE_NAME_CREATE, &
    KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_CREATE, &
    KIM_MODEL_ROUTINE_NAME_COMPUTE, &
    KIM_MODEL_ROUTINE_NAME_EXTENSION, &
    KIM_MODEL_ROUTINE_NAME_REFRESH, &
    KIM_MODEL_ROUTINE_NAME_WRITE_PARAMETERIZED_MODEL, &
    KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_DESTROY, &
    KIM_MODEL_ROUTINE_NAME_DESTROY, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_model_routine_names, &
    kim_get_model_routine_name


  type, bind(c) :: kim_model_routine_name_type
    integer(c_int) model_routine_name_id
  end type kim_model_routine_name_type

  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_Create") &
    :: KIM_MODEL_ROUTINE_NAME_CREATE
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate") &
    :: KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_CREATE
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_Compute") &
    :: KIM_MODEL_ROUTINE_NAME_COMPUTE
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_Extension") &
    :: KIM_MODEL_ROUTINE_NAME_EXTENSION
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_Refresh") &
    :: KIM_MODEL_ROUTINE_NAME_REFRESH
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_WriteParameterizedModel") &
    :: KIM_MODEL_ROUTINE_NAME_WRITE_PARAMETERIZED_MODEL
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy") &
    :: KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_DESTROY
  type(kim_model_routine_name_type), protected, save, &
    bind(c, name="KIM_MODEL_ROUTINE_NAME_Destroy") &
    :: KIM_MODEL_ROUTINE_NAME_DESTROY

  interface operator (.eq.)
    module procedure kim_model_routine_name_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_model_routine_name_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_model_routine_name_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_model_routine_name_to_string
  end interface kim_to_string

contains
  logical recursive function kim_model_routine_name_equal(lhs, rhs)
    implicit none
    type(kim_model_routine_name_type), intent(in) :: lhs
    type(kim_model_routine_name_type), intent(in) :: rhs

    kim_model_routine_name_equal &
      = (lhs%model_routine_name_id .eq. rhs%model_routine_name_id)
  end function kim_model_routine_name_equal

  logical recursive function kim_model_routine_name_not_equal(lhs, rhs)
    implicit none
    type(kim_model_routine_name_type), intent(in) :: lhs
    type(kim_model_routine_name_type), intent(in) :: rhs

    kim_model_routine_name_not_equal = .not. (lhs .eq. rhs)
  end function kim_model_routine_name_not_equal

  recursive subroutine kim_model_routine_name_from_string(string, &
    model_routine_name)
    implicit none
    interface
      type(kim_model_routine_name_type) recursive function from_string(string) &
        bind(c, name="KIM_ModelRoutineName_FromString")
        use, intrinsic :: iso_c_binding
        import kim_model_routine_name_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_model_routine_name_type), intent(out) :: model_routine_name

    model_routine_name = from_string(trim(string)//c_null_char)
  end subroutine kim_model_routine_name_from_string

  recursive subroutine kim_model_routine_name_to_string(model_routine_name, &
    string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(model_routine_name) &
        bind(c, name="KIM_ModelRoutineName_ToString")
        use, intrinsic :: iso_c_binding
        import kim_model_routine_name_type
        implicit none
        type(kim_model_routine_name_type), intent(in), value :: &
          model_routine_name
      end function get_string
    end interface
    type(kim_model_routine_name_type), intent(in) :: &
      model_routine_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(model_routine_name)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_routine_name_to_string

  recursive subroutine kim_get_number_of_model_routine_names( &
    number_of_model_routine_names)
    implicit none
    interface
      recursive subroutine get_number_of_model_routine_names( &
        number_of_model_routine_names) &
        bind(c, &
        name="KIM_MODEL_ROUTINE_NAME_GetNumberOfModelRoutineNames")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_model_routine_names
      end subroutine get_number_of_model_routine_names
    end interface
    integer(c_int), intent(out) :: number_of_model_routine_names

    call get_number_of_model_routine_names(number_of_model_routine_names)
  end subroutine kim_get_number_of_model_routine_names

  recursive subroutine kim_get_model_routine_name(index, &
    model_routine_name, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_model_routine_name(index, &
        model_routine_name) &
        bind(c, name="KIM_MODEL_ROUTINE_NAME_GetModelRoutineName")
        use, intrinsic :: iso_c_binding
        import kim_model_routine_name_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_model_routine_name_type), intent(out) :: &
          model_routine_name
      end function get_model_routine_name
    end interface
    integer(c_int), intent(in) :: index
    type(kim_model_routine_name_type), intent(out) :: model_routine_name
    integer(c_int), intent(out) :: ierr

    ierr = get_model_routine_name(index-1, model_routine_name)
  end subroutine kim_get_model_routine_name
end module kim_model_routine_name_module
