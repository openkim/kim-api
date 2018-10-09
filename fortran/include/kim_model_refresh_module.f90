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


module kim_model_refresh_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_refresh_handle_type, &

    ! Constants
    KIM_MODEL_REFRESH_NULL_HANDLE, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_set_influence_distance_pointer, &
    kim_set_neighbor_list_pointers, &
    kim_get_model_buffer_pointer, &
    kim_log_entry, &
    kim_to_string


  type, bind(c) :: kim_model_refresh_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_refresh_handle_type

  type, bind(c) :: kim_model_refresh_type
    private
    type(c_ptr) :: p
  end type kim_model_refresh_type

  type(kim_model_refresh_handle_type), protected, save &
    :: KIM_MODEL_REFRESH_NULL_HANDLE

  interface operator (.eq.)
    module procedure kim_model_refresh_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_model_refresh_handle_not_equal
  end interface operator (.ne.)

  interface kim_set_influence_distance_pointer
    module procedure kim_model_refresh_set_influence_distance_pointer
  end interface kim_set_influence_distance_pointer

  interface kim_set_neighbor_list_pointers
    module procedure kim_model_refresh_set_neighbor_list_pointers
  end interface kim_set_neighbor_list_pointers

  interface kim_get_model_buffer_pointer
    module procedure kim_model_refresh_get_model_buffer_pointer
  end interface kim_get_model_buffer_pointer

  interface kim_log_entry
    module procedure kim_model_refresh_log_entry
  end interface kim_log_entry

  interface kim_to_string
    module procedure kim_model_refresh_to_string
  end interface kim_to_string

contains
  logical function kim_model_refresh_handle_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_model_refresh_handle_type), intent(in) :: left
    type(kim_model_refresh_handle_type), intent(in) :: right

    if ((.not. c_associated(left%p) .and. c_associated(right%p))) then
      kim_model_refresh_handle_equal = .true.
    else
      kim_model_refresh_handle_equal = c_associated(left%p, right%p)
    end if
  end function kim_model_refresh_handle_equal

  logical function kim_model_refresh_handle_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_model_refresh_handle_type), intent(in) :: left
    type(kim_model_refresh_handle_type), intent(in) :: right

    kim_model_refresh_handle_not_equal = .not. (left .eq. right)
  end function kim_model_refresh_handle_not_equal

  subroutine kim_model_refresh_set_influence_distance_pointer( &
    model_refresh_handle, influence_distance)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine set_influence_distance_pointer(model_refresh, &
        influence_distance) &
        bind(c, name="KIM_ModelRefresh_SetInfluenceDistancePointer")
        use, intrinsic :: iso_c_binding
        import kim_model_refresh_type
        implicit none
        type(kim_model_refresh_type), intent(inout) :: &
          model_refresh
        type(c_ptr), intent(in), value :: influence_distance
      end subroutine set_influence_distance_pointer
    end interface
    type(kim_model_refresh_handle_type), intent(inout) :: model_refresh_handle
    real(c_double), intent(in), target :: influence_distance
    type(kim_model_refresh_type), pointer :: model_refresh

    call c_f_pointer(model_refresh_handle%p, model_refresh)
    call set_influence_distance_pointer(model_refresh, &
      c_loc(influence_distance))
  end subroutine kim_model_refresh_set_influence_distance_pointer

  subroutine kim_model_refresh_set_neighbor_list_pointers( &
    model_refresh_handle, number_of_neighbor_lists, cutoffs, &
    modelWillNotRequestNeighborsOfNoncontributingParticles)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine set_neighbor_list_pointers(model_refresh, &
        number_of_neighbor_lists, cutoffs_ptr, &
        modelWillNotRequestNeighborsOfNoncontributingParticles) &
        bind(c, name="KIM_ModelRefresh_SetNeighborListPointers")
        use, intrinsic :: iso_c_binding
        import kim_model_refresh_type
        implicit none
        type(kim_model_refresh_type), intent(inout) :: &
          model_refresh
        integer(c_int), intent(in), value :: number_of_neighbor_lists
        type(c_ptr), intent(in), value :: cutoffs_ptr
        type(c_ptr), intent(in), value :: &
          modelWillNotRequestNeighborsOfNoncontributingParticles
      end subroutine set_neighbor_list_pointers
    end interface
    type(kim_model_refresh_handle_type), intent(inout) :: model_refresh_handle
    integer(c_int), intent(in), value :: number_of_neighbor_lists
    real(c_double), intent(in), target :: cutoffs(number_of_neighbor_lists)
    integer(c_int), intent(in), target :: &
      modelWillNotRequestNeighborsOfNoncontributingParticles( &
      number_of_neighbor_lists)
    type(kim_model_refresh_type), pointer :: model_refresh

    call c_f_pointer(model_refresh_handle%p, model_refresh)
    call set_neighbor_list_pointers(model_refresh, number_of_neighbor_lists, &
      c_loc(cutoffs), &
      c_loc(modelWillNotRequestNeighborsOfNoncontributingParticles))
  end subroutine kim_model_refresh_set_neighbor_list_pointers

  subroutine kim_model_refresh_get_model_buffer_pointer( &
    model_refresh_handle, ptr)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine get_model_buffer_pointer(model_refresh, ptr) &
        bind(c, name="KIM_ModelRefresh_GetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        import kim_model_refresh_type
        implicit none
        type(kim_model_refresh_type), intent(in) :: &
          model_refresh
        type(c_ptr), intent(out) :: ptr
      end subroutine get_model_buffer_pointer
    end interface
    type(kim_model_refresh_handle_type), intent(in) :: model_refresh_handle
    type(c_ptr), intent(out) :: ptr
    type(kim_model_refresh_type), pointer :: model_refresh

    call c_f_pointer(model_refresh_handle%p, model_refresh)
    call get_model_buffer_pointer(model_refresh, ptr)
  end subroutine kim_model_refresh_get_model_buffer_pointer

  subroutine kim_model_refresh_log_entry(model_refresh_handle, &
    log_verbosity, message)
    use, intrinsic :: iso_c_binding
    use kim_log_verbosity_module, only : kim_log_verbosity_type
    implicit none
    interface
      subroutine log_entry(model_refresh, log_verbosity, message, &
        line_number, file_name) bind(c, name="KIM_ModelRefresh_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only : kim_log_verbosity_type
        import kim_model_refresh_type
        implicit none
        type(kim_model_refresh_type), intent(in) :: &
          model_refresh
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_refresh_handle_type), intent(in) :: model_refresh_handle
    type(kim_log_verbosity_type), intent(in), value :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_refresh_type), pointer :: model_refresh

    call c_f_pointer(model_refresh_handle%p, model_refresh)
    call log_entry(model_refresh, log_verbosity, trim(message)//c_null_char, &
      0, ""//c_null_char)
  end subroutine kim_model_refresh_log_entry

  subroutine kim_model_refresh_to_string(model_refresh_handle, string)
    use, intrinsic :: iso_c_binding
    use kim_convert_string_module, only : kim_convert_string
    implicit none
    interface
      type(c_ptr) function model_refresh_string(model_refresh) &
        bind(c, name="KIM_ModelRefresh_ToString")
        use, intrinsic :: iso_c_binding
        import kim_model_refresh_type
        implicit none
        type(kim_model_refresh_type), intent(in) :: &
          model_refresh
      end function model_refresh_string
    end interface
    type(kim_model_refresh_handle_type), intent(in) :: model_refresh_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_refresh_type), pointer :: model_refresh

    type(c_ptr) :: p

    call c_f_pointer(model_refresh_handle%p, model_refresh)
    p = model_refresh_string(model_refresh)
    if (c_associated(p)) then
      call kim_convert_string(p, string)
    else
      string = ""
    end if
  end subroutine kim_model_refresh_to_string
end module kim_model_refresh_module
