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


module kim_log_f_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_log_type, &
    create, &
    destroy, &
    get_id, &
    set_id, &
    push_verbosity, &
    pop_verbosity, &
    log_entry

  type, bind(c) :: kim_log_type
    private
    type(c_ptr) :: p
  end type kim_log_type

  interface
    integer(c_int) function create(log) bind(c, name="KIM_Log_Create")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(out) :: log
    end function create

    subroutine destroy(log) bind(c, name="KIM_Log_Destroy")
      use, intrinsic :: iso_c_binding
      implicit none
      type(c_ptr), intent(inout) :: log
    end subroutine destroy

    type(c_ptr) function get_id(log) bind(c, name="KIM_Log_GetID")
      use, intrinsic :: iso_c_binding
      import kim_log_type
      implicit none
      type(kim_log_type), intent(in) :: log
    end function get_id

    subroutine set_id(log, id_string) bind(c, name="KIM_Log_SetID")
      use, intrinsic :: iso_c_binding
      import kim_log_type
      implicit none
      type(kim_log_type), intent(inout) :: log
      character(c_char), intent(in) :: id_string(*)
    end subroutine set_id

    subroutine push_verbosity(log, log_verbosity) &
      bind(c, name="KIM_Log_PushVerbosity")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_log_type
      implicit none
      type(kim_log_type), intent(inout) :: log
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
    end subroutine push_verbosity

    subroutine pop_verbosity(log) bind(c, name="KIM_Log_PopVerbosity")
      use, intrinsic :: iso_c_binding
      import kim_log_type
      implicit none
      type(kim_log_type), intent(inout) :: log
    end subroutine pop_verbosity

    subroutine log_entry(log, log_verbosity, message, line_number, file_name) &
      bind(c, name="KIM_Log_LogEntry")
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_log_type
      implicit none
      type(kim_log_type), intent(in) :: log
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(c_char), intent(in) :: message(*)
      integer(c_int), intent(in), value :: line_number
      character(c_char), intent(in) :: file_name(*)
    end subroutine log_entry
  end interface
end module kim_log_f_module

! free functions to implement kim_log_module

logical function kim_log_handle_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  implicit none
  type(kim_log_handle_type), intent(in) :: left
  type(kim_log_handle_type), intent(in) :: right

  if ((.not. c_associated(left%p)) .and. (.not. c_associated(right%p))) then
    kim_log_handle_equal = .true.
  else
    kim_log_handle_equal = c_associated(left%p, right%p)
  end if
end function kim_log_handle_equal

logical function kim_log_handle_not_equal(left, right)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_module, only : operator (.eq.)
  implicit none
  type(kim_log_handle_type), intent(in) :: left
  type(kim_log_handle_type), intent(in) :: right

  kim_log_handle_not_equal = .not. (left .eq. right)
end function kim_log_handle_not_equal

subroutine kim_log_create(log_handle, ierr)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : create
  implicit none
  type(kim_log_handle_type), intent(out) :: log_handle
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: plog

  ierr = create(plog)
  log_handle%p = plog
end subroutine kim_log_create

subroutine kim_log_destroy(log_handle)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : destroy
  implicit none
  type(kim_log_handle_type), intent(inout) :: log_handle

  type(c_ptr) :: plog
  plog = log_handle%p
  call destroy(plog)
  log_handle%p = c_null_ptr
end subroutine kim_log_destroy

subroutine kim_log_get_id(log_handle, id_string)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : kim_log_type
  use kim_log_f_module, only : get_id
  use kim_convert_string_module, only : kim_convert_string
  implicit none
  type(kim_log_handle_type), intent(in) :: log_handle
  character(len=*, kind=c_char), intent(out) :: id_string
  type(kim_log_type), pointer :: log

  type(c_ptr) :: p

  call c_f_pointer(log_handle%p, log)
  p = get_id(log)
  if (c_associated(p)) then
    call kim_convert_string(p, id_string)
  else
    id_string = ""
  end if
end subroutine kim_log_get_id

subroutine kim_log_set_id(log_handle, id_string)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : kim_log_type
  use kim_log_f_module, only : set_id
  implicit none
  type(kim_log_handle_type), intent(in) :: log_handle
  character(len=*, kind=c_char), intent(in) :: id_string
  type(kim_log_type), pointer :: log

  call c_f_pointer(log_handle%p, log)
  call set_id(log, trim(id_string)//c_null_char)
end subroutine kim_log_set_id

subroutine kim_log_push_verbosity(log_handle, log_verbosity)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : kim_log_type
  use kim_log_f_module, only : push_verbosity
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_handle_type), intent(in) :: log_handle
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  type(kim_log_type), pointer :: log

  call c_f_pointer(log_handle%p, log)
  call push_verbosity(log, log_verbosity)
end subroutine kim_log_push_verbosity

subroutine kim_log_pop_verbosity(log_handle)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : kim_log_type
  use kim_log_f_module, only : pop_verbosity
  implicit none
  type(kim_log_handle_type), intent(in) :: log_handle
  type(kim_log_type), pointer :: log

  call c_f_pointer(log_handle%p, log)
  call pop_verbosity(log)
end subroutine kim_log_pop_verbosity

subroutine kim_log_log_entry(log_handle, log_verbosity, message, &
  line_number, file_name)
  use, intrinsic :: iso_c_binding
  use kim_log_module, only : kim_log_handle_type
  use kim_log_f_module, only : kim_log_type
  use kim_log_f_module, only : log_entry
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  implicit none
  type(kim_log_handle_type), intent(in) :: log_handle
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  character(len=*, kind=c_char), intent(in) :: message
  integer(c_int), intent(in), value :: line_number
  character(len=*, kind=c_char), intent(in) :: file_name
  type(kim_log_type), pointer :: log

  call c_f_pointer(log_handle%p, log)
  call log_entry(log, log_verbosity, trim(message)//c_null_char, &
    line_number, trim(file_name)//c_null_char)
end subroutine kim_log_log_entry
