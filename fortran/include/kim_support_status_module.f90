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


module kim_support_status_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_support_status_type, &

    ! Constants
    KIM_SUPPORT_STATUS_REQUIRED_BY_API, &
    KIM_SUPPORT_STATUS_NOT_SUPPORTED, &
    KIM_SUPPORT_STATUS_REQUIRED, &
    KIM_SUPPORT_STATUS_OPTIONAL, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_support_statuses, &
    kim_get_support_status


  type, bind(c) :: kim_support_status_type
    integer(c_int) :: support_status_id
  end type kim_support_status_type

  type(kim_support_status_type), protected, &
    bind(c, name="KIM_SUPPORT_STATUS_requiredByAPI") &
    :: KIM_SUPPORT_STATUS_REQUIRED_BY_API
  type(kim_support_status_type), protected, &
    bind(c, name="KIM_SUPPORT_STATUS_notSupported") &
    :: KIM_SUPPORT_STATUS_NOT_SUPPORTED
  type(kim_support_status_type), protected, &
    bind(c, name="KIM_SUPPORT_STATUS_required") &
    :: KIM_SUPPORT_STATUS_REQUIRED
  type(kim_support_status_type), protected, &
    bind(c, name="KIM_SUPPORT_STATUS_optional") &
    :: KIM_SUPPORT_STATUS_OPTIONAL

  interface operator (.eq.)
    module procedure kim_support_status_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_support_status_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_support_status_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_support_status_to_string
  end interface kim_to_string

contains
  logical function kim_support_status_equal(lhs, rhs)
    implicit none
    type(kim_support_status_type), intent(in) :: lhs
    type(kim_support_status_type), intent(in) :: rhs

    kim_support_status_equal &
      = (lhs%support_status_id .eq. rhs%support_status_id)
  end function kim_support_status_equal

  logical function kim_support_status_not_equal(lhs, rhs)
    implicit none
    type(kim_support_status_type), intent(in) :: lhs
    type(kim_support_status_type), intent(in) :: rhs

    kim_support_status_not_equal = .not. (lhs .eq. rhs)
  end function kim_support_status_not_equal

  subroutine kim_support_status_from_string(string, support_status)
    implicit none
    interface
      type(kim_support_status_type) function from_string(string) &
        bind(c, name="KIM_SupportStatus_FromString")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_support_status_type), intent(out) :: support_status

    support_status = from_string(trim(string)//c_null_char)
  end subroutine kim_support_status_from_string

  subroutine kim_support_status_to_string(support_status, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) function get_string(support_status) &
        bind(c, name="KIM_SupportStatus_ToString")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        type(kim_support_status_type), intent(in), value :: support_status
      end function get_string
    end interface
    type(kim_support_status_type), intent(in) :: support_status
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(support_status)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_support_status_to_string

  subroutine kim_get_number_of_support_statuses(number_of_support_statuses)
    implicit none
    interface
      subroutine get_number_of_support_statuses(number_of_support_statuses) &
        bind(c, name="KIM_SUPPORT_STATUS_GetNumberOfSupportStatuses")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_support_statuses
      end subroutine get_number_of_support_statuses
    end interface
    integer(c_int), intent(out) :: number_of_support_statuses

    call get_number_of_support_statuses(number_of_support_statuses)
  end subroutine kim_get_number_of_support_statuses

  subroutine kim_get_support_status(index, support_status, ierr)
    implicit none
    interface
      integer(c_int) function get_support_status(index, support_status) &
        bind(c, name="KIM_SUPPORT_STATUS_GetSupportStatus")
        use, intrinsic :: iso_c_binding
        import kim_support_status_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_support_status_type), intent(out) :: support_status
      end function get_support_status
    end interface
    integer(c_int), intent(in) :: index
    type(kim_support_status_type), intent(out) :: support_status
    integer(c_int), intent(out) :: ierr

    ierr = get_support_status(index-1, support_status)
  end subroutine kim_get_support_status
end module kim_support_status_module
