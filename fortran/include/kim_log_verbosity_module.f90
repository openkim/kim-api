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


module kim_log_verbosity_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_log_verbosity_type, &

    ! Constants
    KIM_LOG_VERBOSITY_SILENT, &
    KIM_LOG_VERBOSITY_FATAL, &
    KIM_LOG_VERBOSITY_ERROR, &
    KIM_LOG_VERBOSITY_WARNING, &
    KIM_LOG_VERBOSITY_INFORMATION, &
    KIM_LOG_VERBOSITY_DEBUG, &

    ! Routines
    kim_known, &
    operator (.lt.), &
    operator (.gt.), &
    operator (.le.), &
    operator (.ge.), &
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_log_verbosities, &
    kim_get_log_verbosity


  type, bind(c) :: kim_log_verbosity_type
    integer(c_int) :: log_verbosity_id
  end type kim_log_verbosity_type

  type(kim_log_verbosity_type), protected, save, &
    bind(c, name="KIM_LOG_VERBOSITY_silent") &
    :: KIM_LOG_VERBOSITY_SILENT
  type(kim_log_verbosity_type), protected, save, &
    bind(c, name="KIM_LOG_VERBOSITY_fatal") &
    :: KIM_LOG_VERBOSITY_FATAL
  type(kim_log_verbosity_type), protected, save, &
    bind(c, name="KIM_LOG_VERBOSITY_error") &
    :: KIM_LOG_VERBOSITY_ERROR
  type(kim_log_verbosity_type), protected, save, &
    bind(c, name="KIM_LOG_VERBOSITY_warning") &
    :: KIM_LOG_VERBOSITY_WARNING
  type(kim_log_verbosity_type), protected, save, &
    bind(c, name="KIM_LOG_VERBOSITY_information") &
    :: KIM_LOG_VERBOSITY_INFORMATION
  type(kim_log_verbosity_type), protected, save, &
    bind(c, name="KIM_LOG_VERBOSITY_debug") &
    :: KIM_LOG_VERBOSITY_DEBUG

  interface kim_known
    module procedure kim_log_verbosity_known
  end interface kim_known

  interface operator (.lt.)
    module procedure kim_log_verbosity_less_than
  end interface operator (.lt.)

  interface operator (.gt.)
    module procedure kim_log_verbosity_greater_than
  end interface operator (.gt.)

  interface operator (.le.)
    module procedure kim_log_verbosity_less_than_equal
  end interface operator (.le.)

  interface operator (.ge.)
    module procedure kim_log_verbosity_greater_than_equal
  end interface operator (.ge.)

  interface operator (.eq.)
    module procedure kim_log_verbosity_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_log_verbosity_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_log_verbosity_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_log_verbosity_to_string
  end interface kim_to_string

contains
  logical recursive function kim_log_verbosity_known(log_verbosity)
    implicit none
    interface
      integer(c_int) recursive function known(log_verbosity) &
        bind(c, name="KIM_ChargeUnit_Known")
        use, intrinsic :: iso_c_binding
        import kim_log_verbosity_type
        implicit none
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end function known
    end interface
    type(kim_log_verbosity_type), intent(in) :: log_verbosity

    kim_log_verbosity_known = (known(log_verbosity) /= 0)
  end function kim_log_verbosity_known

  logical recursive function kim_log_verbosity_less_than(lhs, rhs)
    implicit none
    type(kim_log_verbosity_type), intent(in) :: lhs
    type(kim_log_verbosity_type), intent(in) :: rhs

    kim_log_verbosity_less_than &
      = (lhs%log_verbosity_id .lt. rhs%log_verbosity_id)
  end function kim_log_verbosity_less_than

  logical recursive function kim_log_verbosity_greater_than(lhs, rhs)
    implicit none
    type(kim_log_verbosity_type), intent(in) :: lhs
    type(kim_log_verbosity_type), intent(in) :: rhs

    kim_log_verbosity_greater_than &
      = (lhs%log_verbosity_id .ge. rhs%log_verbosity_id)
  end function kim_log_verbosity_greater_than

  logical recursive function kim_log_verbosity_less_than_equal(lhs, rhs)
    implicit none
    type(kim_log_verbosity_type), intent(in) :: lhs
    type(kim_log_verbosity_type), intent(in) :: rhs

    kim_log_verbosity_less_than_equal &
      = (lhs%log_verbosity_id .le. rhs%log_verbosity_id)
  end function kim_log_verbosity_less_than_equal

  logical recursive function kim_log_verbosity_greater_than_equal(lhs, rhs)
    implicit none
    type(kim_log_verbosity_type), intent(in) :: lhs
    type(kim_log_verbosity_type), intent(in) :: rhs

    kim_log_verbosity_greater_than_equal &
      = (lhs%log_verbosity_id .ge. rhs%log_verbosity_id)
  end function kim_log_verbosity_greater_than_equal

  logical recursive function kim_log_verbosity_equal(lhs, rhs)
    implicit none
    type(kim_log_verbosity_type), intent(in) :: lhs
    type(kim_log_verbosity_type), intent(in) :: rhs

    kim_log_verbosity_equal &
      = (lhs%log_verbosity_id .eq. rhs%log_verbosity_id)
  end function kim_log_verbosity_equal

  logical recursive function kim_log_verbosity_not_equal(lhs, rhs)
    implicit none
    type(kim_log_verbosity_type), intent(in) :: lhs
    type(kim_log_verbosity_type), intent(in) :: rhs

    kim_log_verbosity_not_equal = .not. (lhs .eq. rhs)
  end function kim_log_verbosity_not_equal

  recursive subroutine kim_log_verbosity_from_string(string, log_verbosity)
    implicit none
    interface
      type(kim_log_verbosity_type) recursive function from_string(string) &
        bind(c, name="KIM_LogVerbosity_FromString")
        use, intrinsic :: iso_c_binding
        import kim_log_verbosity_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_log_verbosity_type), intent(out) :: log_verbosity

    log_verbosity = from_string(trim(string)//c_null_char)
  end subroutine kim_log_verbosity_from_string

  recursive subroutine kim_log_verbosity_to_string(log_verbosity, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(log_verbosity) &
        bind(c, name="KIM_LogVerbosity_ToString")
        use, intrinsic :: iso_c_binding
        import kim_log_verbosity_type
        implicit none
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      end function get_string
    end interface
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(log_verbosity)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_log_verbosity_to_string

  recursive subroutine kim_get_number_of_log_verbosities( &
    number_of_log_verbosities)
    implicit none
    interface
      recursive subroutine get_number_of_log_verbosities( &
        number_of_log_verbosities) &
        bind(c, name="KIM_LOG_VERBOSITY_GetNumberOfLogVerbosities")
        use, intrinsic :: iso_c_binding
        implicit none
        integer(c_int), intent(out) :: number_of_log_verbosities
      end subroutine get_number_of_log_verbosities
    end interface
    integer(c_int), intent(out) :: number_of_log_verbosities

    call get_number_of_log_verbosities(number_of_log_verbosities)
  end subroutine kim_get_number_of_log_verbosities

  recursive subroutine kim_get_log_verbosity(index, log_verbosity, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_log_verbosity(index, &
        log_verbosity) bind(c, name="KIM_LOG_VERBOSITY_GetLogVerbosity")
        use, intrinsic :: iso_c_binding
        import kim_log_verbosity_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_log_verbosity_type), intent(out) :: log_verbosity
      end function get_log_verbosity
    end interface
    integer(c_int), intent(in) :: index
    type(kim_log_verbosity_type), intent(out) :: log_verbosity
    integer(c_int), intent(out) :: ierr

    ierr = get_log_verbosity(index-1, log_verbosity)
  end subroutine kim_get_log_verbosity
end module kim_log_verbosity_module
