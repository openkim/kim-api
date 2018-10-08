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


module kim_time_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_time_unit_type, &

    ! Constants
    KIM_TIME_UNIT_UNUSED, &
    KIM_TIME_UNIT_FS, &
    KIM_TIME_UNIT_PS, &
    KIM_TIME_UNIT_NS, &
    KIM_TIME_UNIT_S, &

    ! Routines
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_time_units, &
    kim_get_time_unit


  type, bind(c) :: kim_time_unit_type
    integer(c_int) time_unit_id
  end type kim_time_unit_type

  type(kim_time_unit_type), protected, bind(c, name="KIM_TIME_UNIT_unused") &
    :: kim_time_unit_unused
  type(kim_time_unit_type), protected, bind(c, name="KIM_TIME_UNIT_fs") &
    :: kim_time_unit_fs
  type(kim_time_unit_type), protected, bind(c, name="KIM_TIME_UNIT_ps") &
    :: kim_time_unit_ps
  type(kim_time_unit_type), protected, bind(c, name="KIM_TIME_UNIT_ns") &
    :: kim_time_unit_ns
  type(kim_time_unit_type), protected, bind(c, name="KIM_TIME_UNIT_s") &
    :: kim_time_unit_s

  interface operator (.eq.)
    module procedure kim_time_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_time_unit_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_time_unit_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_time_unit_to_string
  end interface kim_to_string

  contains
    logical function kim_time_unit_equal(left, right)
      use, intrinsic :: iso_c_binding
      implicit none
      type(kim_time_unit_type), intent(in) :: left
      type(kim_time_unit_type), intent(in) :: right

      kim_time_unit_equal &
        = (left%time_unit_id .eq. right%time_unit_id)
    end function kim_time_unit_equal

    logical function kim_time_unit_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      implicit none
      type(kim_time_unit_type), intent(in) :: left
      type(kim_time_unit_type), intent(in) :: right

      kim_time_unit_not_equal = .not. (left .eq. right)
    end function kim_time_unit_not_equal

    subroutine kim_time_unit_from_string(string, time_unit)
      use, intrinsic :: iso_c_binding
      implicit none
      interface
        type(kim_time_unit_type) function from_string(string) &
          bind(c, name="KIM_TimeUnit_FromString")
          use, intrinsic :: iso_c_binding
          import kim_time_unit_type
          implicit none
          character(c_char), intent(in) :: string(*)
        end function from_string
      end interface
      character(len=*, kind=c_char), intent(in) :: string
      type(kim_time_unit_type), intent(out) :: time_unit

      time_unit = from_string(trim(string)//c_null_char)
    end subroutine kim_time_unit_from_string

    subroutine kim_time_unit_to_string(time_unit, string)
      use, intrinsic :: iso_c_binding
      use kim_convert_string_module, only : kim_convert_string
      implicit none
      interface
        type(c_ptr) function get_string(time_unit) &
          bind(c, name="KIM_TimeUnit_String")
          use, intrinsic :: iso_c_binding
          import kim_time_unit_type
          implicit none
          type(kim_time_unit_type), intent(in), value :: time_unit
        end function get_string
      end interface
      type(kim_time_unit_type), intent(in), value :: time_unit
      character(len=*, kind=c_char), intent(out) :: string

      type(c_ptr) :: p

      p = get_string(time_unit)
      if (c_associated(p)) then
        call kim_convert_string(p, string)
      else
        string = ""
      end if
    end subroutine kim_time_unit_to_string

    subroutine kim_get_number_of_time_units(number_of_time_units)
      use, intrinsic :: iso_c_binding
      implicit none
      interface
        subroutine get_number_of_time_units(number_of_time_units) &
          bind(c, name="KIM_TIME_UNIT_GetNumberOfTimeUnits")
          use, intrinsic :: iso_c_binding
          implicit none
          integer(c_int), intent(out) :: number_of_time_units
        end subroutine get_number_of_time_units
      end interface
      integer(c_int), intent(out) :: number_of_time_units

      call get_number_of_time_units(number_of_time_units)
    end subroutine kim_get_number_of_time_units

    subroutine kim_get_time_unit(index, time_unit, ierr)
      use, intrinsic :: iso_c_binding
      implicit none
      interface
        integer(c_int) function get_time_unit(index, time_unit) &
          bind(c, name="KIM_TIME_UNIT_GetTimeUnit")
          use, intrinsic :: iso_c_binding
          import kim_time_unit_type
          implicit none
          integer(c_int), intent(in), value :: index
          type(kim_time_unit_type), intent(out) :: time_unit
        end function get_time_unit
      end interface
      integer(c_int), intent(in), value :: index
      type(kim_time_unit_type), intent(out) :: time_unit
      integer(c_int), intent(out) :: ierr

      ierr = get_time_unit(index-1, time_unit)
    end subroutine kim_get_time_unit
  end module kim_time_unit_module
