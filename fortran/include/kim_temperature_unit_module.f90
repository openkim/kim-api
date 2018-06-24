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


module kim_temperature_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_temperature_unit_type, &
    kim_temperature_unit_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_temperature_unit_string, &

    kim_temperature_unit_unused, &
    kim_temperature_unit_k, &

    kim_temperature_unit_get_number_of_temperature_units, &
    kim_temperature_unit_get_temperature_unit


  type, bind(c) :: kim_temperature_unit_type
    integer(c_int) temperature_unit_id
  end type kim_temperature_unit_type

  type(kim_temperature_unit_type), protected, &
    bind(c, name="KIM_TEMPERATURE_UNIT_unused") &
    :: kim_temperature_unit_unused
  type(kim_temperature_unit_type), protected, &
    bind(c, name="KIM_TEMPERATURE_UNIT_K") &
    :: kim_temperature_unit_k

  interface operator (.eq.)
    logical function kim_temperature_unit_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_temperature_unit_type
      implicit none
      type(kim_temperature_unit_type), intent(in) :: left
      type(kim_temperature_unit_type), intent(in) :: right
    end function kim_temperature_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_temperature_unit_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_temperature_unit_type
      implicit none
      type(kim_temperature_unit_type), intent(in) :: left
      type(kim_temperature_unit_type), intent(in) :: right
    end function kim_temperature_unit_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_temperature_unit_from_string(string, temperature_unit)
      import kim_temperature_unit_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_temperature_unit_type), intent(out) :: temperature_unit
    end subroutine kim_temperature_unit_from_string

    subroutine kim_temperature_unit_string(temperature_unit, string)
      import kim_temperature_unit_type
      implicit none
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      character(len=*), intent(out) :: string
    end subroutine kim_temperature_unit_string

    subroutine kim_temperature_unit_get_number_of_temperature_units( &
      number_of_temperature_units)
      use, intrinsic :: iso_c_binding
      implicit none
      integer(c_int), intent(out) :: number_of_temperature_units
    end subroutine kim_temperature_unit_get_number_of_temperature_units

    subroutine kim_temperature_unit_get_temperature_unit(index, &
      temperature_unit, ierr)
      use, intrinsic :: iso_c_binding
      import kim_temperature_unit_type
      implicit none
      integer(c_int), intent(in), value :: index
      type(kim_temperature_unit_type), intent(out) :: temperature_unit
      integer(c_int), intent(out) :: ierr
    end subroutine kim_temperature_unit_get_temperature_unit
  end interface
end module kim_temperature_unit_module
