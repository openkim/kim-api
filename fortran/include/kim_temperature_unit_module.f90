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
! Copyright (c) 2016--2017, Regents of the University of Minnesota.
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
  use kim_temperature_unit_id_module
  implicit none
  private

  public &
    kim_temperature_unit_type, &
    operator (.eq.), &
    operator (.ne.), &
    kim_temperature_unit_string, &

    kim_temperature_unit_unused, &
    kim_temperature_unit_k

  type, bind(c) :: kim_temperature_unit_type
    integer(c_int) temperature_unit_id
  end type kim_temperature_unit_type

  type(kim_temperature_unit_type), parameter :: kim_temperature_unit_unused = &
    kim_temperature_unit_type(temperature_unit_unused_id)
  type(kim_temperature_unit_type), parameter :: kim_temperature_unit_k = &
    kim_temperature_unit_type(temperature_unit_k_id)

  interface operator (.eq.)
    module procedure kim_temperature_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_temperature_unit_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_temperature_unit_string(temperature_unit, unit_string)
      import kim_temperature_unit_type
      implicit none
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_temperature_unit_string
  end interface

contains
  logical function kim_temperature_unit_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_temperature_unit_type), intent(in) :: left
    type(kim_temperature_unit_type), intent(in) :: right

    kim_temperature_unit_equal &
      = (left%temperature_unit_id .eq. right%temperature_unit_id)
  end function kim_temperature_unit_equal

  logical function kim_temperature_unit_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_temperature_unit_type), intent(in) :: left
    type(kim_temperature_unit_type), intent(in) :: right

    kim_temperature_unit_not_equal = .not. (left .eq. right)
  end function kim_temperature_unit_not_equal
end module kim_temperature_unit_module
