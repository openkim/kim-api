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


module kim_charge_unit_module
  use, intrinsic :: iso_c_binding
  use kim_charge_unit_id_module
  implicit none
  private

  public &
    kim_charge_unit_type, &
    kim_charge_unit_from_string, &
    operator (.eq.), &
    operator (.ne.), &
    kim_charge_unit_string, &

    kim_charge_unit_unused, &
    kim_charge_unit_c, &
    kim_charge_unit_e, &
    kim_charge_unit_statc

  type, bind(c) :: kim_charge_unit_type
    integer(c_int) charge_unit_id
  end type kim_charge_unit_type

  type(kim_charge_unit_type), parameter :: kim_charge_unit_unused = &
    kim_charge_unit_type(charge_unit_unused_id)
  type(kim_charge_unit_type), parameter :: kim_charge_unit_c = &
    kim_charge_unit_type(charge_unit_c_id)
  type(kim_charge_unit_type), parameter :: kim_charge_unit_e = &
    kim_charge_unit_type(charge_unit_e_id)
  type(kim_charge_unit_type), parameter :: kim_charge_unit_statc = &
    kim_charge_unit_type(charge_unit_statc_id)

  interface operator (.eq.)
    logical function kim_charge_unit_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_charge_unit_type
      implicit none
      type(kim_charge_unit_type), intent(in) :: left
      type(kim_charge_unit_type), intent(in) :: right
    end function kim_charge_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_charge_unit_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_charge_unit_type
      implicit none
      type(kim_charge_unit_type), intent(in) :: left
      type(kim_charge_unit_type), intent(in) :: right
    end function kim_charge_unit_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_charge_unit_from_string(string, charge_unit)
      import kim_charge_unit_type
      implicit none
      character(len=*), intent(in) :: string
      type(kim_charge_unit_type), intent(out) :: charge_unit
    end subroutine kim_charge_unit_from_string

    subroutine kim_charge_unit_string(charge_unit, string)
      import kim_charge_unit_type
      implicit none
      type(kim_charge_unit_type), intent(in), value :: charge_unit
      character(len=*), intent(out) :: string
    end subroutine kim_charge_unit_string
  end interface
end module kim_charge_unit_module
