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


module kim_energy_unit_module
  use, intrinsic :: iso_c_binding
  use kim_energy_unit_id_module
  implicit none
  private

  public &
    kim_energy_unit_type, &
    operator (.eq.), &
    operator (.ne.), &
    kim_energy_unit_string, &

    kim_energy_unit_unused, &
    kim_energy_unit_amu_a2_per_ps2, &
    kim_energy_unit_erg, &
    kim_energy_unit_ev, &
    kim_energy_unit_hartree, &
    kim_energy_unit_j, &
    kim_energy_unit_kcal_mol

  type, bind(c) :: kim_energy_unit_type
    integer(c_int) energy_unit_id
  end type kim_energy_unit_type

  type(kim_energy_unit_type), parameter :: kim_energy_unit_unused = &
    kim_energy_unit_type(energy_unit_unused_id)
  type(kim_energy_unit_type), parameter :: kim_energy_unit_amu_a2_per_ps2 = &
    kim_energy_unit_type(energy_unit_amu_a2_per_ps2_id)
  type(kim_energy_unit_type), parameter :: kim_energy_unit_erg = &
    kim_energy_unit_type(energy_unit_erg_id)
  type(kim_energy_unit_type), parameter :: kim_energy_unit_ev = &
    kim_energy_unit_type(energy_unit_ev_id)
  type(kim_energy_unit_type), parameter :: kim_energy_unit_hartree = &
    kim_energy_unit_type(energy_unit_hartree_id)
  type(kim_energy_unit_type), parameter :: kim_energy_unit_j = &
    kim_energy_unit_type(energy_unit_j_id)
  type(kim_energy_unit_type), parameter :: kim_energy_unit_kcal_mol = &
    kim_energy_unit_type(energy_unit_kcal_mol_id)

  interface operator (.eq.)
    module procedure kim_energy_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_energy_unit_not_equal
  end interface operator (.ne.)

  interface
    subroutine kim_energy_unit_string(energy_unit, unit_string)
      import kim_energy_unit_type
      implicit none
      type(kim_energy_unit_type), intent(in), value :: energy_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_energy_unit_string
  end interface

contains
  logical function kim_energy_unit_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_energy_unit_type), intent(in) :: left
    type(kim_energy_unit_type), intent(in) :: right

    kim_energy_unit_equal &
      = (left%energy_unit_id .eq. right%energy_unit_id)
  end function kim_energy_unit_equal

  logical function kim_energy_unit_not_equal(left, right)
    use, intrinsic :: iso_c_binding
    implicit none
    type(kim_energy_unit_type), intent(in) :: left
    type(kim_energy_unit_type), intent(in) :: right

    kim_energy_unit_not_equal = .not. (left .eq. right)
  end function kim_energy_unit_not_equal
end module kim_energy_unit_module
