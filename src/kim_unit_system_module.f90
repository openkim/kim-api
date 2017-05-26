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


module kim_unit_system_module
  use, intrinsic :: iso_c_binding
  use kim_unit_system_id_module
  implicit none
  private

  public &
    kim_length_unit_type, &
    kim_length_unit_string, &

    kim_energy_unit_type, &
    kim_energy_unit_string, &

    kim_charge_unit_type, &
    kim_charge_unit_string, &

    kim_temperature_unit_type, &
    kim_temperature_unit_string, &

    kim_time_unit_type, &
    kim_time_unit_string, &

    kim_units_a, &
    kim_units_bohr, &
    kim_units_cm, &
    kim_units_m, &
    kim_units_nm, &

    kim_units_amu_a2_per_ps2, &
    kim_units_erg, &
    kim_units_ev, &
    kim_units_hartree, &
    kim_units_j, &
    kim_units_kcal_mol, &

    kim_units_c, &
    kim_units_e, &
    kim_units_statc, &

    kim_units_k, &

    kim_units_fs, &
    kim_units_ps, &
    kim_units_ns, &
    kim_units_s

  type, bind(c) :: kim_length_unit_type
    integer(c_int) length_unit_id
  end type kim_length_unit_type

  type, bind(c) :: kim_energy_unit_type
    integer(c_int) energy_unit_id
  end type kim_energy_unit_type

  type, bind(c) :: kim_charge_unit_type
    integer(c_int) charge_unit_id
  end type kim_charge_unit_type

  type, bind(c) :: kim_temperature_unit_type
    integer(c_int) temperature_unit_id
  end type kim_temperature_unit_type

  type, bind(c) :: kim_time_unit_type
    integer(c_int) time_unit_id
  end type kim_time_unit_type

  type(kim_length_unit_type), parameter :: kim_units_a = &
    kim_length_unit_type(a_id)
  type(kim_length_unit_type), parameter :: kim_units_bohr = &
    kim_length_unit_type(bohr_id)
  type(kim_length_unit_type), parameter :: kim_units_cm = &
    kim_length_unit_type(cm_id)
  type(kim_length_unit_type), parameter :: kim_units_m = &
    kim_length_unit_type(m_id)
  type(kim_length_unit_type), parameter :: kim_units_nm = &
    kim_length_unit_type(nm_id)

  type(kim_energy_unit_type), parameter :: kim_units_amu_a2_per_ps2 = &
    kim_energy_unit_type(amu_a2_per_ps2_id)
  type(kim_energy_unit_type), parameter :: kim_units_erg = &
    kim_energy_unit_type(erg_id)
  type(kim_energy_unit_type), parameter :: kim_units_ev = &
    kim_energy_unit_type(ev_id)
  type(kim_energy_unit_type), parameter :: kim_units_hartree = &
    kim_energy_unit_type(hartree_id)
  type(kim_energy_unit_type), parameter :: kim_units_j = &
    kim_energy_unit_type(j_id)
  type(kim_energy_unit_type), parameter :: kim_units_kcal_mol = &
    kim_energy_unit_type(kcal_mol_id)

  type(kim_charge_unit_type), parameter :: kim_units_c = &
    kim_charge_unit_type(c_id)
  type(kim_charge_unit_type), parameter :: kim_units_e = &
    kim_charge_unit_type(e_id)
  type(kim_charge_unit_type), parameter :: kim_units_statc = &
    kim_charge_unit_type(statc_id)

  type(kim_temperature_unit_type), parameter :: kim_units_k = &
    kim_temperature_unit_type(k_id)

  type(kim_time_unit_type), parameter :: kim_units_fs = &
    kim_time_unit_type(fs_id)
  type(kim_time_unit_type), parameter :: kim_units_ps = &
    kim_time_unit_type(ps_id)
  type(kim_time_unit_type), parameter :: kim_units_ns = &
    kim_time_unit_type(ns_id)
  type(kim_time_unit_type), parameter :: kim_units_s = &
    kim_time_unit_type(s_id)

  interface
    subroutine kim_length_unit_string(length_unit, unit_string)
      import kim_length_unit_type
      implicit none
      type(kim_length_unit_type), intent(in), value :: length_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_length_unit_string

    subroutine kim_energy_unit_string(energy_unit, unit_string)
      import kim_energy_unit_type
      implicit none
      type(kim_energy_unit_type), intent(in), value :: energy_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_energy_unit_string

    subroutine kim_charge_unit_string(charge_unit, unit_string)
      import kim_charge_unit_type
      implicit none
      type(kim_charge_unit_type), intent(in), value :: charge_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_charge_unit_string

    subroutine kim_temperature_unit_string(temperature_unit, unit_string)
      import kim_temperature_unit_type
      implicit none
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_temperature_unit_string

    subroutine kim_time_unit_string(time_unit, unit_string)
      import kim_time_unit_type
      implicit none
      type(kim_time_unit_type), intent(in), value :: time_unit
      character(len=*), intent(out) :: unit_string
    end subroutine kim_time_unit_string
  end interface
end module kim_unit_system_module
