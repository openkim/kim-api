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
! Release: This file is part of the kim-api-2.1.0 package.
!


!> \brief \copybrief KIM::EnergyUnit
!!
!! \sa KIM::EnergyUnit, KIM_EnergyUnit
!!
!! \since 2.0
module kim_energy_unit_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_energy_unit_type, &

    ! Constants
    KIM_ENERGY_UNIT_UNUSED, &
    KIM_ENERGY_UNIT_AMU_A2_PER_PS2, &
    KIM_ENERGY_UNIT_ERG, &
    KIM_ENERGY_UNIT_EV, &
    KIM_ENERGY_UNIT_HARTREE, &
    KIM_ENERGY_UNIT_J, &
    KIM_ENERGY_UNIT_KCAL_MOL, &

    ! Routines
    kim_known, &
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_energy_units, &
    kim_get_energy_unit


  !> \brief \copybrief KIM::EnergyUnit
  !!
  !! \sa KIM::EnergyUnit, KIM_EnergyUnit
  !!
  !! \since 2.0
  type, bind(c) :: kim_energy_unit_type
     !> \brief \copybrief KIM::EnergyUnit::energyUnitID
     !!
     !! \sa KIM::EnergyUnit::energyUnitID, KIM_EnergyUnit::energyUnitID
     !!
     !! \since 2.0
    integer(c_int) energy_unit_id
  end type kim_energy_unit_type

  !> \brief \copybrief KIM::ENERGY_UNIT::unused
  !!
  !! \sa KIM::ENERGY_UNIT::unused, KIM_ENERGY_UNIT_unused
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_unused") &
    :: KIM_ENERGY_UNIT_UNUSED

  !> \brief \copybrief KIM::ENERGY_UNIT::amu_A2_per_ps2
  !!
  !! \sa KIM::ENERGY_UNIT::amu_A2_per_ps2, KIM_ENERGY_UNIT_amu_A2_per_ps2
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_amu_A2_per_ps2") &
    :: KIM_ENERGY_UNIT_AMU_A2_PER_PS2

  !> \brief \copybrief KIM::ENERGY_UNIT::erg
  !!
  !! \sa KIM::ENERGY_UNIT::erg, KIM_ENERGY_UNIT_erg
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_erg") &
    :: KIM_ENERGY_UNIT_ERG

  !> \brief \copybrief KIM::ENERGY_UNIT::eV
  !!
  !! \sa KIM::ENERGY_UNIT::eV, KIM_ENERGY_UNIT_eV
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_eV") &
    :: KIM_ENERGY_UNIT_EV

  !> \brief \copybrief KIM::ENERGY_UNIT::Hartree
  !!
  !! \sa KIM::ENERGY_UNIT::Hartree, KIM_ENERGY_UNIT_Hartree
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_Hartree") &
    :: KIM_ENERGY_UNIT_HARTREE

  !> \brief \copybrief KIM::ENERGY_UNIT::J
  !!
  !! \sa KIM::ENERGY_UNIT::J, KIM_ENERGY_UNIT_J
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_J") &
    :: KIM_ENERGY_UNIT_J

  !> \brief \copybrief KIM::ENERGY_UNIT::kcal_mol
  !!
  !! \sa KIM::ENERGY_UNIT::kcal_mol, KIM_ENERGY_UNIT_kcal_mol
  !!
  !! \since 2.0
  type(kim_energy_unit_type), protected, save, &
    bind(c, name="KIM_ENERGY_UNIT_kcal_mol") &
    :: KIM_ENERGY_UNIT_KCAL_MOL

  !> \brief \copybrief KIM::EnergyUnit::Known
  !!
  !! \sa KIM::EnergyUnit::Known, KIM_EnergyUnit_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_energy_unit_known
  end interface kim_known

  !> \brief \copybrief KIM::EnergyUnit::operator==()
  !!
  !! \sa KIM::EnergyUnit::operator==(), KIM_EnergyUnit_Equal
  !!
  !! \since 2.0
  interface operator (.eq.)
    module procedure kim_energy_unit_equal
  end interface operator (.eq.)

  !> \brief \copybrief KIM::EnergyUnit::operator!=()
  !!
  !! \sa KIM::EnergyUnit::operator!=(), KIM_EnergyUnit_NotEqual
  !!
  !! \since 2.0
  interface operator (.ne.)
    module procedure kim_energy_unit_not_equal
  end interface operator (.ne.)

  !> \brief \copybrief KIM::EnergyUnit::EnergyUnit(std::string const &)
  !!
  !! \sa KIM::EnergyUnit::EnergyUnit(std::string const &),
  !! KIM_EnergyUnit_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_energy_unit_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::EnergyUnit::ToString
  !!
  !! \sa KIM::EnergyUnit::ToString, KIM_EnergyUnit_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_energy_unit_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::EnergyUnit::Known
  !!
  !! \sa KIM::EnergyUnit::Known, KIM_EnergyUnit_Known
  !!
  !! \since 2.0
  logical recursive function kim_energy_unit_known(energy_unit)
    implicit none
    interface
      integer(c_int) recursive function known(energy_unit) &
        bind(c, name="KIM_EnergyUnit_Known")
        use, intrinsic :: iso_c_binding
        import kim_energy_unit_type
        implicit none
        type(kim_energy_unit_type), intent(in), value :: energy_unit
      end function known
    end interface
    type(kim_energy_unit_type), intent(in) :: energy_unit

    kim_energy_unit_known = (known(energy_unit) /= 0)
  end function kim_energy_unit_known

  !> \brief \copybrief KIM::EnergyUnit::operator==()
  !!
  !! \sa KIM::EnergyUnit::operator==(), KIM_EnergyUnit_Equal
  !!
  !! \since 2.0
  logical recursive function kim_energy_unit_equal(lhs, rhs)
    implicit none
    type(kim_energy_unit_type), intent(in) :: lhs
    type(kim_energy_unit_type), intent(in) :: rhs

    kim_energy_unit_equal &
      = (lhs%energy_unit_id .eq. rhs%energy_unit_id)
  end function kim_energy_unit_equal

  !> \brief \copybrief KIM::EnergyUnit::operator!=()
  !!
  !! \sa KIM::EnergyUnit::operator!=(), KIM_EnergyUnit_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_energy_unit_not_equal(lhs, rhs)
    implicit none
    type(kim_energy_unit_type), intent(in) :: lhs
    type(kim_energy_unit_type), intent(in) :: rhs

    kim_energy_unit_not_equal = .not. (lhs .eq. rhs)
  end function kim_energy_unit_not_equal

  !> \brief \copybrief KIM::EnergyUnit::EnergyUnit(std::string const &)
  !!
  !! \sa KIM::EnergyUnit::EnergyUnit(std::string const &),
  !! KIM_EnergyUnit_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_energy_unit_from_string(string, energy_unit)
    implicit none
    interface
      type(kim_energy_unit_type) recursive function from_string(string) &
        bind(c, name="KIM_EnergyUnit_FromString")
        use, intrinsic :: iso_c_binding
        import kim_energy_unit_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_energy_unit_type), intent(out) :: energy_unit

    energy_unit = from_string(trim(string)//c_null_char)
  end subroutine kim_energy_unit_from_string

  !> \brief \copybrief KIM::EnergyUnit::ToString
  !!
  !! \sa KIM::EnergyUnit::ToString, KIM_EnergyUnit_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_energy_unit_to_string(energy_unit, string)
    use kim_convert_string_module, only : kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(energy_unit) &
        bind(c, name="KIM_EnergyUnit_ToString")
        use, intrinsic :: iso_c_binding
        import kim_energy_unit_type
        implicit none
        type(kim_energy_unit_type), intent(in), value :: energy_unit
      end function get_string
    end interface
    type(kim_energy_unit_type), intent(in) :: energy_unit
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(energy_unit)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_energy_unit_to_string

  !> \brief \copybrief KIM::ENERGY_UNIT::GetNumberOfEnergyUnits
  !!
  !! \sa KIM::ENERGY_UNIT::GetNumberOfEnergyUnits,
  !! KIM_ENERGY_UNIT_GetNumberOfEnergyUnits
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_energy_units(number_of_energy_units)
    implicit none
    interface
      recursive subroutine get_number_of_energy_units(number_of_energy_units) &
        bind(c, name="KIM_ENERGY_UNIT_GetNumberOfEnergyUnits")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_energy_units
      end subroutine get_number_of_energy_units
    end interface
    integer(c_int), intent(out) :: number_of_energy_units

    call get_number_of_energy_units(number_of_energy_units)
  end subroutine kim_get_number_of_energy_units

  !> \brief \copybrief KIM::ENERGY_UNIT::GetEnergyUnit
  !!
  !! \sa KIM::ENERGY_UNIT::GetEnergyUnit, KIM_ENERGY_UNIT_GetEnergyUnit
  !!
  !! \since 2.0
  recursive subroutine kim_get_energy_unit(index, energy_unit, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_energy_unit(index, energy_unit) &
        bind(c, name="KIM_ENERGY_UNIT_GetEnergyUnit")
        use, intrinsic :: iso_c_binding
        import kim_energy_unit_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_energy_unit_type), intent(out) :: energy_unit
      end function get_energy_unit
    end interface
    integer(c_int), intent(in) :: index
    type(kim_energy_unit_type), intent(out) :: energy_unit
    integer(c_int), intent(out) :: ierr

    ierr = get_energy_unit(index-1, energy_unit)
  end subroutine kim_get_energy_unit
end module kim_energy_unit_module
