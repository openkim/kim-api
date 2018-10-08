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
    operator (.eq.), &
    operator (.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_energy_units, &
    kim_get_energy_unit


  type, bind(c) :: kim_energy_unit_type
    integer(c_int) energy_unit_id
  end type kim_energy_unit_type

  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_unsued") &
    :: KIM_ENERGY_UNIT_UNUSED
  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_amu_A2_per_ps2") &
    :: KIM_ENERGY_UNIT_AMU_A2_PER_PS2
  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_erg") &
    :: KIM_ENERGY_UNIT_ERG
  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_eV") &
    :: KIM_ENERGY_UNIT_EV
  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_Hartree") &
    :: KIM_ENERGY_UNIT_HARTREE
  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_J") &
    :: KIM_ENERGY_UNIT_J
  type(kim_energy_unit_type), protected, &
    bind(c, name="KIM_ENERGY_UNIT_kcal_mol") &
    :: KIM_ENERGY_UNIT_KCAL_MOL

  interface operator (.eq.)
    module procedure kim_energy_unit_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    module procedure kim_energy_unit_not_equal
  end interface operator (.ne.)

  interface kim_from_string
    module procedure kim_energy_unit_from_string
  end interface kim_from_string

  interface kim_to_string
    module procedure kim_energy_unit_to_string
  end interface kim_to_string

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

  subroutine kim_energy_unit_from_string(string, energy_unit)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      type(kim_energy_unit_type) function from_string(string) &
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

  subroutine kim_energy_unit_to_string(energy_unit, string)
    use, intrinsic :: iso_c_binding
    use kim_convert_string_module, only : kim_convert_string
    implicit none
    interface
      type(c_ptr) function get_string(energy_unit) &
        bind(c, name="KIM_EnergyUnit_String")
        use, intrinsic :: iso_c_binding
        import kim_energy_unit_type
        implicit none
        type(kim_energy_unit_type), intent(in), value :: energy_unit
      end function get_string
    end interface
    type(kim_energy_unit_type), intent(in), value :: energy_unit
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(energy_unit)
    if (c_associated(p)) then
      call kim_convert_string(p, string)
    else
      string = ""
    end if
  end subroutine kim_energy_unit_to_string

  subroutine kim_get_number_of_energy_units(number_of_energy_units)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      subroutine get_number_of_energy_units(number_of_energy_units) &
        bind(c, name="KIM_ENERGY_UNIT_GetNumberOfEnergyUnits")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_energy_units
      end subroutine get_number_of_energy_units
    end interface
    integer(c_int), intent(out) :: number_of_energy_units

    call get_number_of_energy_units(number_of_energy_units)
  end subroutine kim_get_number_of_energy_units

  subroutine kim_get_energy_unit(index, energy_unit, ierr)
    use, intrinsic :: iso_c_binding
    implicit none
    interface
      integer(c_int) function get_energy_unit(index, energy_unit) &
        bind(c, name="KIM_ENERGY_UNIT_GetEnergyUnit")
        use, intrinsic :: iso_c_binding
        import kim_energy_unit_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_energy_unit_type), intent(out) :: energy_unit
      end function get_energy_unit
    end interface
    integer(c_int), intent(in), value :: index
    type(kim_energy_unit_type), intent(out) :: energy_unit
    integer(c_int), intent(out) :: ierr

    ierr = get_energy_unit(index-1, energy_unit)
  end subroutine kim_get_energy_unit
end module kim_energy_unit_module
