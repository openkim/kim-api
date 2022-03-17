!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2022, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!
! SPDX-License-Identifier: LGPL-2.1-or-later
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!

!
! Release: This file is part of the kim-api.git repository.
!

!> \brief \copybrief KIM::ComputeArgumentName
!!
!! \sa KIM::ComputeArgumentName, KIM_ComputeArgumentName
!!
!! \since 2.0
module kim_compute_argument_name_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_compute_argument_name_type, &
    ! Constants
    KIM_COMPUTE_ARGUMENT_NAME_NUMBER_OF_PARTICLES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_SPECIES_CODES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_CONTRIBUTING, &
    KIM_COMPUTE_ARGUMENT_NAME_COORDINATES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_VIRIAL, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_VIRIAL, &
    ! Routines
    kim_known, &
    operator(.eq.), &
    operator(.ne.), &
    kim_from_string, &
    kim_to_string, &
    kim_get_number_of_compute_argument_names, &
    kim_get_compute_argument_name, &
    kim_get_compute_argument_data_type

  !> \brief \copybrief KIM::ComputeArgumentName
  !!
  !! \sa KIM::ComputeArgumentName, KIM_ComputeArgumentName
  !!
  !! \since 2.0
  type, bind(c) :: kim_compute_argument_name_type
    !> \brief \copybrief KIM::ComputeArgumentName::computeArgumentNameID
    !!
    !! \sa KIM::ComputeArgumentName::computeArgumentNameID,
    !! KIM_ComputeArgumentName::computeArgumentNameID
    !!
    !! \since 2.0
    integer(c_int) compute_argument_name_id
  end type kim_compute_argument_name_type

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles,
  !! KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles") &
    :: KIM_COMPUTE_ARGUMENT_NAME_NUMBER_OF_PARTICLES

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes,
  !! KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_SPECIES_CODES

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::particleContributing
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::particleContributing,
  !! KIM_COMPUTE_ARGUMENT_NAME_particleContributing
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_particleContributing") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_CONTRIBUTING

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::coordinates
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::coordinates,
  !! KIM_COMPUTE_ARGUMENT_NAME_coordinates
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_coordinates") &
    :: KIM_COMPUTE_ARGUMENT_NAME_COORDINATES

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialEnergy
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::partialEnergy,
  !! KIM_COMPUTE_ARGUMENT_NAME_partialEnergy
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialEnergy") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialForces
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::partialForces,
  !! KIM_COMPUTE_ARGUMENT_NAME_partialForces
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialForces") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy,
  !! KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_ENERGY

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialVirial
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::partialVirial,
  !! KIM_COMPUTE_ARGUMENT_NAME_partialVirial
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialVirial") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_VIRIAL

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial,
  !! KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial
  !!
  !! \since 2.0
  type(kim_compute_argument_name_type), protected, save, &
    bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_partialParticleVirial") &
    :: KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_PARTICLE_VIRIAL

  !> \brief \copybrief KIM::ComputeArgumentName::Known
  !!
  !! \sa KIM::ComputeArgumentName::Known, KIM_ComputeArgumentName_Known
  !!
  !! \since 2.0
  interface kim_known
    module procedure kim_compute_argument_name_known
  end interface kim_known

  !> \brief \copybrief KIM::ComputeArgumentName::operator==()
  !!
  !! \sa KIM::ComputeArgumentName::operator==(), KIM_ComputeArgumentName_Equal
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_compute_argument_name_equal
  end interface operator(.eq.)

  !> \brief \copybrief KIM::ComputeArgumentName::operator!=()
  !!
  !! \sa KIM::ComputeArgumentName::operator!=(),
  !! KIM_ComputeArgumentName_NotEqual
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_compute_argument_name_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ComputeArgumentName::<!--
  !! -->ComputeArgumentName(std::string const &)
  !!
  !! \sa KIM::ComputeArgumentName::ComputeArgumentName(std::string const &),
  !! KIM_ComputeArgumentName_FromString
  !!
  !! \since 2.0
  interface kim_from_string
    module procedure kim_compute_argument_name_from_string
  end interface kim_from_string

  !> \brief \copybrief KIM::ComputeArgumentName::ToString
  !!
  !! \sa KIM::ComputeArgumentName::ToString, KIM_ComputeArgumentName_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_compute_argument_name_to_string
  end interface kim_to_string

contains
  !> \brief \copybrief KIM::ComputeArgumentName::Known
  !!
  !! \sa KIM::ComputeArgumentName::Known, KIM_ComputeArgumentName_Known
  !!
  !! \since 2.0
  logical recursive function kim_compute_argument_name_known( &
    compute_argument_name)
    implicit none
    interface
      integer(c_int) recursive function known(compute_argument_name) &
        bind(c, name="KIM_ComputeArgumentName_Known")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
      end function known
    end interface
    type(kim_compute_argument_name_type), intent(in) :: compute_argument_name

    kim_compute_argument_name_known = (known(compute_argument_name) /= 0)
  end function kim_compute_argument_name_known

  !> \brief \copybrief KIM::ComputeArgumentName::operator==()
  !!
  !! \sa KIM::ComputeArgumentName::operator==(), KIM_ComputeArgumentName_Equal
  !!
  !! \since 2.0
  logical recursive function kim_compute_argument_name_equal(lhs, rhs)
    implicit none
    type(kim_compute_argument_name_type), intent(in) :: lhs
    type(kim_compute_argument_name_type), intent(in) :: rhs

    kim_compute_argument_name_equal &
      = (lhs%compute_argument_name_id == rhs%compute_argument_name_id)
  end function kim_compute_argument_name_equal

  !> \brief \copybrief KIM::ComputeArgumentName::operator!=()
  !!
  !! \sa KIM::ComputeArgumentName::operator!=(),
  !! KIM_ComputeArgumentName_NotEqual
  !!
  !! \since 2.0
  logical recursive function kim_compute_argument_name_not_equal(lhs, rhs)
    implicit none
    type(kim_compute_argument_name_type), intent(in) :: lhs
    type(kim_compute_argument_name_type), intent(in) :: rhs

    kim_compute_argument_name_not_equal = .not. (lhs == rhs)
  end function kim_compute_argument_name_not_equal

  !> \brief \copybrief KIM::ComputeArgumentName::<!--
  !! -->ComputeArgumentName(std::string const &)
  !!
  !! \sa KIM::ComputeArgumentName::ComputeArgumentName(std::string const &),
  !! KIM_ComputeArgumentName_FromString
  !!
  !! \since 2.0
  recursive subroutine kim_compute_argument_name_from_string( &
    string, compute_argument_name)
    implicit none
    interface
      type(kim_compute_argument_name_type) recursive function from_string( &
        string) bind(c, name="KIM_ComputeArgumentName_FromString")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        character(c_char), intent(in) :: string(*)
      end function from_string
    end interface
    character(len=*, kind=c_char), intent(in) :: string
    type(kim_compute_argument_name_type), intent(out) :: compute_argument_name

    compute_argument_name = from_string(trim(string)//c_null_char)
  end subroutine kim_compute_argument_name_from_string

  !> \brief \copybrief KIM::ComputeArgumentName::ToString
  !!
  !! \sa KIM::ComputeArgumentName::ToString, KIM_ComputeArgumentName_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_compute_argument_name_to_string( &
    compute_argument_name, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      type(c_ptr) recursive function get_string(compute_argument_name) &
        bind(c, name="KIM_ComputeArgumentName_ToString")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
      end function get_string
    end interface
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    character(len=*, kind=c_char), intent(out) :: string

    type(c_ptr) :: p

    p = get_string(compute_argument_name)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_compute_argument_name_to_string

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::<!--
  !! -->GetNumberOfComputeArgumentNames
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames,
  !! KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames
  !!
  !! \since 2.0
  recursive subroutine kim_get_number_of_compute_argument_names( &
    number_of_compute_argument_names)
    implicit none
    interface
      recursive subroutine get_number_of_compute_argument_names( &
        number_of_compute_argument_names) &
        bind(c, &
             name="KIM_COMPUTE_ARGUMENT_NAME_GetNumberOfComputeArgumentNames")
        use, intrinsic :: iso_c_binding
        integer(c_int), intent(out) :: number_of_compute_argument_names
      end subroutine get_number_of_compute_argument_names
    end interface
    integer(c_int), intent(out) :: number_of_compute_argument_names

    call get_number_of_compute_argument_names(number_of_compute_argument_names)
  end subroutine kim_get_number_of_compute_argument_names

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::<!--
  !! -->GetComputeArgumentName
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName,
  !! KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName
  !!
  !! \since 2.0
  recursive subroutine kim_get_compute_argument_name( &
    index, compute_argument_name, ierr)
    implicit none
    interface
      integer(c_int) recursive function get_compute_argument_name( &
        index, compute_argument_name) &
        bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentName")
        use, intrinsic :: iso_c_binding
        import kim_compute_argument_name_type
        implicit none
        integer(c_int), intent(in), value :: index
        type(kim_compute_argument_name_type), intent(out) :: &
          compute_argument_name
      end function get_compute_argument_name
    end interface
    integer(c_int), intent(in) :: index
    type(kim_compute_argument_name_type), intent(out) :: compute_argument_name
    integer(c_int), intent(out) :: ierr

    ierr = get_compute_argument_name(index - 1, compute_argument_name)
  end subroutine kim_get_compute_argument_name

  !> \brief \copybrief KIM::COMPUTE_ARGUMENT_NAME::<!--
  !! -->GetComputeArgumentDataType
  !!
  !! \sa KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentDataType,
  !! KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType
  !!
  !! \since 2.0
  recursive subroutine kim_get_compute_argument_data_type( &
    compute_argument_name, &
    data_type, ierr)
    use kim_data_type_module, only: kim_data_type_type
    implicit none
    interface
      integer(c_int) recursive function get_compute_argument_data_type( &
        compute_argument_name, data_type) &
        bind(c, name="KIM_COMPUTE_ARGUMENT_NAME_GetComputeArgumentDataType")
        use, intrinsic :: iso_c_binding
        use kim_data_type_module, only: kim_data_type_type
        import kim_compute_argument_name_type
        implicit none
        type(kim_compute_argument_name_type), intent(in), value :: &
          compute_argument_name
        type(kim_data_type_type), intent(out) :: data_type
      end function get_compute_argument_data_type
    end interface
    type(kim_compute_argument_name_type), intent(in) :: &
      compute_argument_name
    type(kim_data_type_type), intent(out) :: data_type
    integer(c_int), intent(out) :: ierr

    ierr = get_compute_argument_data_type(compute_argument_name, data_type)
  end subroutine kim_get_compute_argument_data_type
end module kim_compute_argument_name_module
