!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2021, Regents of the University of Minnesota.
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

!> \brief \copybrief KIM::ModelDriverCreate
!!
!! \sa KIM::ModelDriverCreate, KIM_ModelDriverCreate
!!
!! \since 2.0
module kim_model_driver_create_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_driver_create_handle_type, &
    ! Constants
    KIM_MODEL_DRIVER_CREATE_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
    kim_get_parameter_file_directory_name, &
    kim_get_number_of_parameter_files, &
    kim_get_parameter_file_name, &
    kim_get_parameter_file_basename, &
    kim_set_model_numbering, &
    kim_set_influence_distance_pointer, &
    kim_set_neighbor_list_pointers, &
    kim_set_routine_pointer, &
    kim_set_species_code, &
    kim_set_parameter_pointer, &
    kim_set_model_buffer_pointer, &
    kim_set_units, &
    kim_convert_unit, &
    kim_log_entry, &
    kim_to_string

  !> \brief \copybrief KIM::ModelDriverCreate
  !!
  !! \sa KIM::ModelDriverCreate, KIM_ModelDriverCreate
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_driver_create_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_driver_create_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_driver_create_handle_type), protected, save &
    :: KIM_MODEL_DRIVER_CREATE_NULL_HANDLE

  !> \brief Compares kim_model_driver_create_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_model_driver_create_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_model_driver_create_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_model_driver_create_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ModelDriverCreate::GetParameterFileDirectoryName
  !!
  !! \sa KIM::ModelDriverCreate::GetParameterFileDirectoryName,
  !! KIM_ModelDriverCreate_GetParameterFileDirectoryName
  !!
  !! \since 2.2
  interface kim_get_parameter_file_directory_name
    module procedure kim_model_driver_create_get_parameter_file_directory_name
  end interface kim_get_parameter_file_directory_name

  !> \brief \copybrief KIM::ModelDriverCreate::GetNumberOfParameterFiles
  !!
  !! \sa KIM::ModelDriverCreate::GetNumberOfParameterFiles,
  !! KIM_ModelDriverCreate_GetNumberOfParameterFiles
  !!
  !! \since 2.0
  interface kim_get_number_of_parameter_files
    module procedure kim_model_driver_create_get_number_of_parameter_files
  end interface kim_get_number_of_parameter_files

  !> \brief \copybrief KIM::ModelDriverCreate::GetParameterFileName
  !!
  !! \sa KIM::ModelDriverCreate::GetParameterFileName,
  !! KIM_ModelDriverCreate_GetParameterFileName
  !!
  !! \since 2.0
  !!
  !! \deprecated As of 2.2.  Please use
  !! kim_model_driver_create_module::kim_get_parameter_file_basename() instead.
  interface kim_get_parameter_file_name
    module procedure kim_model_driver_create_get_parameter_file_name
  end interface kim_get_parameter_file_name

  !> \brief \copybrief KIM::ModelDriverCreate::GetParameterFileBasename
  !!
  !! \sa KIM::ModelDriverCreate::GetParameterFileBasename,
  !! KIM_ModelDriverCreate_GetParameterFileBasename
  !!
  !! \since 2.2
  interface kim_get_parameter_file_basename
    module procedure kim_model_driver_create_get_parameter_file_basename
  end interface kim_get_parameter_file_basename

  !> \brief \copybrief KIM::ModelDriverCreate::SetModelNumbering
  !!
  !! \sa KIM::ModelDriverCreate::SetModelNumbering,
  !! KIM_ModelDriverCreate_SetModelNumbering
  !!
  !! \since 2.0
  interface kim_set_model_numbering
    module procedure kim_model_driver_create_set_model_numbering
  end interface kim_set_model_numbering

  !> \brief \copybrief KIM::ModelDriverCreate::SetInfluenceDistancePointer
  !!
  !! \sa KIM::ModelDriverCreate::SetInfluenceDistancePointer,
  !! KIM_ModelDriverCreate_SetInfluenceDistancePointer
  !!
  !! \since 2.0
  interface kim_set_influence_distance_pointer
    module procedure kim_model_driver_create_set_influence_distance_pointer
  end interface kim_set_influence_distance_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetNeighborListPointers
  !!
  !! \sa KIM::ModelDriverCreate::SetNeighborListPointers,
  !! KIM_ModelDriverCreate_SetNeighborListPointers
  !!
  !! \since 2.0
  interface kim_set_neighbor_list_pointers
    module procedure kim_model_driver_create_set_neighbor_list_pointers
  end interface kim_set_neighbor_list_pointers

  !> \brief \copybrief KIM::ModelDriverCreate::SetRoutinePointer
  !!
  !! \sa KIM::ModelDriverCreate::SetRoutinePointer,
  !! KIM_ModelDriverCreate_SetRoutinePointer
  !!
  !! \since 2.0
  interface kim_set_routine_pointer
    module procedure kim_model_driver_create_set_routine_pointer
  end interface kim_set_routine_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetSpeciesCode
  !!
  !! \sa KIM::ModelDriverCreate::SetSpeciesCode,
  !! KIM_ModelDriverCreate_SetSpeciesCode
  !!
  !! \since 2.0
  interface kim_set_species_code
    module procedure kim_model_driver_create_set_species_code
  end interface kim_set_species_code

  !> \brief \copybrief KIM::ModelDriverCreate::SetParameterPointer
  !!
  !! \sa KIM::ModelDriverCreate::SetParameterPointer,
  !! KIM_ModelDriverCreate_SetParameterPointerInteger,
  !! KIM_ModelDriverCreate_SetParameterPointerDouble
  !!
  !! \since 2.0
  interface kim_set_parameter_pointer
    module procedure kim_model_driver_create_set_parameter_pointer_integer
    module procedure kim_model_driver_create_set_parameter_pointer_double
  end interface kim_set_parameter_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetModelBufferPointer
  !!
  !! \sa KIM::ModelDriverCreate::SetModelBufferPointer,
  !! KIM_ModelDriverCreate_SetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_set_model_buffer_pointer
    module procedure kim_model_driver_create_set_model_buffer_pointer
  end interface kim_set_model_buffer_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetUnits
  !!
  !! \sa KIM::ModelDriverCreate::SetUnits, KIM_ModelDriverCreate_SetUnits
  !!
  !! \since 2.0
  interface kim_set_units
    module procedure kim_model_driver_create_set_units
  end interface kim_set_units

  !> \brief \copybrief KIM::ModelDriverCreate::ConvertUnit
  !!
  !! \sa KIM::ModelDriverCreate::ConvertUnit, KIM_ModelDriverCreate_ConvertUnit
  !!
  !! \since 2.0
  interface kim_convert_unit
    module procedure kim_model_driver_create_convert_unit
  end interface kim_convert_unit

  !> \brief \copybrief KIM::ModelDriverCreate::LogEntry
  !!
  !! \sa KIM::ModelDriverCreate::LogEntry, KIM_ModelDriverCreate_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_model_driver_create_log_entry
  end interface kim_log_entry

  !> \brief \copybrief KIM::ModelDriverCreate::ToString
  !!
  !! \sa KIM::ModelDriverCreate::ToString, KIM_ModelDriverCreate_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_driver_create_to_string
  end interface kim_to_string

contains
  !> \brief Compares kim_model_driver_create_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_driver_create_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_driver_create_handle_type), intent(in) :: lhs
    type(kim_model_driver_create_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_driver_create_handle_equal = .true.
    else
      kim_model_driver_create_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_driver_create_handle_equal

  !> \brief Compares kim_model_driver_create_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_driver_create_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_driver_create_handle_type), intent(in) :: lhs
    type(kim_model_driver_create_handle_type), intent(in) :: rhs

    kim_model_driver_create_handle_not_equal = .not. (lhs == rhs)
  end function kim_model_driver_create_handle_not_equal

  !> \brief \copybrief KIM::ModelDriverCreate::GetParameterFileDirectoryName
  !!
  !! \sa KIM::ModelDriverCreate::GetParameterFileDirectoryName,
  !! KIM_ModelDriverCreate_GetParameterFileDirectoryName
  !!
  !! \since 2.2
  recursive subroutine &
    kim_model_driver_create_get_parameter_file_directory_name( &
    model_driver_create_handle, directory_name)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    implicit none
    interface
      recursive subroutine get_parameter_file_directory_name( &
        model_driver_create, directory_name) &
        bind(c, name="KIM_ModelDriverCreate_GetParameterFileDirectoryName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) :: model_driver_create
        type(c_ptr), intent(out) :: directory_name
      end subroutine get_parameter_file_directory_name
    end interface
    type(kim_model_driver_create_handle_type), intent(in) :: &
      model_driver_create_handle
    character(len=*, kind=c_char), intent(out) :: directory_name
    type(kim_model_driver_create_type), pointer :: model_driver_create

    type(c_ptr) pdirectory_name

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call get_parameter_file_directory_name(model_driver_create, pdirectory_name)
    call kim_convert_c_char_ptr_to_string(pdirectory_name, directory_name)
  end subroutine kim_model_driver_create_get_parameter_file_directory_name

  !> \brief \copybrief KIM::ModelDriverCreate::GetNumberOfParameterFiles
  !!
  !! \sa KIM::ModelDriverCreate::GetNumberOfParameterFiles,
  !! KIM_ModelDriverCreate_GetNumberOfParameterFiles
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_get_number_of_parameter_files( &
    model_driver_create_handle, number_of_parameter_files)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      recursive subroutine get_number_of_parameter_files( &
        model_driver_create, number_of_parameter_files) &
        bind(c, name="KIM_ModelDriverCreate_GetNumberOfParameterFiles")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        integer(c_int), intent(out) :: number_of_parameter_files
      end subroutine get_number_of_parameter_files
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    integer(c_int), intent(out) :: number_of_parameter_files
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call get_number_of_parameter_files(model_driver_create, &
                                       number_of_parameter_files)
  end subroutine kim_model_driver_create_get_number_of_parameter_files

  !> \brief \copybrief KIM::ModelDriverCreate::GetParameterFileName
  !!
  !! \sa KIM::ModelDriverCreate::GetParameterFileName,
  !! KIM_ModelDriverCreate_GetParameterFileName
  !!
  !! \since 2.0
  !!
  !! \deprecated As of 2.2.  Please use
  !! kim_model_driver_create_module::kim_get_parameter_file_basename() instead.
  recursive subroutine kim_model_driver_create_get_parameter_file_name( &
    model_driver_create_handle, index, parameter_file_name, ierr)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      integer(c_int) recursive function get_parameter_file_name( &
        model_driver_create, index, parameter_file_name) &
        bind(c, name="KIM_ModelDriverCreate_GetParameterFileName")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: parameter_file_name
      end function get_parameter_file_name
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: parameter_file_name
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    type(c_ptr) :: p

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    ierr = get_parameter_file_name(model_driver_create, &
                                   index - 1, p)
    call kim_convert_c_char_ptr_to_string(p, parameter_file_name)
  end subroutine kim_model_driver_create_get_parameter_file_name

  !> \brief \copybrief KIM::ModelDriverCreate::GetParameterFileBasename
  !!
  !! \sa KIM::ModelDriverCreate::GetParameterFileBasename,
  !! KIM_ModelDriverCreate_GetParameterFileBasename
  !!
  !! \since 2.2
  recursive subroutine kim_model_driver_create_get_parameter_file_basename( &
    model_driver_create_handle, index, parameter_file_basename, ierr)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      integer(c_int) recursive function get_parameter_file_basename( &
        model_driver_create, index, parameter_file_basename) &
        bind(c, name="KIM_ModelDriverCreate_GetParameterFileBasename")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        integer(c_int), intent(in), value :: index
        type(c_ptr), intent(out) :: parameter_file_basename
      end function get_parameter_file_basename
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    integer(c_int), intent(in) :: index
    character(len=*, kind=c_char), intent(out) :: parameter_file_basename
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    type(c_ptr) :: p

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    ierr = get_parameter_file_basename(model_driver_create, &
                                       index - 1, p)
    call kim_convert_c_char_ptr_to_string(p, parameter_file_basename)
  end subroutine kim_model_driver_create_get_parameter_file_basename

  !> \brief \copybrief KIM::ModelDriverCreate::SetModelNumbering
  !!
  !! \sa KIM::ModelDriverCreate::SetModelNumbering,
  !! KIM_ModelDriverCreate_SetModelNumbering
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_model_numbering( &
    model_driver_create_handle, numbering, ierr)
    use kim_numbering_module, only: kim_numbering_type
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      integer(c_int) recursive function set_model_numbering( &
        model_driver_create, numbering) &
        bind(c, name="KIM_ModelDriverCreate_SetModelNumbering")
        use, intrinsic :: iso_c_binding
        use kim_numbering_module, only: kim_numbering_type
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(kim_numbering_type), intent(in), value :: numbering
      end function set_model_numbering
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    type(kim_numbering_type), intent(in) :: numbering
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    ierr = set_model_numbering(model_driver_create, numbering)
  end subroutine kim_model_driver_create_set_model_numbering

  !> \brief \copybrief KIM::ModelDriverCreate::SetInfluenceDistancePointer
  !!
  !! \sa KIM::ModelDriverCreate::SetInfluenceDistancePointer,
  !! KIM_ModelDriverCreate_SetInfluenceDistancePointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_influence_distance_pointer( &
    model_driver_create_handle, influence_distance)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      recursive subroutine set_influence_distance_pointer(model_driver_create, &
                                                          influence_distance) &
        bind(c, name="KIM_ModelDriverCreate_SetInfluenceDistancePointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(c_ptr), intent(in), value :: influence_distance
      end subroutine set_influence_distance_pointer
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    real(c_double), intent(in), target :: influence_distance
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call set_influence_distance_pointer(model_driver_create, &
                                        c_loc(influence_distance))
  end subroutine kim_model_driver_create_set_influence_distance_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetNeighborListPointers
  !!
  !! \sa KIM::ModelDriverCreate::SetNeighborListPointers,
  !! KIM_ModelDriverCreate_SetNeighborListPointers
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_neighbor_list_pointers( &
    model_driver_create_handle, number_of_neighbor_lists, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      recursive subroutine set_neighbor_list_pointers( &
        model_driver_create, number_of_neighbor_lists, cutoffs_ptr, &
        model_will_not_request_neighbors_of_noncontributing_particles) &
        bind(c, name="KIM_ModelDriverCreate_SetNeighborListPointers")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        integer(c_int), intent(in), value :: number_of_neighbor_lists
        type(c_ptr), intent(in), value :: cutoffs_ptr
        type(c_ptr), intent(in), value :: &
          model_will_not_request_neighbors_of_noncontributing_particles
      end subroutine set_neighbor_list_pointers
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    integer(c_int), intent(in) :: number_of_neighbor_lists
    real(c_double), intent(in), target :: cutoffs(number_of_neighbor_lists)
    integer(c_int), intent(in), target :: &
      model_will_not_request_neighbors_of_noncontributing_particles( &
      number_of_neighbor_lists)

    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call set_neighbor_list_pointers( &
      model_driver_create, number_of_neighbor_lists, c_loc(cutoffs), &
      c_loc(model_will_not_request_neighbors_of_noncontributing_particles))
  end subroutine kim_model_driver_create_set_neighbor_list_pointers

  !> \brief \copybrief KIM::ModelDriverCreate::SetRoutinePointer
  !!
  !! \sa KIM::ModelDriverCreate::SetRoutinePointer,
  !! KIM_ModelDriverCreate_SetRoutinePointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_routine_pointer( &
    model_driver_create_handle, model_routine_name, language_name, required, &
    fptr, ierr)
    use kim_model_routine_name_module, only: kim_model_routine_name_type
    use kim_language_name_module, only: kim_language_name_type
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      integer(c_int) recursive function set_routine_pointer( &
        model_driver_create, model_routine_name, language_name, required, &
        fptr) bind(c, name="KIM_ModelDriverCreate_SetRoutinePointer")
        use, intrinsic :: iso_c_binding
        use kim_model_routine_name_module, only: kim_model_routine_name_type
        use kim_language_name_module, only: kim_language_name_type
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(kim_model_routine_name_type), intent(in), value &
          :: model_routine_name
        type(kim_language_name_type), intent(in), value :: language_name
        integer(c_int), intent(in), value :: required
        type(c_funptr), intent(in), value :: fptr
      end function set_routine_pointer
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    type(kim_model_routine_name_type), intent(in) :: model_routine_name
    type(kim_language_name_type), intent(in) :: language_name
    integer(c_int), intent(in) :: required
    type(c_funptr), intent(in), value :: fptr  ! must be left as "value"!?!
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    ierr = set_routine_pointer(model_driver_create, model_routine_name, &
                               language_name, required, fptr)
  end subroutine kim_model_driver_create_set_routine_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetSpeciesCode
  !!
  !! \sa KIM::ModelDriverCreate::SetSpeciesCode,
  !! KIM_ModelDriverCreate_SetSpeciesCode
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_species_code( &
    model_driver_create_handle, species_name, code, ierr)
    use kim_species_name_module, only: kim_species_name_type
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      integer(c_int) recursive function set_species_code(model_driver_create, &
                                                         species_name, code) &
        bind(c, name="KIM_ModelDriverCreate_SetSpeciesCode")
        use, intrinsic :: iso_c_binding
        use kim_species_name_module, only: kim_species_name_type
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(kim_species_name_type), intent(in), value :: species_name
        integer(c_int), intent(in), value :: code
      end function set_species_code
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    type(kim_species_name_type), intent(in) :: species_name
    integer(c_int), intent(in) :: code
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    ierr = set_species_code(model_driver_create, species_name, code)
  end subroutine kim_model_driver_create_set_species_code

  !> \brief \copybrief KIM::ModelDriverCreate::SetParameterPointer
  !!
  !! \sa KIM::ModelDriverCreate::SetParameterPointer,
  !! KIM_ModelDriverCreate_SetParameterPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_parameter_pointer_integer( &
    model_driver_create_handle, int1, name, description, ierr)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    integer(c_int), intent(in), target :: int1(:)
    character(len=*, kind=c_char), intent(in) :: name
    character(len=*, kind=c_char), intent(in) :: description
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call set_parameter(model_driver_create, size(int1, 1, c_int), int1, &
                       name, description, ierr)
    return

  contains
    recursive subroutine set_parameter(model_driver_create, extent, int1, &
                                       name, description, ierr)
      use kim_interoperable_types_module, only: kim_model_driver_create_type
      implicit none
      interface
        integer(c_int) recursive function set_parameter_pointer_integer( &
          model_driver_create, extent, ptr, name, description) &
          bind(c, name="KIM_ModelDriverCreate_SetParameterPointerInteger")
          use, intrinsic :: iso_c_binding
          use kim_interoperable_types_module, only: &
            kim_model_driver_create_type
          implicit none
          type(kim_model_driver_create_type), intent(in) &
            :: model_driver_create
          integer(c_int), intent(in), value :: extent
          type(c_ptr), intent(in), value :: ptr
          character(c_char), intent(in) :: name(*)
          character(c_char), intent(in) :: description(*)
        end function set_parameter_pointer_integer
      end interface
      type(kim_model_driver_create_type), intent(in) &
        :: model_driver_create
      integer(c_int), intent(in) :: extent
      integer(c_int), intent(in), target :: int1(extent)
      character(len=*, kind=c_char), intent(in) :: name
      character(len=*, kind=c_char), intent(in) :: description
      integer(c_int), intent(out) :: ierr

      ierr = set_parameter_pointer_integer(model_driver_create, &
                                           extent, &
                                           c_loc(int1), &
                                           trim(name)//c_null_char, &
                                           trim(description)//c_null_char)
    end subroutine set_parameter
  end subroutine kim_model_driver_create_set_parameter_pointer_integer

  !> \brief \copybrief KIM::ModelDriverCreate::SetParameterPointer
  !!
  !! \sa KIM::ModelDriverCreate::SetParameterPointer,
  !! KIM_ModelDriverCreate_SetParameterPointerDouble
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_parameter_pointer_double( &
    model_driver_create_handle, double1, name, description, ierr)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    real(c_double), intent(in), target :: double1(:)
    character(len=*, kind=c_char), intent(in) :: name
    character(len=*, kind=c_char), intent(in) :: description
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call set_parameter(model_driver_create, size(double1, 1, c_int), &
                       double1, name, description, ierr)
    return

  contains
    recursive subroutine set_parameter(model_driver_create, extent, double1, &
                                       name, description, ierr)
      use kim_interoperable_types_module, only: kim_model_driver_create_type
      implicit none
      interface
        integer(c_int) recursive function set_parameter_pointer_double( &
          model_driver_create, extent, ptr, name, description) &
          bind(c, name="KIM_ModelDriverCreate_SetParameterPointerDouble")
          use, intrinsic :: iso_c_binding
          use kim_interoperable_types_module, only: &
            kim_model_driver_create_type
          implicit none
          type(kim_model_driver_create_type), intent(in) &
            :: model_driver_create
          integer(c_int), intent(in), value :: extent
          type(c_ptr), intent(in), value :: ptr
          character(c_char), intent(in) :: name(*)
          character(c_char), intent(in) :: description(*)
        end function set_parameter_pointer_double
      end interface
      type(kim_model_driver_create_type), intent(in) &
        :: model_driver_create
      integer(c_int), intent(in) :: extent
      real(c_double), intent(in), target :: double1(extent)
      character(len=*, kind=c_char), intent(in) :: name
      character(len=*, kind=c_char), intent(in) :: description
      integer(c_int), intent(out) :: ierr

      ierr = set_parameter_pointer_double(model_driver_create, &
                                          extent, &
                                          c_loc(double1), &
                                          trim(name)//c_null_char, &
                                          trim(description)//c_null_char)
    end subroutine set_parameter
  end subroutine kim_model_driver_create_set_parameter_pointer_double

  !> \brief \copybrief KIM::ModelDriverCreate::SetModelBufferPointer
  !!
  !! \sa KIM::ModelDriverCreate::SetModelBufferPointer,
  !! KIM_ModelDriverCreate_SetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_model_buffer_pointer( &
    model_driver_create_handle, ptr)
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      recursive subroutine set_model_buffer_pointer(model_driver_create, ptr) &
        bind(c, name="KIM_ModelDriverCreate_SetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_model_buffer_pointer
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call set_model_buffer_pointer(model_driver_create, ptr)
  end subroutine kim_model_driver_create_set_model_buffer_pointer

  !> \brief \copybrief KIM::ModelDriverCreate::SetUnits
  !!
  !! \sa KIM::ModelDriverCreate::SetUnits, KIM_ModelDriverCreate_SetUnits
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_set_units( &
    model_driver_create_handle, length_unit, energy_unit, charge_unit, &
    temperature_unit, time_unit, ierr)
    use kim_unit_system_module, only: kim_length_unit_type, &
                                      kim_energy_unit_type, &
                                      kim_charge_unit_type, &
                                      kim_temperature_unit_type, &
                                      kim_time_unit_type
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      integer(c_int) recursive function set_units( &
        model_driver_create, length_unit, energy_unit, charge_unit, &
        temperature_unit, time_unit) &
        bind(c, name="KIM_ModelDriverCreate_SetUnits")
        use, intrinsic :: iso_c_binding
        use kim_unit_system_module, only: kim_length_unit_type, &
                                          kim_energy_unit_type, &
                                          kim_charge_unit_type, &
                                          kim_temperature_unit_type, &
                                          kim_time_unit_type
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(kim_length_unit_type), intent(in), value :: length_unit
        type(kim_energy_unit_type), intent(in), value :: energy_unit
        type(kim_charge_unit_type), intent(in), value :: charge_unit
        type(kim_temperature_unit_type), intent(in), value :: temperature_unit
        type(kim_time_unit_type), intent(in), value :: time_unit
      end function set_units
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    type(kim_length_unit_type), intent(in) :: length_unit
    type(kim_energy_unit_type), intent(in) :: energy_unit
    type(kim_charge_unit_type), intent(in) :: charge_unit
    type(kim_temperature_unit_type), intent(in) :: temperature_unit
    type(kim_time_unit_type), intent(in) :: time_unit
    integer(c_int), intent(out) :: ierr
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    ierr = set_units(model_driver_create, length_unit, energy_unit, &
                     charge_unit, temperature_unit, time_unit)
  end subroutine kim_model_driver_create_set_units

  !> \brief \copybrief KIM::ModelDriverCreate::ConvertUnit
  !!
  !! \sa KIM::ModelDriverCreate::ConvertUnit, KIM_ModelDriverCreate_ConvertUnit
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_convert_unit( &
    from_length_unit, from_energy_unit, &
    from_charge_unit, from_temperature_unit, from_time_unit, &
    to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
    to_time_unit, length_exponent, energy_exponent, charge_exponent, &
    temperature_exponent, time_exponent, conversion_factor, ierr)
    use kim_unit_system_module, only: kim_length_unit_type
    use kim_unit_system_module, only: kim_energy_unit_type
    use kim_unit_system_module, only: kim_charge_unit_type
    use kim_unit_system_module, only: kim_temperature_unit_type
    use kim_unit_system_module, only: kim_time_unit_type
    implicit none
    interface
      integer(c_int) recursive function convert_unit( &
        from_length_unit, from_energy_unit, &
        from_charge_unit, from_temperature_unit, from_time_unit, &
        to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
        to_time_unit, length_exponent, energy_exponent, charge_exponent, &
        temperature_exponent, time_exponent, conversion_factor) &
        bind(c, name="KIM_ModelDriverCreate_ConvertUnit")
        use, intrinsic :: iso_c_binding
        use kim_unit_system_module, only: kim_length_unit_type
        use kim_unit_system_module, only: kim_energy_unit_type
        use kim_unit_system_module, only: kim_charge_unit_type
        use kim_unit_system_module, only: kim_temperature_unit_type
        use kim_unit_system_module, only: kim_time_unit_type
        implicit none
        type(kim_length_unit_type), intent(in), value :: from_length_unit
        type(kim_energy_unit_type), intent(in), value :: from_energy_unit
        type(kim_charge_unit_type), intent(in), value :: from_charge_unit
        type(kim_temperature_unit_type), intent(in), value :: &
          from_temperature_unit
        type(kim_time_unit_type), intent(in), value :: from_time_unit
        type(kim_length_unit_type), intent(in), value :: to_length_unit
        type(kim_energy_unit_type), intent(in), value :: to_energy_unit
        type(kim_charge_unit_type), intent(in), value :: to_charge_unit
        type(kim_temperature_unit_type), intent(in), value :: &
          to_temperature_unit
        type(kim_time_unit_type), intent(in), value :: to_time_unit
        real(c_double), intent(in), value :: length_exponent
        real(c_double), intent(in), value :: energy_exponent
        real(c_double), intent(in), value :: charge_exponent
        real(c_double), intent(in), value :: temperature_exponent
        real(c_double), intent(in), value :: time_exponent
        real(c_double), intent(out) :: conversion_factor
      end function convert_unit
    end interface
    type(kim_length_unit_type), intent(in) :: from_length_unit
    type(kim_energy_unit_type), intent(in) :: from_energy_unit
    type(kim_charge_unit_type), intent(in) :: from_charge_unit
    type(kim_temperature_unit_type), intent(in) :: from_temperature_unit
    type(kim_time_unit_type), intent(in) :: from_time_unit
    type(kim_length_unit_type), intent(in) :: to_length_unit
    type(kim_energy_unit_type), intent(in) :: to_energy_unit
    type(kim_charge_unit_type), intent(in) :: to_charge_unit
    type(kim_temperature_unit_type), intent(in) :: to_temperature_unit
    type(kim_time_unit_type), intent(in) :: to_time_unit
    real(c_double), intent(in) :: length_exponent
    real(c_double), intent(in) :: energy_exponent
    real(c_double), intent(in) :: charge_exponent
    real(c_double), intent(in) :: temperature_exponent
    real(c_double), intent(in) :: time_exponent
    real(c_double), intent(out) :: conversion_factor
    integer(c_int), intent(out) :: ierr

    ierr = convert_unit(from_length_unit, from_energy_unit, from_charge_unit, &
                        from_temperature_unit, from_time_unit, to_length_unit, &
                        to_energy_unit, to_charge_unit, to_temperature_unit, &
                        to_time_unit, length_exponent, energy_exponent, &
                        charge_exponent, temperature_exponent, time_exponent, &
                        conversion_factor)
  end subroutine kim_model_driver_create_convert_unit

  !> \brief \copybrief KIM::ModelDriverCreate::LogEntry
  !!
  !! \sa KIM::ModelDriverCreate::LogEntry, KIM_ModelDriverCreate_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_log_entry( &
    model_driver_create_handle, log_verbosity, message)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      recursive subroutine log_entry(model_driver_create, log_verbosity, &
                                     message, line_number, file_name) &
        bind(c, name="KIM_ModelDriverCreate_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_driver_create_type), pointer :: model_driver_create

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    call log_entry(model_driver_create, log_verbosity, &
                   trim(message)//c_null_char, 0, ""//c_null_char)
  end subroutine kim_model_driver_create_log_entry

  !> \brief \copybrief KIM::ModelDriverCreate::ToString
  !!
  !! \sa KIM::ModelDriverCreate::ToString, KIM_ModelDriverCreate_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_driver_create_to_string( &
    model_driver_create_handle, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_driver_create_type
    implicit none
    interface
      type(c_ptr) recursive function model_driver_create_string( &
        model_driver_create) &
        bind(c, name="KIM_ModelDriverCreate_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_driver_create_type
        implicit none
        type(kim_model_driver_create_type), intent(in) &
          :: model_driver_create
      end function model_driver_create_string
    end interface
    type(kim_model_driver_create_handle_type), intent(in) &
      :: model_driver_create_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_driver_create_type), pointer :: model_driver_create

    type(c_ptr) :: p

    call c_f_pointer(model_driver_create_handle%p, model_driver_create)
    p = model_driver_create_string(model_driver_create)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_driver_create_to_string
end module kim_model_driver_create_module
