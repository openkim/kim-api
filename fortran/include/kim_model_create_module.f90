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
! Copyright (c) 2016--2020, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!

!
! Release: This file is part of the kim-api-2.2.1 package.
!

!> \brief \copybrief KIM::ModelCreate
!!
!! \sa KIM::ModelCreate, KIM_ModelCreate
!!
!! \since 2.0
module kim_model_create_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    ! Derived types
    kim_model_create_handle_type, &
    ! Constants
    KIM_MODEL_CREATE_NULL_HANDLE, &
    ! Routines
    operator(.eq.), &
    operator(.ne.), &
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

  !> \brief \copybrief KIM::ModelCreate
  !!
  !! \sa KIM::ModelCreate, KIM_ModelCreate
  !!
  !! \since 2.0
  type, bind(c) :: kim_model_create_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_create_handle_type

  !> \brief NULL handle for use in comparisons.
  !!
  !! \since 2.0
  type(kim_model_create_handle_type), protected, save &
    :: KIM_MODEL_CREATE_NULL_HANDLE

  !> \brief Compares kim_model_create_handle_type's for equality.
  !!
  !! \since 2.0
  interface operator(.eq.)
    module procedure kim_model_create_handle_equal
  end interface operator(.eq.)

  !> \brief Compares kim_model_create_handle_type's for inequality.
  !!
  !! \since 2.0
  interface operator(.ne.)
    module procedure kim_model_create_handle_not_equal
  end interface operator(.ne.)

  !> \brief \copybrief KIM::ModelCreate::SetModelNumbering
  !!
  !! \sa KIM::ModelCreate::SetModelNumbering, KIM_ModelCreate_SetModelNumbering
  !!
  !! \since 2.0
  interface kim_set_model_numbering
    module procedure kim_model_create_set_model_numbering
  end interface kim_set_model_numbering

  !> \brief \copybrief KIM::ModelCreate::SetInfluenceDistancePointer
  !!
  !! \sa KIM::ModelCreate::SetInfluenceDistancePointer,
  !! KIM_ModelCreate_SetInfluenceDistancePointer
  !!
  !! \since 2.0
  interface kim_set_influence_distance_pointer
    module procedure kim_model_create_set_influence_distance_pointer
  end interface kim_set_influence_distance_pointer

  !> \brief \copybrief KIM::ModelCreate::SetNeighborListPointers
  !!
  !! \sa KIM::ModelCreate::SetNeighborListPointers,
  !! KIM_ModelCreate_SetNeighborListPointers
  !!
  !! \since 2.0
  interface kim_set_neighbor_list_pointers
    module procedure kim_model_create_set_neighbor_list_pointers
  end interface kim_set_neighbor_list_pointers

  !> \brief \copybrief KIM::ModelCreate::SetRoutinePointer
  !!
  !! \sa KIM::ModelCreate::SetRoutinePointer, KIM_ModelCreate_SetRoutinePointer
  !!
  !! \since 2.0
  interface kim_set_routine_pointer
    module procedure kim_model_create_set_routine_pointer
  end interface kim_set_routine_pointer

  !> \brief \copybrief KIM::ModelCreate::SetSpeciesCode
  !!
  !! \sa KIM::ModelCreate::SetSpeciesCode, KIM_ModelCreate_SetSpeciesCode
  !!
  !! \since 2.0
  interface kim_set_species_code
    module procedure kim_model_create_set_species_code
  end interface kim_set_species_code

  !> \brief \copybrief KIM::ModelCreate::SetParameterPointer
  !!
  !! \sa KIM::ModelCreate::SetParameterPointer,
  !! KIM_ModelCreate_SetParameterPointerInteger,
  !! KIM_ModelCreate_SetParameterPointerDouble
  !!
  !! \since 2.0
  interface kim_set_parameter_pointer
    module procedure kim_model_create_set_parameter_pointer_integer
    module procedure kim_model_create_set_parameter_pointer_double
  end interface kim_set_parameter_pointer

  !> \brief \copybrief KIM::ModelCreate::SetModelBufferPointer
  !!
  !! \sa KIM::ModelCreate::SetModelBufferPointer,
  !! KIM_ModelCreate_SetModelBufferPointer
  !!
  !! \since 2.0
  interface kim_set_model_buffer_pointer
    module procedure kim_model_create_set_model_buffer_pointer
  end interface kim_set_model_buffer_pointer

  !> \brief \copybrief KIM::ModelCreate::SetUnits
  !!
  !! \sa KIM::ModelCreate::SetUnits, KIM_ModelCreate_SetUnits
  !!
  !! \since 2.0
  interface kim_set_units
    module procedure kim_model_create_set_units
  end interface kim_set_units

  !> \brief \copybrief KIM::ModelCreate::ConvertUnit
  !!
  !! \sa KIM::ModelCreate::ConvertUnit, KIM_ModelCreate_ConvertUnit
  !!
  !! \since 2.0
  interface kim_convert_unit
    module procedure kim_model_create_convert_unit
  end interface kim_convert_unit

  !> \brief \copybrief KIM::ModelCreate::LogEntry
  !!
  !! \sa KIM::ModelCreate::LogEntry, KIM_ModelCreate_LogEntry
  !!
  !! \since 2.0
  interface kim_log_entry
    module procedure kim_model_create_log_entry
  end interface kim_log_entry

  !> \brief \copybrief KIM::ModelCreate::ToString
  !!
  !! \sa KIM::ModelCreate::ToString, KIM_ModelCreate_ToString
  !!
  !! \since 2.0
  interface kim_to_string
    module procedure kim_model_create_to_string
  end interface kim_to_string

contains
  !> \brief Compares kim_model_create_handle_type's for equality.
  !!
  !! \since 2.0
  logical recursive function kim_model_create_handle_equal(lhs, rhs)
    implicit none
    type(kim_model_create_handle_type), intent(in) :: lhs
    type(kim_model_create_handle_type), intent(in) :: rhs

    if ((.not. c_associated(lhs%p)) .and. (.not. c_associated(rhs%p))) then
      kim_model_create_handle_equal = .true.
    else
      kim_model_create_handle_equal = c_associated(lhs%p, rhs%p)
    end if
  end function kim_model_create_handle_equal

  !> \brief Compares kim_model_create_handle_type's for inequality.
  !!
  !! \since 2.0
  logical recursive function kim_model_create_handle_not_equal(lhs, rhs)
    implicit none
    type(kim_model_create_handle_type), intent(in) :: lhs
    type(kim_model_create_handle_type), intent(in) :: rhs

    kim_model_create_handle_not_equal = .not. (lhs == rhs)
  end function kim_model_create_handle_not_equal

  !> \brief \copybrief KIM::ModelCreate::SetModelNumbering
  !!
  !! \sa KIM::ModelCreate::SetModelNumbering, KIM_ModelCreate_SetModelNumbering
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_model_numbering( &
    model_create_handle, numbering, ierr)
    use kim_numbering_module, only: kim_numbering_type
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      integer(c_int) recursive function set_model_numbering( &
        model_create, numbering) &
        bind(c, name="KIM_ModelCreate_SetModelNumbering")
        use, intrinsic :: iso_c_binding
        use kim_numbering_module, only: kim_numbering_type
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(kim_numbering_type), intent(in), value :: numbering
      end function set_model_numbering
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    type(kim_numbering_type), intent(in) :: numbering
    integer(c_int), intent(out) :: ierr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    ierr = set_model_numbering(model_create, numbering)
  end subroutine kim_model_create_set_model_numbering

  !> \brief \copybrief KIM::ModelCreate::SetInfluenceDistancePointer
  !!
  !! \sa KIM::ModelCreate::SetInfluenceDistancePointer,
  !! KIM_ModelCreate_SetInfluenceDistancePointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_influence_distance_pointer( &
    model_create_handle, influence_distance)
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      recursive subroutine set_influence_distance_pointer(model_create, &
                                                          influence_distance) &
        bind(c, name="KIM_ModelCreate_SetInfluenceDistancePointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(c_ptr), intent(in), value :: influence_distance
      end subroutine set_influence_distance_pointer
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    real(c_double), intent(in), target :: influence_distance
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    call set_influence_distance_pointer(model_create, &
                                        c_loc(influence_distance))
  end subroutine kim_model_create_set_influence_distance_pointer

  !> \brief \copybrief KIM::ModelCreate::SetNeighborListPointers
  !!
  !! \sa KIM::ModelCreate::SetNeighborListPointers,
  !! KIM_ModelCreate_SetNeighborListPointers
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_neighbor_list_pointers( &
    model_create_handle, number_of_neighbor_lists, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles)
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      recursive subroutine set_neighbor_list_pointers( &
        model_create, number_of_neighbor_lists, cutoffs_ptr, &
        model_will_not_request_neighbors_of_noncontributing_particles) &
        bind(c, name="KIM_ModelCreate_SetNeighborListPointers")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        integer(c_int), intent(in), value :: number_of_neighbor_lists
        type(c_ptr), intent(in), value :: cutoffs_ptr
        type(c_ptr), intent(in), value :: &
          model_will_not_request_neighbors_of_noncontributing_particles
      end subroutine set_neighbor_list_pointers
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    integer(c_int), intent(in) :: number_of_neighbor_lists
    real(c_double), intent(in), target :: cutoffs(number_of_neighbor_lists)
    integer(c_int), intent(in), target :: &
      model_will_not_request_neighbors_of_noncontributing_particles( &
      number_of_neighbor_lists)
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    call set_neighbor_list_pointers( &
      model_create, number_of_neighbor_lists, c_loc(cutoffs), &
      c_loc(model_will_not_request_neighbors_of_noncontributing_particles))
  end subroutine kim_model_create_set_neighbor_list_pointers

  !> \brief \copybrief KIM::ModelCreate::SetRoutinePointer
  !!
  !! \sa KIM::ModelCreate::SetRoutinePointer, KIM_ModelCreate_SetRoutinePointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_routine_pointer( &
    model_create_handle, model_routine_name, language_name, required, fptr, &
    ierr)
    use kim_model_routine_name_module, only: kim_model_routine_name_type
    use kim_language_name_module, only: kim_language_name_type
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      integer(c_int) recursive function set_routine_pointer( &
        model_create, model_routine_name, language_name, required, fptr) &
        bind(c, name="KIM_ModelCreate_SetRoutinePointer")
        use, intrinsic :: iso_c_binding
        use kim_model_routine_name_module, only: kim_model_routine_name_type
        use kim_language_name_module, only: kim_language_name_type
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(kim_model_routine_name_type), intent(in), value &
          :: model_routine_name
        type(kim_language_name_type), intent(in), value :: language_name
        integer(c_int), intent(in), value :: required
        type(c_funptr), intent(in), value :: fptr
      end function set_routine_pointer
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    type(kim_model_routine_name_type), intent(in) :: model_routine_name
    type(kim_language_name_type), intent(in) :: language_name
    integer(c_int), intent(in) :: required
    type(c_funptr), intent(in), value :: fptr  ! must be left as "value"!?!
    integer(c_int), intent(out) :: ierr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    ierr = set_routine_pointer(model_create, model_routine_name, &
                               language_name, required, fptr)
  end subroutine kim_model_create_set_routine_pointer

  !> \brief \copybrief KIM::ModelCreate::SetSpeciesCode
  !!
  !! \sa KIM::ModelCreate::SetSpeciesCode, KIM_ModelCreate_SetSpeciesCode
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_species_code( &
    model_create_handle, species_name, code, ierr)
    use kim_species_name_module, only: kim_species_name_type
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      integer(c_int) recursive function set_species_code(model_create, &
                                                         species_name, code) &
        bind(c, name="KIM_ModelCreate_SetSpeciesCode")
        use, intrinsic :: iso_c_binding
        use kim_species_name_module, only: kim_species_name_type
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(kim_species_name_type), intent(in), value :: species_name
        integer(c_int), intent(in), value :: code
      end function set_species_code
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    type(kim_species_name_type), intent(in) :: species_name
    integer(c_int), intent(in) :: code
    integer(c_int), intent(out) :: ierr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    ierr = set_species_code(model_create, species_name, code)
  end subroutine kim_model_create_set_species_code

  !> \brief \copybrief KIM::ModelCreate::SetParameterPointer
  !!
  !! \sa KIM::ModelCreate::SetParameterPointer,
  !! KIM_ModelCreate_SetParameterPointerInteger
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_parameter_pointer_integer( &
    model_create_handle, int1, name, description, ierr)
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    integer(c_int), intent(in), target :: int1(:)
    character(len=*, kind=c_char), intent(in) :: name
    character(len=*, kind=c_char), intent(in) :: description
    integer(c_int), intent(out) :: ierr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    call set_parameter(model_create, size(int1, 1, c_int), int1, name, &
                       description, ierr)
    return

  contains
    recursive subroutine set_parameter(model_create, extent, int1, name, &
                                       description, ierr)
      use kim_interoperable_types_module, only: kim_model_create_type
      implicit none
      interface
        integer(c_int) recursive function set_parameter_pointer_integer( &
          model_create, extent, ptr, name, description) &
          bind(c, name="KIM_ModelCreate_SetParameterPointerInteger")
          use, intrinsic :: iso_c_binding
          use kim_interoperable_types_module, only: kim_model_create_type
          implicit none
          type(kim_model_create_type), intent(in) :: model_create
          integer(c_int), intent(in), value :: extent
          type(c_ptr), intent(in), value :: ptr
          character(c_char), intent(in) :: name(*)
          character(c_char), intent(in) :: description(*)
        end function set_parameter_pointer_integer
      end interface
      type(kim_model_create_type), intent(in) :: model_create
      integer(c_int), intent(in) :: extent
      integer(c_int), intent(in), target :: int1(extent)
      character(len=*, kind=c_char), intent(in) :: name
      character(len=*, kind=c_char), intent(in) :: description
      integer(c_int), intent(out) :: ierr

      ierr = set_parameter_pointer_integer(model_create, &
                                           extent, &
                                           c_loc(int1), &
                                           trim(name)//c_null_char, &
                                           trim(description)//c_null_char)
    end subroutine set_parameter
  end subroutine kim_model_create_set_parameter_pointer_integer

  !> \brief \copybrief KIM::ModelCreate::SetParameterPointer
  !!
  !! \sa KIM::ModelCreate::SetParameterPointer,
  !! KIM_ModelCreate_SetParameterPointerDouble
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_parameter_pointer_double( &
    model_create_handle, double1, name, description, ierr)
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    real(c_double), intent(in), target :: double1(:)
    character(len=*, kind=c_char), intent(in) :: name
    character(len=*, kind=c_char), intent(in) :: description
    integer(c_int), intent(out) :: ierr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    call set_parameter(model_create, size(double1, 1, c_int), double1, &
                       name, description, ierr)
    return

  contains
    recursive subroutine set_parameter(model_create, extent, double1, name, &
                                       description, ierr)
      implicit none
      interface
        integer(c_int) recursive function set_parameter_pointer_double( &
          model_create, extent, ptr, name, description) &
          bind(c, name="KIM_ModelCreate_SetParameterPointerDouble")
          use, intrinsic :: iso_c_binding
          use kim_interoperable_types_module, only: kim_model_create_type
          implicit none
          type(kim_model_create_type), intent(in) :: model_create
          integer(c_int), intent(in), value :: extent
          type(c_ptr), intent(in), value :: ptr
          character(c_char), intent(in) :: name(*)
          character(c_char), intent(in) :: description(*)
        end function set_parameter_pointer_double
      end interface
      type(kim_model_create_type), intent(in) :: model_create
      integer(c_int), intent(in) :: extent
      real(c_double), intent(in), target :: double1(extent)
      character(len=*, kind=c_char), intent(in) :: name
      character(len=*, kind=c_char), intent(in) :: description
      integer(c_int), intent(out) :: ierr

      ierr = set_parameter_pointer_double(model_create, &
                                          extent, &
                                          c_loc(double1), &
                                          trim(name)//c_null_char, &
                                          trim(description)//c_null_char)
    end subroutine set_parameter
  end subroutine kim_model_create_set_parameter_pointer_double

  !> \brief \copybrief KIM::ModelCreate::SetModelBufferPointer
  !!
  !! \sa KIM::ModelCreate::SetModelBufferPointer,
  !! KIM_ModelCreate_SetModelBufferPointer
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_model_buffer_pointer( &
    model_create_handle, ptr)
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      recursive subroutine set_model_buffer_pointer(model_create, ptr) &
        bind(c, name="KIM_ModelCreate_SetModelBufferPointer")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(c_ptr), intent(in), value :: ptr
      end subroutine set_model_buffer_pointer
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    type(c_ptr), intent(in) :: ptr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    call set_model_buffer_pointer(model_create, ptr)
  end subroutine kim_model_create_set_model_buffer_pointer

  !> \brief \copybrief KIM::ModelCreate::SetUnits
  !!
  !! \sa KIM::ModelCreate::SetUnits, KIM_ModelCreate_SetUnits
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_set_units( &
    model_create_handle, length_unit, energy_unit, charge_unit, &
    temperature_unit, time_unit, ierr)
    use kim_unit_system_module, only: &
      kim_length_unit_type, &
      kim_energy_unit_type, &
      kim_charge_unit_type, &
      kim_temperature_unit_type, &
      kim_time_unit_type
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      integer(c_int) recursive function set_units( &
        model_create, length_unit, energy_unit, charge_unit, temperature_unit, &
        time_unit) bind(c, name="KIM_ModelCreate_SetUnits")
        use, intrinsic :: iso_c_binding
        use kim_unit_system_module, only: kim_length_unit_type, &
                                          kim_energy_unit_type, &
                                          kim_charge_unit_type, &
                                          kim_temperature_unit_type, &
                                          kim_time_unit_type
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(kim_length_unit_type), intent(in), value :: length_unit
        type(kim_energy_unit_type), intent(in), value :: energy_unit
        type(kim_charge_unit_type), intent(in), value :: charge_unit
        type(kim_temperature_unit_type), intent(in), value :: temperature_unit
        type(kim_time_unit_type), intent(in), value :: time_unit
      end function set_units
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    type(kim_length_unit_type), intent(in) :: length_unit
    type(kim_energy_unit_type), intent(in) :: energy_unit
    type(kim_charge_unit_type), intent(in) :: charge_unit
    type(kim_temperature_unit_type), intent(in) :: temperature_unit
    type(kim_time_unit_type), intent(in) :: time_unit
    integer(c_int), intent(out) :: ierr
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    ierr = set_units(model_create, length_unit, energy_unit, &
                     charge_unit, temperature_unit, time_unit)
  end subroutine kim_model_create_set_units

  !> \brief \copybrief KIM::ModelCreate::ConvertUnit
  !!
  !! \sa KIM::ModelCreate::ConvertUnit, KIM_ModelCreate_ConvertUnit
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_convert_unit( &
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
        bind(c, name="KIM_ModelCreate_ConvertUnit")
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
  end subroutine kim_model_create_convert_unit

  !> \brief \copybrief KIM::ModelCreate::LogEntry
  !!
  !! \sa KIM::ModelCreate::LogEntry, KIM_ModelCreate_LogEntry
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_log_entry(model_create_handle, &
                                                  log_verbosity, message)
    use kim_log_verbosity_module, only: kim_log_verbosity_type
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      recursive subroutine log_entry(model_create, log_verbosity, message, &
                                     line_number, file_name) &
        bind(c, name="KIM_ModelCreate_LogEntry")
        use, intrinsic :: iso_c_binding
        use kim_log_verbosity_module, only: kim_log_verbosity_type
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
        type(kim_log_verbosity_type), intent(in), value :: log_verbosity
        character(c_char), intent(in) :: message(*)
        integer(c_int), intent(in), value :: line_number
        character(c_char), intent(in) :: file_name(*)
      end subroutine log_entry
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    type(kim_log_verbosity_type), intent(in) :: log_verbosity
    character(len=*, kind=c_char), intent(in) :: message
    type(kim_model_create_type), pointer :: model_create

    call c_f_pointer(model_create_handle%p, model_create)
    call log_entry(model_create, log_verbosity, trim(message)//c_null_char, &
                   0, ""//c_null_char)
  end subroutine kim_model_create_log_entry

  !> \brief \copybrief KIM::ModelCreate::ToString
  !!
  !! \sa KIM::ModelCreate::ToString, KIM_ModelCreate_ToString
  !!
  !! \since 2.0
  recursive subroutine kim_model_create_to_string(model_create_handle, string)
    use kim_convert_string_module, only: kim_convert_c_char_ptr_to_string
    use kim_interoperable_types_module, only: kim_model_create_type
    implicit none
    interface
      type(c_ptr) recursive function model_create_string(model_create) &
        bind(c, name="KIM_ModelCreate_ToString")
        use, intrinsic :: iso_c_binding
        use kim_interoperable_types_module, only: kim_model_create_type
        implicit none
        type(kim_model_create_type), intent(in) :: model_create
      end function model_create_string
    end interface
    type(kim_model_create_handle_type), intent(in) :: model_create_handle
    character(len=*, kind=c_char), intent(out) :: string
    type(kim_model_create_type), pointer :: model_create

    type(c_ptr) :: p

    call c_f_pointer(model_create_handle%p, model_create)
    p = model_create_string(model_create)
    call kim_convert_c_char_ptr_to_string(p, string)
  end subroutine kim_model_create_to_string
end module kim_model_create_module
