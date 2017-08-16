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


module kim_model_create_f_module
  implicit none
  private

  public &
    set_model_numbering, &
    set_influence_distance_pointer, &
    set_cutoffs_pointer, &
    set_refresh_pointer, &
    set_destroy_pointer, &
    set_compute_pointer, &
    set_species_code, &
    set_argument_support_status, &
    set_callback_support_status, &
    set_parameter_pointer_integer, &
    set_parameter_pointer_double, &
    set_model_buffer_pointer, &
    set_units, &
    convert_unit, &
    log, &
    model_create_string

  interface
    integer(c_int) function set_model_numbering(model_create, &
      numbering) bind(c, name="KIM_ModelCreate_SetModelNumbering")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      use kim_numbering_module, only : kim_numbering_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      type(kim_numbering_type), intent(in), value :: numbering
    end function set_model_numbering

    subroutine set_influence_distance_pointer(model_create, &
      influence_distance) &
      bind(c, name="KIM_ModelCreate_SetInfluenceDistancePointer")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      type(c_ptr), intent(in), value :: influence_distance
    end subroutine set_influence_distance_pointer

    subroutine set_cutoffs_pointer(model_create, number_of_cutoffs, &
      cutoffs_ptr) bind(c, name="KIM_ModelCreate_SetCutoffsPointer")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      integer(c_int), intent(in), value :: number_of_cutoffs
      type(c_ptr), intent(in), value :: cutoffs_ptr
    end subroutine set_cutoffs_pointer

    integer(c_int) function set_refresh_pointer(model_create, &
      language_name, fptr) &
      bind(c, name="KIM_ModelCreate_SetRefreshPointer")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_refresh_pointer

    integer(c_int) function set_destroy_pointer(model_create, &
      language_name, fptr) &
      bind(c, name="KIM_ModelCreate_SetDestroyPointer")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_destroy_pointer

    integer(c_int) function set_compute_pointer(model_create, &
      language_name, fptr) &
      bind(c, name="KIM_ModelCreate_SetComputePointer")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_compute_pointer

    integer(c_int) function set_species_code(model_create, &
      species_name, code) &
      bind(c, name="KIM_ModelCreate_SetSpeciesCode")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
    end function set_species_code

    integer(c_int) function set_argument_support_status(model_create, &
      argument_name, support_status) &
      bind(c, name="KIM_ModelCreate_SetArgumentSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_model_create_module, only : kim_model_create_type
      use kim_support_status_module, only : kim_support_status_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_support_status_type), intent(in), value :: support_status
    end function set_argument_support_status

    integer(c_int) function set_callback_support_status(model_create, &
      callback_name, support_status) &
      bind(c, name="KIM_ModelCreate_SetCallbackSupportStatus")
      use, intrinsic :: iso_c_binding
      use kim_callback_name_module, only : kim_callback_name_type
      use kim_model_create_module, only : kim_model_create_type
      use kim_support_status_module, only : kim_support_status_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
      type(kim_callback_name_type), intent(in), value :: callback_name
      type(kim_support_status_type), intent(in), value :: support_status
    end function set_callback_support_status

    integer(c_int) function set_parameter_pointer_integer( &
      model_create, extent, ptr, description) &
      bind(c, name="KIM_ModelCreate_SetParameterPointerInteger")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      character(c_char), intent(in) :: description(*)
    end function set_parameter_pointer_integer

    integer(c_int) function set_parameter_pointer_double(model_create, &
      extent, ptr, description) &
      bind(c, name="KIM_ModelCreate_SetParameterPointerDouble")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      character(c_char), intent(in) :: description(*)
    end function set_parameter_pointer_double

    subroutine set_model_buffer_pointer(model_create, ptr) &
      bind(c, name="KIM_ModelCreate_SetModelBufferPointer")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(inout) :: model_create
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_model_buffer_pointer

    integer(c_int) function set_units(model_create, length_unit, &
      energy_unit, charge_unit, temperature_unit, time_unit) &
      bind(c, name="KIM_ModelCreate_SetUnits")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      use kim_unit_system_module, only : &
        kim_length_unit_type, &
        kim_energy_unit_type, &
        kim_charge_unit_type, &
        kim_temperature_unit_type, &
        kim_time_unit_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
      type(kim_length_unit_type), intent(in), value :: length_unit
      type(kim_energy_unit_type), intent(in), value :: energy_unit
      type(kim_charge_unit_type), intent(in), value :: charge_unit
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      type(kim_time_unit_type), intent(in), value :: time_unit
    end function set_units

    integer(c_int) function convert_unit( &
      model_create, from_length_unit, from_energy_unit, &
      from_charge_unit, from_temperature_unit, from_time_unit, &
      to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
      to_time_unit, length_exponent, energy_exponent, charge_exponent, &
      temperature_exponent, time_exponent, conversion_factor) &
      bind(c, name="KIM_ModelCreate_ConvertUnit")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_unit_system_module, only : kim_time_unit_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
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

    subroutine log(model_create, log_verbosity, message, line_number, &
      file_name) bind(c, name="KIM_ModelCreate_Log")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(c_char), intent(in) :: message(*)
      integer(c_int), intent(in), value :: line_number
      character(c_char), intent(in) :: file_name(*)
    end subroutine log

    type(c_ptr) function model_create_string(model_create) &
      bind(c, name="KIM_ModelCreate_String")
      use, intrinsic :: iso_c_binding
      use kim_model_create_module, only : kim_model_create_type
      implicit none
      type(kim_model_create_type), intent(in) :: model_create
    end function model_create_string
  end interface
end module kim_model_create_f_module


! free functions to implement kim_model_create_module

subroutine kim_model_create_set_model_numbering( &
  model_create, numbering, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_numbering_module, only : kim_numbering_type
  use kim_model_create_f_module, only : set_model_numbering
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_numbering_type), intent(in), value :: numbering
  integer(c_int), intent(out) :: ierr

  ierr = set_model_numbering(model_create, numbering)
end subroutine kim_model_create_set_model_numbering

subroutine kim_model_create_set_influence_distance_pointer( &
  model_create, influence_distance)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_influence_distance_pointer
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  real(c_double), intent(in), target :: influence_distance

  call set_influence_distance_pointer(model_create, &
    c_loc(influence_distance))
end subroutine kim_model_create_set_influence_distance_pointer

subroutine kim_model_create_set_cutoffs_pointer(model_create, &
  number_of_cutoffs, cutoffs)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_cutoffs_pointer
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  integer(c_int), intent(in), value :: number_of_cutoffs
  real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)

  call set_cutoffs_pointer(model_create, number_of_cutoffs, &
    c_loc(cutoffs))
end subroutine kim_model_create_set_cutoffs_pointer

subroutine kim_model_create_set_refresh_pointer( &
  model_create, language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_refresh_pointer
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_refresh_pointer(model_create, language_name, fptr)
end subroutine kim_model_create_set_refresh_pointer

subroutine kim_model_create_set_destroy_pointer(model_create, &
  language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_destroy_pointer
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_destroy_pointer(model_create, language_name, fptr)
end subroutine kim_model_create_set_destroy_pointer

subroutine kim_model_create_set_compute_pointer(model_create, &
  language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_compute_pointer
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_compute_pointer(model_create, language_name, fptr)
end subroutine kim_model_create_set_compute_pointer

subroutine kim_model_create_set_species_code(model_create, &
  species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_species_code
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(in), value :: code
  integer(c_int), intent(out) :: ierr

  ierr = set_species_code(model_create, species_name, code)
end subroutine kim_model_create_set_species_code

subroutine kim_model_create_set_argument_support_status( &
  model_create, argument_name, support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_create_module, only : kim_model_create_type
  use kim_support_status_module, only : kim_support_status_type
  use kim_model_create_f_module, only : set_argument_support_status
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_argument_name_type), intent(in), value :: argument_name
  type(kim_support_status_type), intent(in), value :: support_status
  integer(c_int), intent(out) :: ierr

  ierr = set_argument_support_status(model_create, argument_name, &
    support_status)
end subroutine kim_model_create_set_argument_support_status

subroutine kim_model_create_set_callback_support_status( &
  model_create, callback_name, support_status, ierr)
  use, intrinsic :: iso_c_binding
  use kim_callback_name_module, only : kim_callback_name_type
  use kim_model_create_module, only : kim_model_create_type
  use kim_support_status_module, only : kim_support_status_type
  use kim_model_create_f_module, only : set_callback_support_status
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(kim_callback_name_type), intent(in), value :: callback_name
  type(kim_support_status_type), intent(in), value :: support_status
  integer(c_int), intent(out) :: ierr

  ierr = set_callback_support_status(model_create, callback_name, &
    support_status)
end subroutine kim_model_create_set_callback_support_status

subroutine kim_model_create_set_parameter_pointer_integer( &
  model_create, int1, description, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  integer(c_int), intent(in), target :: int1(:)
  character(len=*), intent(in) :: description
  integer(c_int), intent(out) :: ierr

  call set_parameter(model_create, size(int1, 1, c_int), int1, &
    description, ierr)
  return

contains
  subroutine set_parameter(model_create, extent, int1, &
    description, ierr)
    use, intrinsic :: iso_c_binding
    use kim_model_create_module, only : kim_model_create_type
    use kim_model_create_f_module, only : set_parameter_pointer_integer
    implicit none
    type(kim_model_create_type), intent(inout) :: model_create
    integer(c_int), intent(in), value :: extent
    integer(c_int), intent(in), target :: int1(extent)
    character(len=*), intent(in) :: description
    integer(c_int), intent(out) :: ierr

    ierr = set_parameter_pointer_integer(model_create, extent, &
      c_loc(int1), trim(description)//c_null_char)
  end subroutine set_parameter
end subroutine kim_model_create_set_parameter_pointer_integer

subroutine kim_model_create_set_parameter_pointer_double( &
  model_create, double1, description, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  real(c_double), intent(in), target :: double1(:)
  character(len=*), intent(in) :: description
  integer(c_int), intent(out) :: ierr

  call set_parameter(model_create, size(double1, 1, c_int), double1, &
    description, ierr)
  return

contains
  subroutine set_parameter(model_create, extent, double1, &
    description, ierr)
    use, intrinsic :: iso_c_binding
    use kim_model_create_module, only : kim_model_create_type
    use kim_model_create_f_module, only : set_parameter_pointer_integer
    implicit none
    type(kim_model_create_type), intent(inout) :: model_create
    integer(c_int), intent(in), value :: extent
    real(c_double), intent(in), target :: double1(extent)
    character(len=*), intent(in) :: description
    integer(c_int), intent(out) :: ierr

    ierr = set_parameter_pointer_integer(model_create, extent, &
      c_loc(double1), trim(description)//c_null_char)
  end subroutine set_parameter
end subroutine kim_model_create_set_parameter_pointer_double

subroutine kim_model_create_set_model_buffer_pointer( &
  model_create, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : set_model_buffer_pointer
  implicit none
  type(kim_model_create_type), intent(inout) :: model_create
  type(c_ptr), intent(in), value :: ptr

  call set_model_buffer_pointer(model_create, ptr)
end subroutine kim_model_create_set_model_buffer_pointer

subroutine kim_model_create_set_units(model_create, &
  length_unit, energy_unit, charge_unit, temperature_unit, time_unit, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_unit_system_module, only : &
    kim_length_unit_type, &
    kim_energy_unit_type, &
    kim_charge_unit_type, &
    kim_temperature_unit_type, &
    kim_time_unit_type
  use kim_model_create_f_module, only : set_units
  implicit none
  type(kim_model_create_type), intent(in) :: model_create
  type(kim_length_unit_type), intent(in), value :: length_unit
  type(kim_energy_unit_type), intent(in), value :: energy_unit
  type(kim_charge_unit_type), intent(in), value :: charge_unit
  type(kim_temperature_unit_type), intent(in), value :: temperature_unit
  type(kim_time_unit_type), intent(in), value :: time_unit
  integer(c_int), intent(out) :: ierr

  ierr = set_units(model_create, length_unit, energy_unit, &
    charge_unit, temperature_unit, time_unit)
end subroutine kim_model_create_set_units

subroutine kim_model_create_convert_unit( &
  model_create, from_length_unit, from_energy_unit, &
  from_charge_unit, from_temperature_unit, from_time_unit, &
  to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
  to_time_unit, length_exponent, energy_exponent, charge_exponent, &
  temperature_exponent, time_exponent, conversion_factor, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_unit_system_module, only : kim_length_unit_type
  use kim_unit_system_module, only : kim_energy_unit_type
  use kim_unit_system_module, only : kim_charge_unit_type
  use kim_unit_system_module, only : kim_temperature_unit_type
  use kim_unit_system_module, only : kim_time_unit_type
  use kim_model_create_f_module, only : convert_unit
  implicit none
  type(kim_model_create_type), intent(in) :: model_create
  type(kim_length_unit_type), intent(in), value :: from_length_unit
  type(kim_energy_unit_type), intent(in), value :: from_energy_unit
  type(kim_charge_unit_type), intent(in), value :: from_charge_unit
  type(kim_temperature_unit_type), intent(in), value :: from_temperature_unit
  type(kim_time_unit_type), intent(in), value :: from_time_unit
  type(kim_length_unit_type), intent(in), value :: to_length_unit
  type(kim_energy_unit_type), intent(in), value :: to_energy_unit
  type(kim_charge_unit_type), intent(in), value :: to_charge_unit
  type(kim_temperature_unit_type), intent(in), value :: to_temperature_unit
  type(kim_time_unit_type), intent(in), value :: to_time_unit
  real(c_double), intent(in), value :: length_exponent
  real(c_double), intent(in), value :: energy_exponent
  real(c_double), intent(in), value :: charge_exponent
  real(c_double), intent(in), value :: temperature_exponent
  real(c_double), intent(in), value :: time_exponent
  real(c_double), intent(out) :: conversion_factor
  integer(c_int), intent(out) :: ierr

  ierr = convert_unit(model_create, from_length_unit, &
    from_energy_unit, from_charge_unit, from_temperature_unit, &
    from_time_unit, to_length_unit, to_energy_unit, to_charge_unit, &
    to_temperature_unit, to_time_unit, length_exponent, energy_exponent, &
    charge_exponent, temperature_exponent, time_exponent, conversion_factor)
end subroutine kim_model_create_convert_unit

subroutine kim_model_create_log(model_create, log_verbosity, &
  message, line_number, file_name)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_model_create_f_module, only : log
  implicit none
  type(kim_model_create_type), intent(in) :: model_create
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  character(len=*), intent(in) :: message
  integer(c_int), intent(in), value :: line_number
  character(len=*), intent(in) :: file_name

  call log(model_create, log_verbosity, trim(message)//c_null_char, &
    line_number, trim(file_name)//c_null_char)
end subroutine kim_model_create_log

subroutine kim_model_create_string(model_create, string)
  use, intrinsic :: iso_c_binding
  use kim_model_create_module, only : kim_model_create_type
  use kim_model_create_f_module, only : model_create_string
  implicit none
  type(kim_model_create_type), intent(in) :: model_create
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)), pointer :: fp
  integer(c_int) :: null_index

  p = model_create_string(model_create)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_model_create_string
