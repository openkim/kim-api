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


module kim_model_driver_initialization_f_module
  implicit none
  private

  public &
    get_number_of_parameter_files, &
    get_parameter_file_name, &
    set_model_numbering, &
    set_influence_distance, &
    set_cutoffs, &
    set_reinit, &
    set_destroy, &
    set_compute_func, &
    set_species_code, &
    set_argument_attribute, &
    set_call_back_attribute, &
    set_parameter_int, &
    set_parameter_double, &
    set_model_buffer, &
    set_units, &
    convert_unit, &
    log, &
    model_driver_initialization_string

  interface
    subroutine get_number_of_parameter_files(model_driver_initialization, &
      number_of_parameter_files) &
      bind(c, &
      name="KIM_ModelDriverInitialization_get_number_of_parameter_files")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      integer(c_int), intent(out) :: number_of_parameter_files
    end subroutine get_number_of_parameter_files

    integer(c_int) function get_parameter_file_name( &
      model_driver_initialization, index, parameter_file_name) &
      bind(c, name="KIM_ModelDriverInitialization_get_parameter_file_name")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      integer(c_int), intent(in), value :: index
      type(c_ptr), intent(out) :: parameter_file_name
    end function get_parameter_file_name

    integer(c_int) function set_model_numbering(model_driver_initialization, &
      numbering) &
      bind(c, name="KIM_ModelDriverInitialization_set_model_numbering")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      use kim_numbering_module, only : kim_numbering_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      type(kim_numbering_type), intent(in), value :: numbering
    end function set_model_numbering

    subroutine set_influence_distance(model_driver_initialization, &
      influence_distance) &
      bind(c, name="KIM_ModelDriverInitialization_set_influence_distance")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      type(c_ptr), intent(in), value :: influence_distance
    end subroutine set_influence_distance

    subroutine set_cutoffs(model_driver_initialization, number_of_cutoffs, &
      cutoffs_ptr) bind(c, name="KIM_ModelDriverInitialization_set_cutoffs")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      integer(c_int), intent(in), value :: number_of_cutoffs
      type(c_ptr), intent(in), value :: cutoffs_ptr
    end subroutine set_cutoffs

    integer(c_int) function set_reinit(model_driver_initialization, &
      language_name, fptr) &
      bind(c, name="KIM_ModelDriverInitialization_set_reinit")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_reinit

    integer(c_int) function set_destroy(model_driver_initialization, &
      language_name, fptr) &
      bind(c, name="KIM_ModelDriverInitialization_set_destroy")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_destroy

    integer(c_int) function set_compute_func(model_driver_initialization, &
      language_name, fptr) &
      bind(c, name="KIM_ModelDriverInitialization_set_compute_func")
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
    end function set_compute_func

    integer(c_int) function set_species_code(model_driver_initialization, &
      species_name, code) &
      bind(c, name="KIM_ModelDriverInitialization_set_species_code")
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
    end function set_species_code

    integer(c_int) function set_argument_attribute( &
      model_driver_initialization, argument_name, attribute) &
      bind(c, name="KIM_ModelDriverInitialization_set_argument_attribute")
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      use kim_attribute_module, only : kim_attribute_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_attribute_type), intent(in), value :: attribute
    end function set_argument_attribute

    integer(c_int) function set_call_back_attribute( &
      model_driver_initialization, call_back_name, attribute) &
      bind(c, name="KIM_ModelDriverInitialization_set_call_back_attribute")
      use, intrinsic :: iso_c_binding
      use kim_call_back_name_module, only : kim_call_back_name_type
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      use kim_attribute_module, only : kim_attribute_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      type(kim_call_back_name_type), intent(in), value :: call_back_name
      type(kim_attribute_type), intent(in), value :: attribute
    end function set_call_back_attribute

    integer(c_int) function set_parameter_int(model_driver_initialization, &
      extent, ptr, description) &
      bind(c, name="KIM_ModelDriverInitialization_set_parameter_int")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      character(c_char), intent(in) :: description(*)
    end function set_parameter_int

    integer(c_int) function set_parameter_double(model_driver_initialization, &
      extent, ptr, description) &
      bind(c, name="KIM_ModelDriverInitialization_set_parameter_double")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      integer(c_int), intent(in), value :: extent
      type(c_ptr), intent(in), value :: ptr
      character(c_char), intent(in) :: description(*)
    end function set_parameter_double

    subroutine set_model_buffer(model_driver_initialization, ptr) &
      bind(c, name="KIM_ModelDriverInitialization_set_model_buffer")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(inout) &
        :: model_driver_initialization
      type(c_ptr), intent(in), value :: ptr
    end subroutine set_model_buffer

    integer(c_int) function set_units(model_driver_initialization, &
      length_unit, energy_unit, charge_unit, temperature_unit, time_unit) &
      bind(c, name="KIM_ModelDriverInitialization_set_units")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      use kim_unit_system_module, only : &
        kim_length_unit_type, &
        kim_energy_unit_type, &
        kim_charge_unit_type, &
        kim_temperature_unit_type, &
        kim_time_unit_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      type(kim_length_unit_type), intent(in), value :: length_unit
      type(kim_energy_unit_type), intent(in), value :: energy_unit
      type(kim_charge_unit_type), intent(in), value :: charge_unit
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      type(kim_time_unit_type), intent(in), value :: time_unit
    end function set_units

    integer(c_int) function convert_unit( &
      model_driver_initialization, from_length_unit, from_energy_unit, &
      from_charge_unit, from_temperature_unit, from_time_unit, &
      to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
      to_time_unit, length_exponent, energy_exponent, charge_exponent, &
      temperature_exponent, time_exponent, conversion_factor) &
      bind(c, name="KIM_ModelDriverInitialization_convert_unit")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_unit_system_module, only : kim_time_unit_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
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

    subroutine log(model_driver_initialization, log_verbosity, message, &
      line_number, file_name) bind(c, name="KIM_ModelDriverInitialization_Log")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(c_char), intent(in) :: message(*)
      integer(c_int), intent(in), value :: line_number
      character(c_char), intent(in) :: file_name(*)
    end subroutine log

    type(c_ptr) function model_driver_initialization_string( &
      model_driver_initialization) &
      bind(c, name="KIM_ModelDriverInitialization_string")
      use, intrinsic :: iso_c_binding
      use kim_model_driver_initialization_module, only &
        : kim_model_driver_initialization_type
      implicit none
      type(kim_model_driver_initialization_type), intent(in) &
        :: model_driver_initialization
    end function model_driver_initialization_string
  end interface
end module kim_model_driver_initialization_f_module


! free functions to implement kim_model_driver_initialization_module

subroutine kim_model_driver_initialization_get_number_of_parameter_files( &
  model_driver_initialization, number_of_parameter_files)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only &
    : get_number_of_parameter_files
  implicit none
  type(kim_model_driver_initialization_type), intent(in) &
    :: model_driver_initialization
  integer(c_int), intent(out) :: number_of_parameter_files

  call get_number_of_parameter_files(model_driver_initialization, &
    number_of_parameter_files)
end subroutine kim_model_driver_initialization_get_number_of_parameter_files

subroutine kim_model_driver_initialization_get_parameter_file_name( &
  model_driver_initialization, index, parameter_file_name, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only &
    : get_parameter_file_name
  implicit none
  type(kim_model_driver_initialization_type), intent(in) &
    :: model_driver_initialization
  integer(c_int), intent(in), value :: index
  character(len=*), intent(out) :: parameter_file_name
  integer(c_int), intent(out) :: ierr

  type(c_ptr) :: p
  character(len=len(parameter_file_name)), pointer :: fp
  integer(c_int) :: null_index

  ierr = get_parameter_file_name(model_driver_initialization, &
    index-1, p)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  parameter_file_name = fp(1:null_index)
end subroutine kim_model_driver_initialization_get_parameter_file_name

subroutine kim_model_driver_initialization_set_model_numbering( &
  model_driver_initialization, numbering, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_numbering_module, only : kim_numbering_type
  use kim_model_driver_initialization_f_module, only : set_model_numbering
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_numbering_type), intent(in), value :: numbering
  integer(c_int), intent(out) :: ierr

  ierr = set_model_numbering(model_driver_initialization, numbering)
end subroutine kim_model_driver_initialization_set_model_numbering

subroutine kim_model_driver_initialization_set_influence_distance( &
  model_driver_initialization, influence_distance)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_influence_distance
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  real(c_double), intent(in), target :: influence_distance

  call set_influence_distance(model_driver_initialization, &
    c_loc(influence_distance))
end subroutine kim_model_driver_initialization_set_influence_distance

subroutine kim_model_driver_initialization_set_cutoffs( &
  model_driver_initialization, number_of_cutoffs, cutoffs)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_cutoffs
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  integer(c_int), intent(in), value :: number_of_cutoffs
  real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)

  call set_cutoffs(model_driver_initialization, number_of_cutoffs, &
    c_loc(cutoffs))
end subroutine kim_model_driver_initialization_set_cutoffs

subroutine kim_model_driver_initialization_set_reinit( &
  model_driver_initialization, language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_reinit
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_reinit(model_driver_initialization, language_name, fptr)
end subroutine kim_model_driver_initialization_set_reinit

subroutine kim_model_driver_initialization_set_destroy( &
  model_driver_initialization, language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_destroy
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_destroy(model_driver_initialization, language_name, fptr)
end subroutine kim_model_driver_initialization_set_destroy

subroutine kim_model_driver_initialization_set_compute_func( &
  model_driver_initialization, language_name, fptr, ierr)
  use, intrinsic :: iso_c_binding
  use kim_language_name_module, only : kim_language_name_type
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_compute_func
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_language_name_type), intent(in), value :: language_name
  type(c_funptr), intent(in), value :: fptr
  integer(c_int), intent(out) :: ierr

  ierr = set_compute_func(model_driver_initialization, language_name, fptr)
end subroutine kim_model_driver_initialization_set_compute_func

subroutine kim_model_driver_initialization_set_species_code( &
  model_driver_initialization, species_name, code, ierr)
  use, intrinsic :: iso_c_binding
  use kim_species_name_module, only : kim_species_name_type
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_species_code
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_species_name_type), intent(in), value :: species_name
  integer(c_int), intent(in), value :: code
  integer(c_int), intent(out) :: ierr

  ierr = set_species_code(model_driver_initialization, species_name, code)
end subroutine kim_model_driver_initialization_set_species_code

subroutine kim_model_driver_initialization_set_argument_attribute( &
  model_driver_initialization, argument_name, attribute, ierr)
  use, intrinsic :: iso_c_binding
  use kim_argument_name_module, only : kim_argument_name_type
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_attribute_module, only : kim_attribute_type
  use kim_model_driver_initialization_f_module, only : set_argument_attribute
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_argument_name_type), intent(in), value :: argument_name
  type(kim_attribute_type), intent(in), value :: attribute
  integer(c_int), intent(out) :: ierr

  ierr = set_argument_attribute(model_driver_initialization, argument_name, &
    attribute)
end subroutine kim_model_driver_initialization_set_argument_attribute

subroutine kim_model_driver_initialization_set_call_back_attribute( &
  model_driver_initialization, call_back_name, attribute, ierr)
  use, intrinsic :: iso_c_binding
  use kim_call_back_name_module, only : kim_call_back_name_type
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_attribute_module, only : kim_attribute_type
  use kim_model_driver_initialization_f_module, only : set_call_back_attribute
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(kim_call_back_name_type), intent(in), value :: call_back_name
  type(kim_attribute_type), intent(in), value :: attribute
  integer(c_int), intent(out) :: ierr

  ierr = set_call_back_attribute(model_driver_initialization, call_back_name, &
    attribute)
end subroutine kim_model_driver_initialization_set_call_back_attribute

subroutine kim_model_driver_initialization_set_parameter_int( &
  model_driver_initialization, int1, description, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  integer(c_int), intent(in), target :: int1(:)
  character(len=*), intent(in) :: description
  integer(c_int), intent(out) :: ierr

  call set_parameter(model_driver_initialization, size(int1, 1, c_int), int1, &
    description, ierr)
  return

contains
  subroutine set_parameter(model_driver_initialization, extent, int1, &
    description, ierr)
    use, intrinsic :: iso_c_binding
    use kim_model_driver_initialization_module, only &
      : kim_model_driver_initialization_type
    use kim_model_driver_initialization_f_module, only : set_parameter_int
    implicit none
    type(kim_model_driver_initialization_type), intent(inout) &
      :: model_driver_initialization
    integer(c_int), intent(in), value :: extent
    integer(c_int), intent(in), target :: int1(extent)
    character(len=*), intent(in) :: description
    integer(c_int), intent(out) :: ierr

    ierr = set_parameter_int(model_driver_initialization, extent, c_loc(int1), &
      trim(description)//c_null_char)
  end subroutine set_parameter
end subroutine kim_model_driver_initialization_set_parameter_int

subroutine kim_model_driver_initialization_set_parameter_double( &
  model_driver_initialization, double1, description, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  real(c_double), intent(in), target :: double1(:)
  character(len=*), intent(in) :: description
  integer(c_int), intent(out) :: ierr

  call set_parameter(model_driver_initialization, size(double1, 1, c_int), &
    double1, description, ierr)
  return

contains
  subroutine set_parameter(model_driver_initialization, extent, double1, &
    description, ierr)
    use, intrinsic :: iso_c_binding
    use kim_model_driver_initialization_module, only &
      : kim_model_driver_initialization_type
    use kim_model_driver_initialization_f_module, only : set_parameter_int
    implicit none
    type(kim_model_driver_initialization_type), intent(inout) &
      :: model_driver_initialization
    integer(c_int), intent(in), value :: extent
    real(c_double), intent(in), target :: double1(extent)
    character(len=*), intent(in) :: description
    integer(c_int), intent(out) :: ierr

    ierr = set_parameter_int(model_driver_initialization, extent, &
      c_loc(double1), trim(description)//c_null_char)
  end subroutine set_parameter
end subroutine kim_model_driver_initialization_set_parameter_double

subroutine kim_model_driver_initialization_set_model_buffer( &
  model_driver_initialization, ptr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only : set_model_buffer
  implicit none
  type(kim_model_driver_initialization_type), intent(inout) &
    :: model_driver_initialization
  type(c_ptr), intent(in), value :: ptr

  call set_model_buffer(model_driver_initialization, ptr)
end subroutine kim_model_driver_initialization_set_model_buffer

subroutine kim_model_driver_initialization_set_units( &
  model_driver_initialization, length_unit, energy_unit, charge_unit, &
  temperature_unit, time_unit, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_unit_system_module, only : &
    kim_length_unit_type, &
    kim_energy_unit_type, &
    kim_charge_unit_type, &
    kim_temperature_unit_type, &
    kim_time_unit_type
  use kim_model_driver_initialization_f_module, only : set_units
  implicit none
  type(kim_model_driver_initialization_type), intent(in) &
    :: model_driver_initialization
  type(kim_length_unit_type), intent(in), value :: length_unit
  type(kim_energy_unit_type), intent(in), value :: energy_unit
  type(kim_charge_unit_type), intent(in), value :: charge_unit
  type(kim_temperature_unit_type), intent(in), value :: temperature_unit
  type(kim_time_unit_type), intent(in), value :: time_unit
  integer(c_int), intent(out) :: ierr

  ierr = set_units(model_driver_initialization, length_unit, energy_unit, &
    charge_unit, temperature_unit, time_unit)
end subroutine kim_model_driver_initialization_set_units

subroutine kim_model_driver_initialization_convert_unit( &
  model_driver_initialization, from_length_unit, from_energy_unit, &
  from_charge_unit, from_temperature_unit, from_time_unit, &
  to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
  to_time_unit, length_exponent, energy_exponent, charge_exponent, &
  temperature_exponent, time_exponent, conversion_factor, ierr)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_unit_system_module, only : kim_length_unit_type
  use kim_unit_system_module, only : kim_energy_unit_type
  use kim_unit_system_module, only : kim_charge_unit_type
  use kim_unit_system_module, only : kim_temperature_unit_type
  use kim_unit_system_module, only : kim_time_unit_type
  use kim_model_driver_initialization_f_module, only : convert_unit
  implicit none
  type(kim_model_driver_initialization_type), intent(in) &
    :: model_driver_initialization
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

  ierr = convert_unit(model_driver_initialization, from_length_unit, &
    from_energy_unit, from_charge_unit, from_temperature_unit, &
    from_time_unit, to_length_unit, to_energy_unit, to_charge_unit, &
    to_temperature_unit, to_time_unit, length_exponent, energy_exponent, &
    charge_exponent, temperature_exponent, time_exponent, conversion_factor)
end subroutine kim_model_driver_initialization_convert_unit

subroutine kim_model_driver_initialization_log(model_driver_initialization, &
  log_verbosity, message, line_number, file_name)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_log_verbosity_module, only : kim_log_verbosity_type
  use kim_model_driver_initialization_f_module, only : log
  implicit none
  type(kim_model_driver_initialization_type), intent(in) &
    :: model_driver_initialization
  type(kim_log_verbosity_type), intent(in), value :: log_verbosity
  character(len=*), intent(in) :: message
  integer(c_int), intent(in), value :: line_number
  character(len=*), intent(in) :: file_name

  call log(model_driver_initialization, log_verbosity, &
    trim(message)//c_null_char, line_number, trim(file_name)//c_null_char)
end subroutine kim_model_driver_initialization_log

subroutine kim_model_driver_initialization_string(model_driver_initialization, &
  string)
  use, intrinsic :: iso_c_binding
  use kim_model_driver_initialization_module, only &
    : kim_model_driver_initialization_type
  use kim_model_driver_initialization_f_module, only &
    : model_driver_initialization_string
  implicit none
  type(kim_model_driver_initialization_type), intent(in) &
    :: model_driver_initialization
  character(len=*), intent(out) :: string

  type(c_ptr) :: p
  character(len=len(string)), pointer :: fp
  integer(c_int) :: null_index

  p = model_driver_initialization_string(model_driver_initialization)
  call c_f_pointer(p, fp)
  null_index = scan(fp, char(0))-1
  string = fp(1:null_index)
end subroutine kim_model_driver_initialization_string
