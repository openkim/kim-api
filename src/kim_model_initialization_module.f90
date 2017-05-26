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


module kim_model_initialization_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_initialization_type, &
    kim_model_initialization_set_model_numbering, &
    kim_model_initialization_set_influence_distance, &
    kim_model_initialization_set_cutoffs, &
    kim_model_initialization_set_reinit, &
    kim_model_initialization_set_destroy, &
    kim_model_initialization_set_compute_func, &
    kim_model_initialization_set_species_code, &
    kim_model_initialization_set_argument_attribute, &
    kim_model_initialization_set_call_back_attribute, &
    kim_model_initialization_set_parameter, &
    kim_model_initialization_set_model_buffer, &
    kim_model_initialization_set_units, &
    kim_model_initialization_convert_unit, &
    kim_model_initialization_log, &
    kim_model_initialization_string

  type, bind(c) :: kim_model_initialization_type
    private
    type(c_ptr) :: p
  end type kim_model_initialization_type

  interface kim_model_initialization_set_parameter
    subroutine kim_model_initialization_set_parameter_int( &
      model_initialization, int1, description, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      integer(c_int), intent(in), target :: int1(:)
      character(len=*), intent(in) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_parameter_int

    subroutine kim_model_initialization_set_parameter_double( &
      model_initialization, double1, description, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      real(c_double), intent(in), target :: double1(:)
      character(len=*), intent(in) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_parameter_double
  end interface kim_model_initialization_set_parameter

  interface
    subroutine kim_model_initialization_set_model_numbering( &
      model_initialization, numbering, ierr)
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_numbering_type), intent(in), value :: numbering
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_model_numbering

    subroutine kim_model_initialization_set_influence_distance( &
      model_initialization, influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      real(c_double), intent(in), target :: influence_distance
    end subroutine kim_model_initialization_set_influence_distance

    subroutine kim_model_initialization_set_cutoffs(model_initialization, &
      number_of_cutoffs, cutoffs)
      use, intrinsic :: iso_c_binding
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      integer(c_int), intent(in), value :: number_of_cutoffs
      real(c_double), intent(in), target :: cutoffs(number_of_cutoffs)
    end subroutine kim_model_initialization_set_cutoffs

    subroutine kim_model_initialization_set_reinit(model_initialization, &
      language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_reinit

    subroutine kim_model_initialization_set_destroy(model_initialization, &
      language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_destroy

    subroutine kim_model_initialization_set_compute_func(model_initialization, &
      language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_compute_func

    subroutine kim_model_initialization_set_species_code(model_initialization, &
      species_name, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_species_code

    subroutine kim_model_initialization_set_argument_attribute( &
      model_initialization, argument_name, attribute, ierr)
      use, intrinsic :: iso_c_binding
      use kim_argument_name_module, only : kim_argument_name_type
      use kim_attribute_module, only : kim_attribute_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_argument_name_type), intent(in), value :: argument_name
      type(kim_attribute_type), intent(in), value :: attribute
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_argument_attribute

    subroutine kim_model_initialization_set_call_back_attribute( &
      model_initialization, call_back_name, attribute, ierr)
      use, intrinsic :: iso_c_binding
      use kim_call_back_name_module, only : kim_call_back_name_type
      use kim_attribute_module, only : kim_attribute_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(kim_call_back_name_type), intent(in), value :: call_back_name
      type(kim_attribute_type), intent(in), value :: attribute
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_call_back_attribute

    subroutine kim_model_initialization_set_model_buffer(model_initialization, &
      ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(inout) :: model_initialization
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_initialization_set_model_buffer

    subroutine kim_model_initialization_set_units(model_initialization, &
      length_unit, energy_unit, charge_unit, temperature_unit, time_unit, ierr)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : &
        kim_length_unit_type, &
        kim_energy_unit_type, &
        kim_charge_unit_type, &
        kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(in) :: model_initialization
      type(kim_length_unit_type), intent(in), value :: length_unit
      type(kim_energy_unit_type), intent(in), value :: energy_unit
      type(kim_charge_unit_type), intent(in), value :: charge_unit
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      type(kim_time_unit_type), intent(in), value :: time_unit
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_set_units

    subroutine kim_model_initialization_convert_unit( &
      model_initialization, from_length_unit, from_energy_unit, &
      from_charge_unit, from_temperature_unit, from_time_unit, &
      to_length_unit, to_energy_unit, to_charge_unit, to_temperature_unit, &
      to_time_unit, length_exponent, energy_exponent, charge_exponent, &
      temperature_exponent, time_exponent, conversion_factor, ierr)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : kim_length_unit_type
      use kim_unit_system_module, only : kim_energy_unit_type
      use kim_unit_system_module, only : kim_charge_unit_type
      use kim_unit_system_module, only : kim_temperature_unit_type
      use kim_unit_system_module, only : kim_time_unit_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(in) :: model_initialization
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
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_initialization_convert_unit

    subroutine kim_model_initialization_log(model_initialization, &
      log_verbosity, message, line_number, file_name)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(in) :: model_initialization
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*), intent(in) :: message
      integer(c_int), intent(in), value :: line_number
      character(len=*), intent(in) :: file_name
    end subroutine kim_model_initialization_log

    subroutine kim_model_initialization_string(model_initialization, string)
      use, intrinsic :: iso_c_binding
      import kim_model_initialization_type
      implicit none
      type(kim_model_initialization_type), intent(in) :: model_initialization
      character(len=*), intent(out) :: string
    end subroutine kim_model_initialization_string
end interface
end module kim_model_initialization_module
