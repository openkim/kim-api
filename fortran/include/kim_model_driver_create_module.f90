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


module kim_model_driver_create_module
  use, intrinsic :: iso_c_binding
  implicit none
  private

  public &
    kim_model_driver_create_handle_type, &
    kim_model_driver_create_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_driver_create_get_number_of_parameter_files, &
    kim_model_driver_create_get_parameter_file_name, &
    kim_model_driver_create_set_model_numbering, &
    kim_model_driver_create_set_influence_distance_pointer, &
    kim_model_driver_create_set_neighbor_list_pointers, &
    kim_model_driver_create_set_refresh_pointer, &
    kim_model_driver_create_set_destroy_pointer, &
    kim_model_driver_create_set_compute_arguments_create_pointer, &
    kim_model_driver_create_set_compute_arguments_destroy_pointer, &
    kim_model_driver_create_set_compute_pointer, &
    kim_model_driver_create_set_species_code, &
    kim_model_driver_create_set_parameter_pointer, &
    kim_model_driver_create_set_model_buffer_pointer, &
    kim_model_driver_create_set_units, &
    kim_model_driver_create_convert_unit, &
    kim_model_driver_create_log_entry, &
    kim_model_driver_create_string

  type, bind(c) :: kim_model_driver_create_handle_type
    type(c_ptr) :: p = c_null_ptr
  end type kim_model_driver_create_handle_type

  type(kim_model_driver_create_handle_type), protected, save &
    :: kim_model_driver_create_null_handle

  interface operator (.eq.)
    logical function kim_model_driver_create_handle_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) :: left
      type(kim_model_driver_create_handle_type), intent(in) :: right
    end function kim_model_driver_create_handle_equal
  end interface operator (.eq.)

  interface operator (.ne.)
    logical function kim_model_driver_createhandle_not_equal(left, right)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) :: left
      type(kim_model_driver_create_handle_type), intent(in) :: right
    end function kim_model_driver_createhandle_not_equal
  end interface operator (.ne.)

  interface kim_model_driver_create_set_parameter_pointer
    subroutine kim_model_driver_create_set_parameter_pointer_integer( &
      model_driver_create_handle, int1, description, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      integer(c_int), intent(in), target :: int1(:)
      character(len=*, kind=c_char), intent(in) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_parameter_pointer_integer

    subroutine kim_model_driver_create_set_parameter_pointer_double( &
      model_driver_create_handle, double1, description, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      real(c_double), intent(in), target :: double1(:)
      character(len=*, kind=c_char), intent(in) :: description
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_parameter_pointer_double
  end interface kim_model_driver_create_set_parameter_pointer

  interface
    subroutine kim_model_driver_create_get_number_of_parameter_files( &
      model_driver_create_handle, number_of_parameter_files)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      integer(c_int), intent(out) :: number_of_parameter_files
    end subroutine kim_model_driver_create_get_number_of_parameter_files

    subroutine kim_model_driver_create_get_parameter_file_name( &
      model_driver_create_handle, index, parameter_file_name, ierr)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      integer(c_int), intent(in), value :: index
      character(len=*, kind=c_char), intent(out) :: parameter_file_name
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_get_parameter_file_name

    subroutine kim_model_driver_create_set_model_numbering( &
      model_driver_create_handle, numbering, ierr)
      use, intrinsic :: iso_c_binding
      use kim_numbering_module, only : kim_numbering_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_numbering_type), intent(in), value :: numbering
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_model_numbering

    subroutine kim_model_driver_create_set_influence_distance_pointer( &
      model_driver_create_handle, influence_distance)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      real(c_double), intent(in), target :: influence_distance
    end subroutine kim_model_driver_create_set_influence_distance_pointer

    subroutine kim_model_driver_create_set_neighbor_list_pointers( &
      model_driver_create_handle, number_of_neighbor_lists, cutoffs, &
      model_will_not_request_neighbors_of_noncontributing_particles)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(inout) &
        :: model_driver_create_handle
      integer(c_int), intent(in), value :: number_of_neighbor_lists
      real(c_double), intent(in), target :: cutoffs(number_of_neighbor_lists)
      integer(c_int), intent(in), target :: &
        model_will_not_request_neighbors_of_noncontributing_particles( &
        number_of_neighbor_lists)
    end subroutine kim_model_driver_create_set_neighbor_list_pointers

    subroutine kim_model_driver_create_set_refresh_pointer( &
      model_driver_create_handle, language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_refresh_pointer

    subroutine kim_model_driver_create_set_destroy_pointer( &
      model_driver_create_handle, language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_destroy_pointer

    subroutine kim_model_driver_create_set_compute_arguments_create_pointer( &
      model_driver_create_handle, language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_compute_arguments_create_pointer

    subroutine kim_model_driver_create_set_compute_arguments_destroy_pointer( &
      model_driver_create_handle, language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_compute_arguments_destroy_pointer

    subroutine kim_model_driver_create_set_compute_pointer( &
      model_driver_create_handle, language_name, fptr, ierr)
      use, intrinsic :: iso_c_binding
      use kim_language_name_module, only : kim_language_name_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_language_name_type), intent(in), value :: language_name
      type(c_funptr), intent(in), value :: fptr
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_compute_pointer

    subroutine kim_model_driver_create_set_species_code( &
      model_driver_create_handle, species_name, code, ierr)
      use, intrinsic :: iso_c_binding
      use kim_species_name_module, only : kim_species_name_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_species_name_type), intent(in), value :: species_name
      integer(c_int), intent(in), value :: code
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_species_code

    subroutine kim_model_driver_create_set_model_buffer_pointer( &
      model_driver_create_handle, ptr)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(c_ptr), intent(in), value :: ptr
    end subroutine kim_model_driver_create_set_model_buffer_pointer

    subroutine kim_model_driver_create_set_units( &
      model_driver_create_handle, length_unit, energy_unit, charge_unit, &
      temperature_unit, time_unit, ierr)
      use, intrinsic :: iso_c_binding
      use kim_unit_system_module, only : &
        kim_length_unit_type, &
        kim_energy_unit_type, &
        kim_charge_unit_type, &
        kim_temperature_unit_type, &
        kim_time_unit_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_length_unit_type), intent(in), value :: length_unit
      type(kim_energy_unit_type), intent(in), value :: energy_unit
      type(kim_charge_unit_type), intent(in), value :: charge_unit
      type(kim_temperature_unit_type), intent(in), value :: temperature_unit
      type(kim_time_unit_type), intent(in), value :: time_unit
      integer(c_int), intent(out) :: ierr
    end subroutine kim_model_driver_create_set_units

    subroutine kim_model_driver_create_convert_unit( &
      model_driver_create_handle, from_length_unit, from_energy_unit, &
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
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
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
    end subroutine kim_model_driver_create_convert_unit

    subroutine kim_model_driver_create_log_entry( &
      model_driver_create_handle, log_verbosity, message, line_number, &
      file_name)
      use, intrinsic :: iso_c_binding
      use kim_log_verbosity_module, only : kim_log_verbosity_type
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      type(kim_log_verbosity_type), intent(in), value :: log_verbosity
      character(len=*, kind=c_char), intent(in) :: message
      integer(c_int), intent(in), value :: line_number
      character(len=*, kind=c_char), intent(in) :: file_name
    end subroutine kim_model_driver_create_log_entry

    subroutine kim_model_driver_create_string( &
      model_driver_create_handle, string)
      use, intrinsic :: iso_c_binding
      import kim_model_driver_create_handle_type
      implicit none
      type(kim_model_driver_create_handle_type), intent(in) &
        :: model_driver_create_handle
      character(len=*, kind=c_char), intent(out) :: string
    end subroutine kim_model_driver_create_string
  end interface
end module kim_model_driver_create_module
