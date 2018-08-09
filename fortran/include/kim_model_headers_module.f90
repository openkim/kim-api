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


module kim_model_headers_module
  use, intrinsic :: iso_c_binding

  use kim_model_create_module, only : &
    kim_model_create_handle_type, &
    kim_model_create_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_create_set_model_numbering, &
    kim_model_create_set_influence_distance_pointer, &
    kim_model_create_set_neighbor_list_pointers, &
    kim_model_create_set_refresh_pointer, &
    kim_model_create_set_destroy_pointer, &
    kim_model_create_set_compute_arguments_create_pointer, &
    kim_model_create_set_compute_arguments_destroy_pointer, &
    kim_model_create_set_compute_pointer, &
    kim_model_create_set_species_code, &
    kim_model_create_set_parameter_pointer, &
    kim_model_create_set_model_buffer_pointer, &
    kim_model_create_set_units, &
    kim_model_create_convert_unit, &
    kmcrle => kim_model_create_log_entry, kim_model_create_log_entry, &
    kim_model_create_string

  use kim_log_verbosity_module, only : &
    kim_log_verbosity_type, &
    kim_log_verbosity_from_string, &
    operator (.lt.), &
    operator (.gt.), &
    operator (.le.), &
    operator (.ge.), &
    operator (.eq.), &
    operator (.ne.), &
    kim_log_verbosity_string, &
    kim_log_verbosity_silent, klvs => kim_log_verbosity_silent, &
    kim_log_verbosity_fatal, klvf => kim_log_verbosity_fatal, &
    kim_log_verbosity_error, klve => kim_log_verbosity_error, &
    kim_log_verbosity_warning, klvw => kim_log_verbosity_warning, &
    kim_log_verbosity_information, klvi => kim_log_verbosity_information, &
    kim_log_verbosity_debug, klvd => kim_log_verbosity_debug, &
    kim_log_verbosity_get_number_of_log_verbosities, &
    kim_log_verbosity_get_log_verbosity, &
    kim_log_file, klf  => kim_log_file, &
    kim_log_message, klm => kim_log_message

  use kim_language_name_module
  use kim_numbering_module
  use kim_species_name_module
  use kim_support_status_module
  use kim_unit_system_module
  use kim_model_compute_arguments_create_module, only : &
    kim_model_compute_arguments_create_handle_type, &
    kim_model_compute_arguments_create_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_compute_arguments_create_set_argument_support_status, &
    kim_model_compute_arguments_create_set_callback_support_status, &
    kim_model_compute_arguments_create_set_model_buffer_pointer, &
    kim_model_compute_arguments_create_log_entry, &
    kmcacle => kim_model_compute_arguments_create_log_entry, &
    kim_model_compute_arguments_create_string

  use kim_compute_argument_name_module
  use kim_data_type_module
  use kim_compute_callback_name_module
  use kim_model_compute_arguments_module, only : &
    kim_model_compute_arguments_handle_type, &
    kim_model_compute_arguments_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_compute_arguments_get_neighbor_list, &
    kim_model_compute_arguments_process_dedr_term, &
    kim_model_compute_arguments_process_d2edr2_term, &
    kim_model_compute_arguments_get_argument_pointer, &
    kim_model_compute_arguments_is_callback_present, &
    kim_model_compute_arguments_set_model_buffer_pointer, &
    kim_model_compute_arguments_get_model_buffer_pointer, &
    kim_model_compute_arguments_log_entry, &
    kmcale => kim_model_compute_arguments_log_entry, &
    kim_model_compute_arguments_string

  use kim_model_compute_arguments_destroy_module, only : &
    kim_model_compute_arguments_destroy_handle_type, &
    kim_model_compute_arguments_destroy_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_compute_arguments_destroy_get_model_buffer_pointer, &
    kim_model_compute_arguments_destroy_log_entry, &
    kmcadle => kim_model_compute_arguments_destroy_log_entry, &
    kim_model_compute_arguments_destroy_string

  use kim_model_compute_module, only : &
    kim_model_compute_handle_type, &
    kim_model_compute_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_compute_get_model_buffer_pointer, &
    kim_model_compute_log_entry, kmcole => kim_model_compute_log_entry, &
    kim_model_compute_string

  use kim_model_refresh_module, only : &
    kim_model_refresh_handle_type, &
    kim_model_refresh_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_refresh_set_influence_distance_pointer, &
    kim_model_refresh_set_neighbor_list_pointers, &
    kim_model_refresh_get_model_buffer_pointer, &
    kim_model_refresh_log_entry, kmrle => kim_model_refresh_log_entry, &
    kim_model_refresh_string

  use kim_model_destroy_module, only : &
    kim_model_destroy_handle_type, &
    kim_model_destroy_null_handle, &
    operator (.eq.), &
    operator (.ne.), &
    kim_model_destroy_get_model_buffer_pointer, &
    kim_model_destroy_log_entry, kmdle => kim_model_destroy_log_entry, &
    kim_model_destroy_string

  implicit none
  public
end module kim_model_headers_module
