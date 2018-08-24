#
# CDDL HEADER START
#
# The contents of this file are subject to the terms of the Common Development
# and Distribution License Version 1.0 (the "License").
#
# You can obtain a copy of the license at
# http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
# specific language governing permissions and limitations under the License.
#
# When distributing Covered Code, include this CDDL HEADER in each file and
# include the License file in a prominent location with the name LICENSE.CDDL.
# If applicable, add the following below this CDDL HEADER, with the fields
# enclosed by brackets "[]" replaced with your own identifying information:
#
# Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
#
# CDDL HEADER END
#

#
# Copyright (c) 2013--2018, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


target_include_directories(kim-api PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

configure_file(kim_version.fd.in ${CMAKE_BINARY_DIR}/kim_version.fd)

set(F_MOD_DIR $<TARGET_PROPERTY:kim-api,Fortran_MODULE_DIRECTORY>)
set(FORTRAN_INCLUDES
  ${F_MOD_DIR}/kim_charge_unit_module.mod
  ${F_MOD_DIR}/kim_compute_argument_name_module.mod
  ${F_MOD_DIR}/kim_compute_arguments_module.mod
  ${F_MOD_DIR}/kim_compute_callback_name_module.mod
  ${F_MOD_DIR}/kim_data_type_module.mod
  ${F_MOD_DIR}/kim_energy_unit_module.mod
  ${F_MOD_DIR}/kim_language_name_module.mod
  ${F_MOD_DIR}/kim_length_unit_module.mod
  ${F_MOD_DIR}/kim_log_module.mod
  ${F_MOD_DIR}/kim_log_verbosity_module.mod
  ${F_MOD_DIR}/kim_model_compute_arguments_create_module.mod
  ${F_MOD_DIR}/kim_model_compute_arguments_destroy_module.mod
  ${F_MOD_DIR}/kim_model_compute_arguments_module.mod
  ${F_MOD_DIR}/kim_model_compute_module.mod
  ${F_MOD_DIR}/kim_model_create_module.mod
  ${F_MOD_DIR}/kim_model_destroy_module.mod
  ${F_MOD_DIR}/kim_model_driver_create_module.mod
  ${F_MOD_DIR}/kim_model_driver_headers_module.mod
  ${F_MOD_DIR}/kim_model_headers_module.mod
  ${F_MOD_DIR}/kim_model_module.mod
  ${F_MOD_DIR}/kim_model_refresh_module.mod
  ${F_MOD_DIR}/kim_numbering_module.mod
  ${F_MOD_DIR}/kim_sem_ver_module.mod
  ${F_MOD_DIR}/kim_simulator_headers_module.mod
  ${F_MOD_DIR}/kim_species_name_module.mod
  ${F_MOD_DIR}/kim_support_status_module.mod
  ${F_MOD_DIR}/kim_temperature_unit_module.mod
  ${F_MOD_DIR}/kim_time_unit_module.mod
  ${F_MOD_DIR}/kim_unit_system_module.mod
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_create_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_destroy_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_create_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_log_macros.fd
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_log_macros.fd
  ${CMAKE_BINARY_DIR}/kim_version.fd
  )

install(FILES ${FORTRAN_INCLUDES} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_Fortran_MODULE_DIR_IDENTIFIER})

set(F_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_charge_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_argument_name_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_arguments_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_callback_name_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_data_type_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_energy_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_language_name_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_length_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_verbosity_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_create_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_destroy_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_create_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_headers_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_headers_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_numbering_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_sem_ver_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_simulator_headers_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_species_name_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_support_status_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_temperature_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_time_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_unit_system_module.f90
  )

target_sources(kim-api PRIVATE ${F_SOURCES})
