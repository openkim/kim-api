#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#
# SPDX-License-Identifier: LGPL-2.1-or-later
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

#
# Release: This file is part of the kim-api-2.4.1 package.
#


target_include_directories(kim-api PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

set(F_MOD_DIR $<TARGET_PROPERTY:kim-api,Fortran_MODULE_DIRECTORY>)
set(FORTRAN_MODULES
  ${F_MOD_DIR}/kim_charge_unit_module.mod
  ${F_MOD_DIR}/kim_collection_item_type_module.mod
  ${F_MOD_DIR}/kim_collection_module.mod
  ${F_MOD_DIR}/kim_collections_module.mod
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
  ${F_MOD_DIR}/kim_model_extension_module.mod
  ${F_MOD_DIR}/kim_model_destroy_module.mod
  ${F_MOD_DIR}/kim_model_driver_create_module.mod
  ${F_MOD_DIR}/kim_model_driver_headers_module.mod
  ${F_MOD_DIR}/kim_model_headers_module.mod
  ${F_MOD_DIR}/kim_model_module.mod
  ${F_MOD_DIR}/kim_model_refresh_module.mod
  ${F_MOD_DIR}/kim_model_routine_name_module.mod
  ${F_MOD_DIR}/kim_model_write_parameterized_model_module.mod
  ${F_MOD_DIR}/kim_numbering_module.mod
  ${F_MOD_DIR}/kim_sem_ver_module.mod
  ${F_MOD_DIR}/kim_simulator_headers_module.mod
  ${F_MOD_DIR}/kim_simulator_model_module.mod
  ${F_MOD_DIR}/kim_species_name_module.mod
  ${F_MOD_DIR}/kim_support_status_module.mod
  ${F_MOD_DIR}/kim_supported_extensions_module.mod
  ${F_MOD_DIR}/kim_temperature_unit_module.mod
  ${F_MOD_DIR}/kim_time_unit_module.mod
  ${F_MOD_DIR}/kim_unit_system_module.mod
  )

install(FILES ${FORTRAN_MODULES}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_LIBDIR}/${PROJECT_NAME}/${KIM_API_Fortran_MODULE_DIR_IDENTIFIER})

set(F_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_charge_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_collection_item_type_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_collection_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_collections_module.f90
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
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_extension_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_headers_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_headers_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_routine_name_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_write_parameterized_model_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_numbering_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_sem_ver_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_simulator_headers_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_simulator_model_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_species_name_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_support_status_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_supported_extensions_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_temperature_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_time_unit_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_unit_system_module.f90
  )

target_sources(kim-api PRIVATE ${F_SOURCES})
