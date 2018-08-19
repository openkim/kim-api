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


set(F_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_charge_unit_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_argument_name_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_arguments_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_compute_callback_name_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_convert_string_module.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_data_type_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_energy_unit_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_language_name_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_length_unit_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_log_verbosity_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_create_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_destroy_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_arguments_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_compute_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_create_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_destroy_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_driver_create_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_model_refresh_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_numbering_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_sem_ver_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_species_name_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_support_status_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_temperature_unit_f.f90
  ${CMAKE_CURRENT_SOURCE_DIR}/kim_time_unit_f.f90
  )

target_sources(kim-api PRIVATE ${F_SOURCES})
