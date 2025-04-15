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
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

#
# Release: This file is part of the kim-api-2.4.1 package.
#


set(C_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ChargeUnit_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Collection_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Collections_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_CollectionItemType_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArgumentName_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArguments_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeCallbackName_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_DataType_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_EnergyUnit_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LanguageName_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LengthUnit_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Log_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LogVerbosity_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Model_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCompute_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArguments_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsCreate_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsDestroy_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCreate_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelExtension_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDestroy_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDriverCreate_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRefresh_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRoutineName_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelWriteParameterizedModel_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Numbering_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SemVer_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SimulatorModel_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SpeciesName_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SupportStatus_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TemperatureUnit_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TimeUnit_c.cpp
  )

target_sources(kim-api PRIVATE ${C_SOURCES})
