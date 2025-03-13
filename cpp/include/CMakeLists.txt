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
# Release: This file is part of the kim-api.git repository.
#


target_include_directories(kim-api PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  )

configure_file(KIM_LOG_DEFINES.inc.in ${CMAKE_CURRENT_BINARY_DIR}/KIM_LOG_DEFINES.inc)
configure_file(KIM_Version.hpp.in ${CMAKE_CURRENT_BINARY_DIR}/KIM_Version.hpp)

set(CPP_HEADERS
  KIM_ChargeUnit.hpp
  KIM_Collection.hpp
  KIM_CollectionItemType.hpp
  KIM_Collections.hpp
  KIM_ComputeArgumentName.hpp
  KIM_ComputeArguments.hpp
  KIM_ComputeCallbackName.hpp
  KIM_DataType.hpp
  KIM_EnergyUnit.hpp
  KIM_FunctionTypes.hpp
  KIM_LanguageName.hpp
  KIM_LengthUnit.hpp
  KIM_Log.hpp
  KIM_LogMacros.hpp
  KIM_LogVerbosity.hpp
  KIM_ModelComputeArgumentsCreate.hpp
  KIM_ModelComputeArgumentsDestroy.hpp
  KIM_ModelComputeArguments.hpp
  KIM_ModelCompute.hpp
  KIM_ModelCreate.hpp
  KIM_ModelExtension.hpp
  KIM_ModelDestroy.hpp
  KIM_ModelDriverCreate.hpp
  KIM_ModelDriverHeaders.hpp
  KIM_ModelHeaders.hpp
  KIM_Model.hpp
  KIM_ModelRefresh.hpp
  KIM_ModelRoutineName.hpp
  KIM_ModelWriteParameterizedModel.hpp
  KIM_Numbering.hpp
  KIM_SemVer.hpp
  KIM_SimulatorHeaders.hpp
  KIM_SimulatorModel.hpp
  KIM_SpeciesName.hpp
  KIM_SupportedExtensions.hpp
  KIM_SupportStatus.hpp
  KIM_TemperatureUnit.hpp
  KIM_TimeUnit.hpp
  KIM_UnitSystem.hpp
  ${CMAKE_CURRENT_BINARY_DIR}/KIM_LOG_DEFINES.inc
  ${CMAKE_CURRENT_BINARY_DIR}/KIM_Version.hpp
  )

install(FILES ${CPP_HEADERS}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_INCLUDEDIR}/${PROJECT_NAME})
