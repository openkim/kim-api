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
# Release: This file is part of the kim-api-2.3.0 package.
#


target_include_directories(kim-api PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  )

configure_file(KIM_Version.h.in ${CMAKE_CURRENT_BINARY_DIR}/KIM_Version.h)

set(C_HEADERS
    KIM_ChargeUnit.h
    KIM_Collection.h
    KIM_Collections.h
    KIM_CollectionItemType.h
    KIM_ComputeArgumentName.h
    KIM_ComputeArguments.h
    KIM_ComputeCallbackName.h
    KIM_DataType.h
    KIM_EnergyUnit.h
    KIM_FunctionTypes.h
    KIM_LanguageName.h
    KIM_LengthUnit.h
    KIM_Log.h
    KIM_LogMacros.h
    KIM_LogVerbosity.h
    KIM_Model.h
    KIM_ModelCompute.h
    KIM_ModelComputeArgumentsCreate.h
    KIM_ModelComputeArgumentsDestroy.h
    KIM_ModelComputeArguments.h
    KIM_ModelCreate.h
    KIM_ModelExtension.h
    KIM_ModelDestroy.h
    KIM_ModelDriverCreate.h
    KIM_ModelDriverHeaders.h
    KIM_ModelHeaders.h
    KIM_ModelRefresh.h
    KIM_ModelRoutineName.h
    KIM_ModelWriteParameterizedModel.h
    KIM_Numbering.h
    KIM_SemVer.h
    KIM_SimulatorHeaders.h
    KIM_SimulatorModel.h
    KIM_SpeciesName.h
    KIM_SupportStatus.h
    KIM_SupportedExtensions.h
    KIM_TemperatureUnit.h
    KIM_TimeUnit.h
    KIM_UnitSystem.h
    ${CMAKE_CURRENT_BINARY_DIR}/KIM_Version.h
    )

install(FILES ${C_HEADERS}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_INCLUDEDIR}/${PROJECT_NAME})
