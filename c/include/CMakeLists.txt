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
# Copyright (c) 2013--2021, Regents of the University of Minnesota.
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
