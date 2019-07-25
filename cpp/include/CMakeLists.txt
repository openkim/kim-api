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
# Copyright (c) 2013--2019, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api-2.1.1 package.
#


target_include_directories(kim-api PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

configure_file(KIM_LOG_DEFINES.inc.in ${CMAKE_BINARY_DIR}/KIM_LOG_DEFINES.inc)
configure_file(KIM_Version.hpp.in ${CMAKE_BINARY_DIR}/KIM_Version.hpp)

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
  ${CMAKE_BINARY_DIR}/KIM_LOG_DEFINES.inc
  ${CMAKE_BINARY_DIR}/KIM_Version.hpp
  )

install(FILES ${CPP_HEADERS} DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME})
