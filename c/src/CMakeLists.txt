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


set(C_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ChargeUnit_c.cpp
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
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDestroy_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDriverCreate_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRefresh_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Numbering_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SemVer_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SpeciesName_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SupportStatus_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TemperatureUnit_c.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TimeUnit_c.cpp
  )

target_sources(kim-api PRIVATE ${C_SOURCES})
