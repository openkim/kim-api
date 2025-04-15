#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#    Alexander Stukowski
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


target_include_directories(kim-api PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${PROJECT_SOURCE_DIR}/cmake  # for KIM_SharedLibrarySchema.hpp
  ${CMAKE_CURRENT_SOURCE_DIR}/libb64/libb64/include
  )

configure_file(KIM_Configuration.hpp.in ${CMAKE_CURRENT_BINARY_DIR}/KIM_Configuration.hpp)

set(CPP_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ChargeUnit.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Collection.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_CollectionItemType.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Collections.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_CollectionsImplementation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArgumentName.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArguments.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeCallbackName.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_DataType.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_EnergyUnit.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_FilesystemPath.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LanguageName.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LengthUnit.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Log.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LogVerbosity.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Model.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCompute.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArguments.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsCreate.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelComputeArgumentsDestroy.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelCreate.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDestroy.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelDriverCreate.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelExtension.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRefresh.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelRoutineName.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelWriteParameterizedModel.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_Numbering.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SemVer.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SpeciesName.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SupportStatus.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SimulatorModel.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SimulatorModelImplementation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TemperatureUnit.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_TimeUnit.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ComputeArgumentsImplementation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_LogImplementation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_ModelImplementation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/KIM_SharedLibrary.cpp
)

set(C_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/libb64/libb64/src/cdecode.c
  ${CMAKE_CURRENT_SOURCE_DIR}/libb64/libb64/src/cencode.c
)
target_compile_definitions(kim-api PRIVATE BUFFERSIZE=${KIM_API_BASE64_BUFFERSIZE})

target_sources(kim-api PRIVATE ${CPP_SOURCES} ${C_SOURCES})

if(WIN32 AND NOT CYGWIN)
  # Activate C++17 language standard on Win32 platform, because kim-api code uses C++ filesystem library internally.
  # Note: The 'cxx_std_17' target compile feature was added by CMake version 3.8.
  # For older CMake versions, fall back to setting the CXX_STANDARD target property instead.
  get_property(cxx_features GLOBAL PROPERTY CMAKE_CXX_KNOWN_FEATURES)
  if("cxx_std_17" IN_LIST cxx_features)
    target_compile_features(kim-api PUBLIC cxx_std_17)
  else()
    set_property(TARGET kim-api PROPERTY CXX_STANDARD 17)
    set_property(TARGET kim-api PROPERTY CXX_STANDARD_REQUIRED YES)
  endif()
endif()

# The KIM API code uses POSIX.1-2008 functions such as strdup() or mkstemp(), which are
# not part of the ISO C standard. Thus, when compiling with -std=c++98 instead of -std=gnu98,
# these function would be unavailable.
# Defining _DEFAULT_SOURCE during compilation tells the GNU C Library headers to make the
# POSIX and System V functions available also in strict ANSI C mode.
# See also: https://www.gnu.org/software/libc/manual/html_node/Feature-Test-Macros.html
target_compile_definitions(kim-api PRIVATE _DEFAULT_SOURCE)
