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
#    Alexander Stukowski
#

#
# Release: This file is part of the kim-api.git repository.
#


target_include_directories(kim-api PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_BINARY_DIR}
  ${PROJECT_SOURCE_DIR}/cmake  # for KIM_SharedLibrarySchema.hpp
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

target_sources(kim-api PRIVATE ${CPP_SOURCES})

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
