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
# Copyright (c) 2013--2020, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#    Alexander Stukowski
#

#
# Release: This file is part of the kim-api-2.2.1 package.
#


cmake_minimum_required(VERSION 3.10)
enable_testing()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
list(APPEND CMAKE_PREFIX_PATH ${CMAKE_CURRENT_BINARY_DIR})

include(SetCacheWithFallback)
include(DefaultCompilerStandards)

# Define options
#
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(_LOG_MAX "DEBUG")
else()
  set(_LOG_MAX "INFORMATION")
endif()
set_cache_with_fallback(KIM_API_LOG_MAXIMUM_LEVEL "${_LOG_MAX}" STRING "Maximum log verbosity")
unset(_LOG_MAX)
set_property(CACHE KIM_API_LOG_MAXIMUM_LEVEL PROPERTY STRINGS "" SILENT FATAL ERROR WARNING INFORMATION DEBUG)
#
option(KIM_API_BUILD_EXAMPLES "Build example Drivers, Models, Simulator Models, and Simulators" ON)
#
set(KIM_API_PROJECT_NAME "kim-api" CACHE STRING "KIM API project name string")
mark_as_advanced(KIM_API_PROJECT_NAME)
#
option(KIM_API_ENABLE_SANITIZE "Enable AddressSanitizer" OFF)
mark_as_advanced(KIM_API_ENABLE_SANITIZE)
#
option(KIM_API_ENABLE_COVERAGE "Enable code coverage" OFF)
mark_as_advanced(KIM_API_ENABLE_COVERAGE)
#
string(TIMESTAMP _KIM_API_CONFIGURATION_TIMESTAMP "%Y-%m-%d-%H-%M-%S" UTC)
set(KIM_API_CONFIGURATION_TIMESTAMP "${_KIM_API_CONFIGURATION_TIMESTAMP}" CACHE STRING "UTC timestamp for configuration of kim-api")
unset(_KIM_API_CONFIGURATION_TIMESTAMP)
mark_as_advanced(KIM_API_CONFIGURATION_TIMESTAMP)
#
# Additional options (that depend on call to project()) defined below


#
# Define main project
#
project(${KIM_API_PROJECT_NAME} VERSION 2.2.1 LANGUAGES CXX C Fortran)  # must remain a single line for create-pacakge script
set(PROJECT_DESCRIPTION "An Application Programming Interface (API) for the Knowledgebase of Interatomic Models (KIM).")
#
include(GNUInstallDirs)  # needs to come after languages are enabled
include(RelocatableGNUInstallDirs)  # provides CMAKE_INSTALL_RELOC_*DIRS variables
include(DefineVersionVariables)


# Define options dependent on languages being enabled and/or PROJECT_NAME being defined
#
set(KIM_API_CMAKE_C_COMPILER "${CMAKE_C_COMPILER}" CACHE FILEPATH "C compiler filepath to be used after installation for item compilation.")
mark_as_advanced(KIM_API_CMAKE_C_COMPILER)
set(KIM_API_CMAKE_CXX_COMPILER "${CMAKE_CXX_COMPILER}" CACHE FILEPATH "C++ compiler filepath to be used after installation for item compilation.")
mark_as_advanced(KIM_API_CMAKE_CXX_COMPILER)
set(KIM_API_CMAKE_Fortran_COMPILER "${CMAKE_Fortran_COMPILER}" CACHE FILEPATH "Fortran compiler filepath to be used after installation for item compilation.")
mark_as_advanced(KIM_API_CMAKE_Fortran_COMPILER)

include(DefineInternalVariables)
#
# Define options dependent on internal variables
#
include(CompletionConfig)  # set install dirs for completions;  NOTE: depends on Module 'DefineInternalVariables'
set_cache_with_fallback(KIM_API_USER_CONFIGURATION_FILE ".${PROJECT_NAME}/${KIM_API_UID}/config" FILEPATH "Default configuration file name. If not absolute, then relative to user home directory")
mark_as_advanced(KIM_API_USER_CONFIGURATION_FILE)
# "~/" is expanded by cmake when set(... CACHE PATH ...) is used below; so do not use PATH as type
set_cache_with_fallback(KIM_API_USER_MODEL_DRIVERS_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_UID}/${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" STRING "Default user collection model drivers dir")  # do not use PATH type
mark_as_advanced(KIM_API_USER_MODEL_DRIVERS_DIR_DEFAULT)
set_cache_with_fallback(KIM_API_USER_PORTABLE_MODELS_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_UID}/${KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER}" STRING "Default user collection portable models dir")  # do not use PATH type
mark_as_advanced(KIM_API_USER_PORTABLE_MODELS_DIR_DEFAULT)
set_cache_with_fallback(KIM_API_USER_SIMULATOR_MODELS_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_UID}/${KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER}" STRING "Default user collection simulator models dir")  # do not use PATH type
mark_as_advanced(KIM_API_USER_SIMULATOR_MODELS_DIR_DEFAULT)
#
#
macro(_set_system_dir var identifier type_string)
  set(_default_dir_1 "${PROJECT_NAME}/${identifier}")  # _1 single escape -- to be used directly
  set(_default_dir_2 "${PROJECT_NAME}/${identifier}")  # _2 double escape -- to be passed into macro/function
  set(_description "System collection (${KIM_API_PATH_SEPARATOR}-separated) ${type_string} directory list")
  if(WIN32)
    file(TO_CMAKE_PATH "${CMAKE_INSTALL_FULL_LIBDIR}/${_default_dir_1}" _normalized_libdir_1)
    file(TO_CMAKE_PATH "${CMAKE_INSTALL_FULL_LIBDIR}/${_default_dir_2}" _normalized_libdir_2)
    set(_default_dir_1 "${_normalized_libdir_1}")
    set(_default_dir_2 "${_normalized_libdir_2}")
    unset(_normalized_libdir_1)
    unset(_normalized_libdir_2)
  else()
    set(_default_dir_1 "\${ORIGIN}/${_default_dir_1}")   # Here is the important difference between the _1 and _2
    set(_default_dir_2 "\\\${ORIGIN}/${_default_dir_2}") # Here is the important difference between the _1 and _2
    set(_description "${_description} (entries may begin with $ORIGIN (w/ or w/o {}'s), if appropriate)")
  endif()
  set_cache_with_fallback(${var} "${_default_dir_2}" STRING ${_description})
  if("${${var}}" MATCHES "^${KIM_API_PATH_SEPARATOR}")
    set(${var} "${_default_dir_1}${${var}}")
  endif()
  unset(_default_dir_1)
  unset(_default_dir_2)
  unset(_description)
  mark_as_advanced(${var})
endmacro(_set_system_dir)
#
_set_system_dir(KIM_API_SYSTEM_MODEL_DRIVERS_DIR "${KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER}" "model drivers")
#
_set_system_dir(KIM_API_SYSTEM_PORTABLE_MODELS_DIR "${KIM_API_PORTABLE_MODEL_PLURAL_IDENTIFIER}" "portable models")
#
_set_system_dir(KIM_API_SYSTEM_SIMULATOR_MODELS_DIR "${KIM_API_SIMULATOR_MODEL_PLURAL_IDENTIFIER}" "simulator models")


include(DefaultCompilerFlags)

# Define kim-api target
#
add_library(kim-api SHARED "")
set_target_properties(kim-api
  PROPERTIES
    OUTPUT_NAME "${PROJECT_NAME}"
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}/fortran/${KIM_API_Fortran_MODULE_DIR_IDENTIFIER}"
    )
target_include_directories(kim-api PUBLIC
  $<BUILD_INTERFACE:$<TARGET_PROPERTY:kim-api,Fortran_MODULE_DIRECTORY>>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_RELOC_LIBDIR}/${PROJECT_NAME}/${KIM_API_Fortran_MODULE_DIR_IDENTIFIER}>)
target_link_libraries(kim-api ${CMAKE_DL_LIBS})

# Add install rules for kim-api
#
install(TARGETS kim-api
  EXPORT KIM_API_Targets
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  INCLUDES DESTINATION "${CMAKE_INSTALL_RELOC_INCLUDEDIR}/${PROJECT_NAME}"
  LIBRARY DESTINATION "${CMAKE_INSTALL_RELOC_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_RELOC_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_RELOC_BINDIR}")

# Add install rules for various metadata files
#
install(
  FILES
    "${PROJECT_SOURCE_DIR}/README.md"
    "${PROJECT_SOURCE_DIR}/NEWS"
    "${PROJECT_SOURCE_DIR}/LICENSE.CDDL"
  DESTINATION
    "${CMAKE_INSTALL_RELOC_DOCDIR}"
  )

# Add include subdirectories
#
add_subdirectory(cpp/include)
add_subdirectory(c/include)
add_subdirectory(fortran/include)

# Add src subdirectories
#
add_subdirectory(cpp/src)
add_subdirectory(c/src)
add_subdirectory(fortran/src)

# Add cmake subdirectory
#
add_subdirectory(cmake)

#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}" KIM_API_C_ID_PROJECT_NAME)  # used in ./completions and ./utils
#

# Add utils subdirectory
#
add_subdirectory(utils)

# Add other subdirectories
#
add_subdirectory(docs)
add_subdirectory(completions)
add_subdirectory(editors/emacs)
add_subdirectory(pkg-config)

# Add Models & Drivers
#
if(KIM_API_BUILD_EXAMPLES)
  set(KIM_API_CMAKE_PREFIX_DIR "")  # avoid uninitialized variable warning
  set(KIM-API_DIR "${PROJECT_BINARY_DIR}/cmake/${KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER}")
  set(KIM-API-ITEMS_DIR "${PROJECT_BINARY_DIR}/cmake/${KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER}-items")
  add_subdirectory(examples/model-drivers)
  add_subdirectory(examples/portable-models)
  add_subdirectory(examples/simulator-models)
  add_subdirectory(examples/simulators)
  unset(KIM_API_CMAKE_PREFIX_DIR)
  unset(KIM-API_DIR)
  unset(KIM-API-ITEMS_DIR)
endif()

include(WriteKIMConfigSummary)
