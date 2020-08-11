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
# Release: This file is part of the kim-api.git repository.
#


cmake_minimum_required(VERSION 3.4)
enable_testing()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
list(APPEND CMAKE_PREFIX_PATH ${CMAKE_CURRENT_BINARY_DIR})

include(DefaultCompilerStandards)

# Define options
#
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(_LOG_MAX "DEBUG")
else()
  set(_LOG_MAX "INFORMATION")
endif()
set(KIM_API_LOG_MAXIMUM_LEVEL "${_LOG_MAX}" CACHE STRING "Maximum log verbosity")
set_property(CACHE KIM_API_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)
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
mark_as_advanced(KIM_API_CONFIGURATION_TIMESTAMP)
#
# Additional options (that depend on call to project()) defined below


#
# Define main project
#
project(${KIM_API_PROJECT_NAME} VERSION 2.2.0 LANGUAGES CXX C Fortran)  # must remain a single line for create-pacakge script
set(PROJECT_DESCRIPTION "An Application Programming Interface (API) for the Knowledgebase of Interatomic Models (KIM).")
include(GNUInstallDirs)  # needs to come after call to project()
include(CompletionConfig)  # set install dirs for completions
include(DefaultRPATHSettings)
include(DefineVersionVariables)


# Define options dependent on PROJECT_NAME
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
set(KIM_API_USER_CONFIGURATION_FILE ".${PROJECT_NAME}/${KIM_API_UID}/config" CACHE FILEPATH "Default configuration file name. If not absolute, then relative to user home directory")
mark_as_advanced(KIM_API_USER_CONFIGURATION_FILE)
set(KIM_API_USER_MODEL_DRIVERS_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_UID}/${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" CACHE PATH "Default user collection model drivers dir")
mark_as_advanced(KIM_API_USER_MODEL_DRIVERS_DIR_DEFAULT)
set(KIM_API_USER_PORTABLE_MODELS_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_UID}/${KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER}" CACHE PATH "Default user collection portable models dir")
mark_as_advanced(KIM_API_USER_PORTABLE_MODELS_DIR_DEFAULT)
set(KIM_API_USER_SIMULATOR_MODELS_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_UID}/${KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER}" CACHE PATH "Default user collection simulator models dir")
mark_as_advanced(KIM_API_USER_SIMULATOR_MODELS_DIR_DEFAULT)
#
if((NOT KIM_API_SYSTEM_MODEL_DRIVERS_DIR) OR (KIM_API_SYSTEM_MODEL_DRIVERS_DIR MATCHES "^${KIM_API_PATH_SEPARATOR}"))
  set(KIM_API_SYSTEM_MODEL_DRIVERS_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/${PROJECT_NAME}/${KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER}${KIM_API_SYSTEM_MODEL_DRIVERS_DIR}" CACHE STRING "System collection (${KIM_API_PATH_SEPARATOR}-separated) model drivers directory list" FORCE)
endif()
mark_as_advanced(KIM_API_SYSTEM_PORTABLE_MODEL_DRIVERS_DIR)
if((NOT KIM_API_SYSTEM_PORTABLE_MODELS_DIR) OR (KIM_API_SYSTEM_PORTABLE_MODELS_DIR MATCHES "^${KIM_API_PATH_SEPARATOR}"))
  set(KIM_API_SYSTEM_PORTABLE_MODELS_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/${PROJECT_NAME}/${KIM_API_PORTABLE_MODEL_PLURAL_IDENTIFIER}${KIM_API_SYSTEM_PORTABLE_MODELS_DIR}" CACHE STRING "System collection (${KIM_API_PATH_SEPARATOR}-separated) portable models directory list" FORCE)
endif()
mark_as_advanced(KIM_API_SYSTEM_MODELS_DIR)
if((NOT KIM_API_SYSTEM_SIMULATOR_MODELS_DIR) OR (KIM_API_SYSTEM_SIMULATOR_MODELS_DIR MATCHES "^${KIM_API_PATH_SEPARATOR}"))
  set(KIM_API_SYSTEM_SIMULATOR_MODELS_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/${PROJECT_NAME}/${KIM_API_SIMULATOR_MODEL_PLURAL_IDENTIFIER}${KIM_API_SYSTEM_SIMULATOR_MODELS_DIR}" CACHE STRING "System collection (${KIM_API_PATH_SEPARATOR}-separated) simulator models directory list" FORCE)
endif()
mark_as_advanced(KIM_API_SYSTEM_SIMULATOR_MODELS_DIR)

include(DefaultCompilerFlags)

if(MINGW)
  # On MinGW Windows platform, make <dlfcn.h> functions (dlopen, dlsym, etc.) available
  # using a third-party portability layer: https://github.com/dlfcn-win32/dlfcn-win32
  # It should be installed in the MSYS2 environment using the command:
  #   pacman -S --needed mingw-w64-x86_64-dlfcn
  set(CMAKE_DL_LIBS "dl")
endif()

# Define kim-api target
#
add_library(kim-api SHARED "")
set_target_properties(kim-api
  PROPERTIES
    OUTPUT_NAME "${PROJECT_NAME}"
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/Fortran_MODULES"
    )
target_include_directories(kim-api PUBLIC
  $<BUILD_INTERFACE:$<TARGET_PROPERTY:kim-api,Fortran_MODULE_DIRECTORY>>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_Fortran_MODULE_DIR_IDENTIFIER}>)
target_include_directories(kim-api PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>)
target_link_libraries(kim-api ${CMAKE_DL_LIBS})

# Add install rules for kim-api
#
install(TARGETS kim-api
  EXPORT KIM_API_Targets
  INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}"
  LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}")


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

# Add other subdirectories
#
add_subdirectory(cmake)
add_subdirectory(completions)
add_subdirectory(editors/emacs)
add_subdirectory(pkg-config)
add_subdirectory(utils)
if(${CMAKE_MINOR_VERSION} GREATER 8)
  add_subdirectory(docs)
else()
  add_custom_target(docs)
endif()

# Add Models & Drivers
#
if(KIM_API_BUILD_EXAMPLES)
  set(KIM-API_DIR "${CMAKE_BINARY_DIR}/cmake/${KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER}")
  set(KIM-API-ITEMS_DIR "${CMAKE_BINARY_DIR}/cmake/${KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER}-items")
  add_subdirectory(examples/model-drivers)
  add_subdirectory(examples/portable-models)
  add_subdirectory(examples/simulator-models)
  add_subdirectory(examples/simulators)
endif()
