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


cmake_minimum_required(VERSION 3.4)
enable_testing()

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
list(APPEND CMAKE_PREFIX_PATH ${CMAKE_CURRENT_BINARY_DIR})

include(DefaultCompilerStandards)

# Define options
#
set(PROJECT_PREFIX "kim-api" CACHE STRING "Project prefix string")
mark_as_advanced(KIM_API_PROJECT_PREFIX)
#
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(LOG_MAX "DEBUG")
else()
  set(LOG_MAX "ERROR")
endif()
set(KIM_API_LOG_MAXIMUM_LEVEL "${LOG_MAX}" CACHE STRING "Maximum log verbosity")
set_property(CACHE KIM_API_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)
#
option(KIM_API_BUILD_MODELS_AND_DRIVERS "Build all included models, drivers, and simulators" OFF)
#
option(KIM_API_ENABLE_SANITIZE "Enable AddressSanitizer" OFF)
mark_as_advanced(KIM_API_ENABLE_SANITIZE)
#
option(KIM_API_ENABLE_COVERAGE "Enable code coverage" OFF)
mark_as_advanced(KIM_API_ENABLE_COVERAGE)
#
# option(KIM_API_DOWNLOAD_OPENKIM "TODO Download all models, model drivers and simulators" OFF)
#
# Additional options (that depend on call to project()) defined below


#
# Define main project
#
project(${PROJECT_PREFIX}-v2 VERSION 2.0.0 LANGUAGES CXX C Fortran)
include(GNUInstallDirs)  # needs to come after call to project()
include(DefaultRPATHSettings)
include(DefineVersionVariables)


# Define options dependent on PROJECT_NAME
#
set(KIM_API_USER_CONFIGURATION_FILE ".${PROJECT_NAME}/config" CACHE STRING "Default configuration file name.  If not absolute, then relative to user home directory")
mark_as_advanced(KIM_API_USER_CONFIGURATION_FILE)

include(DefineInternalVariables)

include(DefaultCompilerFlags)

# Define kim-api target
#
add_library(kim-api SHARED "")
set_target_properties(kim-api
  PROPERTIES
    OUTPUT_NAME "${PROJECT_NAME}"
    INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}"
    SOVERSION ${PROJECT_VERSION_MAJOR}
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/Fortran_MODULES"
    )
target_include_directories(kim-api PUBLIC $<TARGET_PROPERTY:kim-api,Fortran_MODULE_DIRECTORY>)
target_include_directories(kim-api PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(kim-api ${CMAKE_DL_LIBS})

# Add include subdirectories
#
add_subdirectory(cpp/include)
add_subdirectory(c/include)
add_subdirectory(fortran/src)

# Add src subdirectories
#
add_subdirectory(cpp/src)
add_subdirectory(c/src)
add_subdirectory(fortran/include)

# Add other subdirectories
#
add_subdirectory(cmake)
add_subdirectory(completions)
add_subdirectory(pkg-config)
add_subdirectory(utils)

if(${CMAKE_MINOR_VERSION} GREATER 8)
  add_subdirectory(docs)
endif()


# Add Models & Drivers, if appropriate
#
if(KIM_API_BUILD_MODELS_AND_DRIVERS)
  list(APPEND CMAKE_MODULE_PATH ${CMAKE_BINARY_DIR}/cmake/Modules)
  add_subdirectory(examples/models)
  add_subdirectory(examples/model-drivers)
  add_subdirectory(examples/simulators)
endif()


# # Add downloads, if appropriate
# #
# if(KIM_API_DOWNLOAD_OPENKIM)
#     # TODO insert full model/model_driver/simulator git repo or tarball here
#     set(GIT_REPOSITORY "https://github.com/openkim/kim-api.git")
#     set(GIT_BRANCH "cmake")
#
#     # TODO don't hardcode the directory "download"
#     if(EXISTS ${CMAKE_BINARY_DIR}/download)
#         execute_process(COMMAND git pull  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/download)
#     else()
#         execute_process(COMMAND git clone -b ${GIT_BRANCH} ${GIT_REPOSITORY} download WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
#     endif()
#
#     # TODO don't hardcode these directories
#     add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/models)
#     add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/model-drivers)
#     add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/simulators)
# endif()


# Add install rules for kim-api
#
install(TARGETS kim-api DESTINATION ${CMAKE_INSTALL_LIBDIR})
