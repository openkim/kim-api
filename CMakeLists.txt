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


cmake_minimum_required(VERSION 3.0)
enable_testing()


# Update paths
#
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/Modules)


# Define options
#
set(PROJECT_PREFIX "kim-api" CACHE STRING "TODO add description here")
mark_as_advanced(KIM_API_PROJECT_PREFIX)
#
set(KIM_API_LOG_MAXIMUM_LEVEL "ERROR" CACHE STRING "TODO add description here")
set_property(CACHE KIM_API_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)
#
option(KIM_API_BUILD_MODULES "TODO Build model" OFF)
#
option(KIM_API_DOWNLOAD_ALL "TODO Download all models, model drivers and simulators" OFF)
#
set(KIM_API_USER_CONFIGURATION_DIR ".${PROJECT_PREFIX}" CACHE STRING "TODO if starts with / absolute, otherwise homedir.")
mark_as_advanced(KIM_API_USER_CONFIGURATION_DIR)
#
set(KIM_API_MODEL_DRIVER_IDENTIFIER "model-driver" CACHE STRING "TODO add description here")
mark_as_advanced(KIM_API_MODEL_DRIVER_IDENTIFIER)
#
set(KIM_API_MODEL_IDENTIFIER "model" CACHE STRING "TODO add description here")
mark_as_advanced(KIM_API_MODEL_IDENTIFIER)



#
# Define main project
#
project(${PROJECT_PREFIX}-v2 VERSION 2.0.0 LANGUAGES CXX C Fortran)
include(GNUInstallDirs)  # needs to come after call to project()
#
# Define remaining version variables
#
set(PROJECT_VERSION_PRERELEASE "git" CACHE STRING "TODO ...")
set(PROJECT_VERSION_BUILD_METADATA "" CACHE STRING "TODO ...") #@@@ this should be updated at build time, not configuration time...
# TODO : translate next line for metadata :: http://xit0.org/2013/04/cmake-use-git-branch-and-commit-details-in-project/
# more complex: https://github.com/votca/tools/blob/master/CMakeModules/gitversion.cmake
# $(shell git rev-parse --short HEAD)$(shell if ! (git diff --quiet && git diff --cached --quiet); then printf ".dirty"; fi).$(KIM_COMPILERSUITE).$(KIM_SYSTEMLINKER).$(KIM_SYSTEMARCH).$(KIM_LINK)
set(PROJECT_VERSION_STRING "${PROJECT_VERSION}")
if(PROJECT_VERSION_PRERELEASE)
  set(PROJECT_VERSION_STRING
    "${PROJECT_VERSION_STRING}-${PROJECT_VERSION_PRERELEASE}")
endif()
if(PROJECT_VERSION_BUILD_METADATA)
  set(PROJECT_VERSION_STRING
    "${PROJECT_VERSION_STRING}+${PROJECT_VERSION_BUILD_METADATA}")
endif()


# Set global compiler options
#
include(CheckFortranCompilerFlag)
check_fortran_compiler_flag("-ffree-line-length-none" HAS_FREE_LINE_LENGTH_FLAG)
if(HAS_FREE_LINE_LENGTH_FLAG)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
endif()
#
if (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
  option(ENABLE_COVERAGE "Enable code coverage" OFF)
  mark_as_advanced(ENABLE_COVERAGE)
  if(ENABLE_COVERAGE)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} --coverage")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --coverage")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --coverage")
  endif()
endif()


# Define kim-api target
#
add_library(kim-api SHARED)
set_target_properties(kim-api
  PROPERTIES
    OUTPUT_NAME ${PROJECT_NAME}
    INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}"
    SOVERSION 2
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
add_subdirectory(completions)
add_subdirectory(pkg-config)
add_subdirectory(utilities)
add_subdirectory(documentation)


# Add Models & Drivers, if appropriate
if(KIM_API_BUILD_MODULES)
  add_subdirectory(examples/models)
  add_subdirectory(examples/model_drivers)
  add_subdirectory(examples/simulators)
endif()


# Add downloads, if appropriate
#
if(KIM_API_DOWNLOAD_ALL)
    # TODO insert full model/model_driver/simulator git repo or tarball here
    set(GIT_REPOSITORY "https://github.com/openkim/kim-api.git")
    set(GIT_BRANCH "cmake")

    # TODO don't hardcode the directory "download"
    if(EXISTS ${CMAKE_BINARY_DIR}/download)
        execute_process(COMMAND git pull  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/download)
    else()
        execute_process(COMMAND git clone -b ${GIT_BRANCH} ${GIT_REPOSITORY} download WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    endif()

    # TODO don't hardcode these directories
    add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/models)
    add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/model_drivers)
    add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/simulators)
endif()


# Add install rules for kim-api
#
install(TARGETS kim-api DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Add install of cmake files
#
install(DIRECTORY cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME})
