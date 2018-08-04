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

set(PROJECT_PREFIX "kim-api" CACHE STRING "TODO add description here")
mark_as_advanced(PROJECT_PREFIX)


project(${PROJECT_PREFIX}-v2 VERSION 2.0.0 LANGUAGES C CXX Fortran)
include(GNUInstallDirs)
enable_testing()

set(PROJECT_VERSION_PRERELEASE git)
set(PROJECT_VERSION_BUILD_METADATA)
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



if (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
  option(ENABLE_COVERAGE "Enable code coverage" OFF)
  mark_as_advanced(ENABLE_COVERAGE)
  if(ENABLE_COVERAGE)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} --coverage")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --coverage")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --coverage")
  endif()
endif()

include(CheckFortranCompilerFlag)
check_fortran_compiler_flag("-ffree-line-length-none" HAS_FREE_LINE_LENGTH_FLAG)
if(HAS_FREE_LINE_LENGTH_FLAG)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
endif()

# TODO : figure this RPATH stuff all out
#SET(CMAKE_SKIP_BUILD_RPATH FALSE)
#SET(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)
SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_FULL_LIBDIR}")
#SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)




set(USER_CONFIG_FILE_ROOT_NAME "$ENV{HOME}" CACHE STRING "TODO add description")
mark_as_advanced(USER_CONFIG_FILE_ROOT_NAME)
set(USER_CONFIG_FILE_DIR_NAME ".${PROJECT_PREFIX}" CACHE STRING "TODO add description")
mark_as_advanced(USER_CONFIG_FILE_DIR_NAME)


set(KIM_LOG_MAXIMUM_LEVEL "ERROR" CACHE STRING "TODO add description here")
set_property(CACHE KIM_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)

configure_file(cpp/include/KIM_LOG_DEFINES.inc.in KIM_LOG_DEFINES.inc)
configure_file(cpp/include/KIM_Version.hpp.in KIM_Version.hpp)
configure_file(c/include/KIM_Version.h.in KIM_Version.h)
configure_file(fortran/include/kim_version.fd.in kim_version.fd)

add_library(kim-api SHARED)
set_target_properties(kim-api PROPERTIES
  Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/Fortran_MODULES")
target_include_directories(kim-api PUBLIC $<TARGET_PROPERTY:kim-api,Fortran_MODULE_DIRECTORY>)
target_include_directories(kim-api PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(kim-api ${CMAKE_DL_LIBS})
set_target_properties(kim-api PROPERTIES OUTPUT_NAME ${PROJECT_NAME}
    INSTALL_NAME_DIR "${CMAKE_INSTALL_FULL_LIBDIR}" SOVERSION 2)

add_subdirectory(c/include)
add_subdirectory(c/src)
add_subdirectory(cpp/include)
add_subdirectory(cpp/src)
add_subdirectory(fortran/include)
add_subdirectory(fortran/src)

add_subdirectory(completion)
add_subdirectory(cpp/src/utils)
add_subdirectory(docs)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/Modules)
option(BUILD_MODULES "TODO Build model" OFF)
if(BUILD_MODULES)
  add_subdirectory(examples/models)
  add_subdirectory(examples/model_drivers)
  add_subdirectory(examples/simulators)
endif()

option(DOWNLOAD_ALL "TODO Download all models, model drivers and simulators" OFF)
if(DOWNLOAD_ALL)
    # TODO insert full model/model_driver/simulator git repo or tarball here
    set(GIT_REPOSITORY "https://github.com/openkim/kim-api.git")
    set(GIT_BRANCH "cmake")

    if(EXISTS ${CMAKE_BINARY_DIR}/download)
        execute_process(COMMAND git pull  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/download)
    else()
        execute_process(COMMAND git clone -b ${GIT_BRANCH} ${GIT_REPOSITORY} download WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    endif()

    add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/models)
    add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/model_drivers)
    add_subdirectory(${CMAKE_BINARY_DIR}/download/examples/simulators)
endif()

install(TARGETS kim-api DESTINATION ${CMAKE_INSTALL_LIBDIR})

install(DIRECTORY cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME})
