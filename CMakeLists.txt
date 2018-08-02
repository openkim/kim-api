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

set(PACKAGE_NAME "kim-api" CACHE STRING "TODO add description here")
set(SANITIZED_PACKAGE_NAME "kim_api")

project(kim-api VERSION 2.0.0 LANGUAGES C CXX Fortran)
include(GNUInstallDirs)

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

set(KIM_VERSION_MAJOR ${PROJECT_VERSION_MAJOR})
set(KIM_VERSION_MINOR ${PROJECT_VERSION_MINOR})
set(KIM_VERSION_PATCH ${PROJECT_VERSION_PATCH})
set(KIM_VERSION_PRERELEASE git)
set(KIM_VERSION_BUILD_METADATA)
# TODO : translate next line for metadata :: http://xit0.org/2013/04/cmake-use-git-branch-and-commit-details-in-project/
# more complex: https://github.com/votca/tools/blob/master/CMakeModules/gitversion.cmake
# $(shell git rev-parse --short HEAD)$(shell if ! (git diff --quiet && git diff --cached --quiet); then printf ".dirty"; fi).$(KIM_COMPILERSUITE).$(KIM_SYSTEMLINKER).$(KIM_SYSTEMARCH).$(KIM_LINK)

set(KIM_VERSION_STRING "${KIM_VERSION_MAJOR}.${KIM_VERSION_MINOR}.${KIM_VERSION_PATCH}")

if(KIM_VERSION_PRERELEASE)
    set(KIM_VERSION_STRING "${KIM_VERSION_STRING}-${KIM_VERSION_PRERELEASE}")
endif()

if(KIM_VERSION_BUILD_METADATA)
    set(KIM_VERSION_STRING "${KIM_VERSION_STRING}+${KIM_VERSION_BUILD_METADATA}")
endif()

set(FULL_PACKAGE_NAME "${PACKAGE_NAME}-v${KIM_VERSION_MAJOR}")
set(PACKAGE_DIR "${CMAKE_INSTALL_FULL_LIBDIR}/${FULL_PACKAGE_NAME}")

set(LIB_ROOT       ${PACKAGE_NAME})
set(KIM_DIR        "${CMAKE_SOURCE_DIR}")
set(KIM_LIB        "${LIB_ROOT}-v${KIM_VERSION_MAJOR}")
set(KIM_LIB_BUILD  "${LIB_ROOT}-v${KIM_VERSION_STRING}")
set(MODEL_LIB       "${LIB_ROOT}-model-v${KIM_VERSION_MAJOR}")
set(MODEL_DRIVER_LIB "${LIB_ROOT}-model-driver-v${KIM_VERSION_MAJOR}")

set(MODELSDIR "models")
set(MODELDRIVERSDIR "model_drivers")

set(USER_CONFIG_FILE_ROOT_NAME "$ENV{HOME}" CACHE STRING "TODO add description")
set(USER_CONFIG_FILE_DIR_NAME ".${PACKAGE_NAME}" CACHE STRING "TODO add description")
mark_as_advanced(USER_CONFIG_FILE_ROOT_NAME)
mark_as_advanced(USER_CONFIG_FILE_DIR_NAME)

set(KIM_LOG_MAXIMUM_LEVEL "ERROR" CACHE STRING "TODO add description here")
set_property(CACHE KIM_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)

if (APPLE)
  set(SYSTEM_TYPE "darwin")
else ()
  set(SYSTEM_TYPE "linux")
endif ()
# add free-bsd here....

configure_file(cpp/include/KIM_LOG_DEFINES.inc.in KIM_LOG_DEFINES.inc)
configure_file(cpp/include/KIM_Version.hpp.in KIM_Version.hpp)
configure_file(c/include/KIM_Version.h.in KIM_Version.h)
configure_file(fortran/include/kim_version.fd.in kim_version.fd)

configure_file(Makefile.KIM_Config.in Makefile.KIM_Config)
configure_file(Makefile.Version.in Makefile.Version)

add_library(kim-api SHARED)
target_include_directories(kim-api PUBLIC ${CMAKE_BINARY_DIR})
target_link_libraries(kim-api ${CMAKE_DL_LIBS})
set_target_properties(kim-api PROPERTIES OUTPUT_NAME ${FULL_PACKAGE_NAME}
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

install(TARGETS kim-api DESTINATION ${CMAKE_INSTALL_LIBDIR})

install(FILES ${CMAKE_BINARY_DIR}/Makefile.KIM_Config DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME})
install(FILES ${CMAKE_BINARY_DIR}/Makefile.Version DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME})
install(DIRECTORY cmake DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME})

set(BUILD_SYSTEM
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.Generic
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.LoadDefaults
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.ModelDriver
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.ParameterizedModel
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.SanityCheck
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.SimulatorModel
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/Makefile.StandAloneModel
  )
install(FILES ${BUILD_SYSTEM} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME}/build_system)

set(BUILD_COMPILER
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/compiler_defaults/Makefile.GCC
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/compiler_defaults/Makefile.INTEL
  )
install(FILES ${BUILD_COMPILER} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME}/build_system/compiler_defaults)

set(BUILD_LINKER
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/linker_defaults/Makefile.DARWIN
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/linker_defaults/Makefile.FREEBSD
  ${CMAKE_CURRENT_SOURCE_DIR}/build_system/linker_defaults/Makefile.LINUX
  )
install(FILES ${BUILD_LINKER} DESTINATION ${CMAKE_INSTALL_LIBDIR}/${FULL_PACKAGE_NAME}/build_system/linker_defaults)
