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

# Language standards (also enforced with FLAGS below)
#
set(CMAKE_CXX_STANDARD 98)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS OFF)
#
set(CMAKE_C_STANDARD 90)
set(CMAKE_C_STANDARD_REQUIRED YES)
set(CMAKE_C_EXTENSIONS OFF)
#
# NO similar setting exist yet in CMake for Fortran


# Define options
#
set(PROJECT_PREFIX "kim-api" CACHE STRING "TODO add description here")
mark_as_advanced(KIM_API_PROJECT_PREFIX)
#
set(KIM_API_LOG_MAXIMUM_LEVEL "ERROR" CACHE STRING "TODO add description here")
set_property(CACHE KIM_API_LOG_MAXIMUM_LEVEL PROPERTY STRINGS SILENT FATAL ERROR WARNING INFORMATION DEBUG)
#
option(KIM_API_BUILD_MODELS_AND_DRIVERS "TODO Build model" OFF)
#
# option(KIM_API_DOWNLOAD_OPENKIM "TODO Download all models, model drivers and simulators" OFF)
#
# Additional options (that depend on call to project()) defined below


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


# Define options dependent on PROJECT_NAME
#
set(KIM_API_USER_CONFIGURATION_FILE ".${PROJECT_NAME}/config" CACHE STRING "TODO if starts with / absolute, otherwise homedir")
mark_as_advanced(KIM_API_USER_CONFIGURATION_FILE)
#
if (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
  option(KIM_API_ENABLE_COVERAGE "Enable code coverage" OFF)
  mark_as_advanced(KIM_API_ENABLE_COVERAGE)
endif()


# Define other internal variables
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}" KIM_API_C_ID_PROJECT_NAME)
set(KIM_API_C_ID_PROJECT_NAME "${KIM_API_C_ID_PROJECT_NAME}" CACHE INTERNAL "Canonical C-id for project name")
#
set(KIM_API_CMAKE_DIR_IDENTIFIER "cmake" CACHE INTERNAL "Canonical id for the CMake dir")
#
set(KIM_API_MODEL_DRIVER_IDENTIFIER "model-driver" CACHE INTERNAL "Canonical id for a model driver")
set(KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER "${KIM_API_MODEL_DRIVER_IDENTIFIER}s" CACHE INTERNAL "plural")
#
set(KIM_API_MODEL_IDENTIFIER "model" CACHE INTERNAL "Canonical id for a model")
set(KIM_API_MODEL_PLURAL_IDENTIFIER "${KIM_API_MODEL_IDENTIFIER}s" CACHE INTERNAL "plural")
#
set(KIM_API_PARAMETERIZED_MODEL_IDENTIFIER "parameterized-model" CACHE INTERNAL "Canonical id for a parameterized model")
#
set(KIM_API_SIMULATOR_MODEL_IDENTIFIER "simulator-model" CACHE INTERNAL "Canonical id for a simulator model")
#
set(KIM_API_STAND_ALONE_MODEL_IDENTIFIER "stand-alone-model" CACHE INTERNAL "Canonical id for a stand-alone model")
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_CONFIGURATION_FILE" KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
#
string(TOUPPER ${KIM_API_ENVIRONMENT_CONFIGURATION_FILE} KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
set(KIM_API_ENVIRONEMT_CONFIGURATION_FILE "${KIM_API_ENVIRONMENT_CONFIGURATION_FILE}" CACHE INTERNAL "Configuration file environment variable name")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_CMAKE_MODULES_DIR" KIM_API_ENVIRONMENT_CMAKE_MODULES_DIR)
string(TOUPPER ${KIM_API_ENVIRONMENT_CMAKE_MODULES_DIR} KIM_API_ENVIRONMENT_CMAKE_MODULES_DIR)
set(KIM_API_ENVIRONEMT_CMAKE_MODULES_DIR "${KIM_API_ENVIRONMENT_CMAKE_MODULES_DIR}" CACHE INTERNAL "CMake Modules dir environment variable name")
#
set(KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER "${KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Model drivers directory identifier")
set(KIM_API_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" CACHE INTERNAL "Default user collection model drivers dir")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
string(TOUPPER ${KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR} KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR "${KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR}" CACHE INTERNAL "Model drivers directory environment variable name")
#
set(KIM_API_MODEL_PLURAL_DIR_IDENTIFIER "${KIM_API_MODEL_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Models directory identifier")
set(KIM_API_USER_MODEL_PLURAL_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_MODEL_PLURAL_DIR_IDENTIFIER}" CACHE INTERNAL "Default user collection models dir")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_MODEL_PLURAL_DIR_IDENTIFIER}" KIM_API_ENVIRONMENT_MODEL_PLURAL_DIR)
string(TOUPPER ${KIM_API_ENVIRONMENT_MODEL_PLURAL_DIR} KIM_API_ENVIRONMENT_MODEL_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_MODEL_PLURAL_DIR "${KIM_API_ENVIRONMENT_MODEL_PLURAL_DIR}" CACHE INTERNAL "Models directory environment variable name")


# TODO figure out how to make these stick the first time...
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
# Set global compiler options
#
include(EnableCXXCompilerFlagIfSupported)
#enable_cxx_compiler_flag_if_supported("-std=c++${CMAKE_CXX_STANDARD}")
enable_cxx_compiler_flag_if_supported("-Wall")
#enable_cxx_compiler_flag_if_supported("-Wextra")
enable_cxx_compiler_flag_if_supported("-pedantic")
set(KIM_API_CXX_FLAGS "${KIM_API_CXX_FLAGS}" CACHE STRING "TODO add description")
set(CMAKE_CXX_FLAGS "${KIM_API_CXX_FLAGS} ${CMAKE_CXX_FLAGS}")
#
include(EnableCCompilerFlagIfSupported)
#enable_c_compiler_flag_if_supported("-std=c${CMAKE_C_STANDARD}")
enable_c_compiler_flag_if_supported("-Wall")
#enable_c_compiler_flag_if_supported("-Wextra")
enable_c_compiler_flag_if_supported("-pedantic")
set(KIM_API_C_FLAGS "${KIM_API_C_FLAGS}" CACHE STRING "TODO add description")
set(CMAKE_C_FLAGS "${KIM_API_C_FLAGS} ${CMAKE_C_FLAGS}")
#
include(EnableFortranCompilerFlagIfSupported)
#enable_fortran_compiler_flag_if_supported("-std=f2003")
enable_fortran_compiler_flag_if_supported("-Wall")
#enable_fortran_compiler_flag_if_supported("-Wextra")
enable_fortran_compiler_flag_if_supported("-Wimplicit-interface")
enable_fortran_compiler_flag_if_supported("-pedantic")
enable_fortran_compiler_flag_if_supported("-ffree-line-length-none")
set(KIM_API_Fortran_FLAGS "${KIM_API_Fortran_FLAGS}" CACHE STRING "TODO add description")
set(CMAKE_Fortran_FLAGS "${KIM_API_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS}")

#
if(KIM_API_ENABLE_COVERAGE)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} --coverage")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --coverage")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} --coverage")
endif()


# Define kim-api target
#
add_library(kim-api SHARED "")
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
add_subdirectory(cmake)
add_subdirectory(completions)
add_subdirectory(pkg-config)
add_subdirectory(utils)
#add_subdirectory(doc)


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
