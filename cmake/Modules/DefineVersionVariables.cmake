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


set(PROJECT_VERSION_PRERELEASE "git" CACHE STRING "Version prerelease value")

find_package(Git)
if((${GIT_FOUND}) AND (EXISTS "${CMAKE_SOURCE_DIR}/.git"))
  execute_process(COMMAND ${GIT_EXECUTABLE} update-index -q --refresh
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR} TIMEOUT 5 OUTPUT_QUIET
    ERROR_VARIABLE EXEC_ERR OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE KIM_API_GIT_COMMIT_ID OUTPUT_STRIP_TRAILING_WHITESPACE
    )
  execute_process(COMMAND ${GIT_EXECUTABLE} diff-index --name-only HEAD
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    OUTPUT_VARIABLE _HAS_CHANGES OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET
    )
  if(NOT "${_HAS_CHANGES}" STREQUAL "")
    set(KIM_API_GIT_COMMIT_ID "${KIM_API_GIT_COMMIT_ID}.dirty")
  endif()
  set(KIM_API_BUILD_METADATA "${KIM_API_GIT_COMMIT_ID}")

  set(RECONFIGURE_FILE "${CMAKE_BINARY_DIR}/reconfigure")
  execute_process(COMMAND ${CMAKE_COMMAND} -E touch "${RECONFIGURE_FILE}")
  add_custom_target(git-commit-id ALL
    COMMAND ${CMAKE_COMMAND} -E touch "${RECONFIGURE_FILE}"
    )
  include("${RECONFIGURE_FILE}")
endif()

if(NOT "${KIM_API_BUILD_METADATA}" STREQUAL "")
  string(APPEND KIM_API_BUILD_METADATA ".")
endif()
string(APPEND KIM_API_BUILD_METADATA "${CMAKE_CXX_COMPILER_ID}")
set(PROJECT_VERSION_BUILD_METADATA "${KIM_API_BUILD_METADATA}" CACHE STRING "Version metadata value" FORCE)


set(PROJECT_VERSION_STRING "${PROJECT_VERSION}")
if(PROJECT_VERSION_PRERELEASE)
  set(PROJECT_VERSION_STRING
    "${PROJECT_VERSION_STRING}-${PROJECT_VERSION_PRERELEASE}")
endif()
if(PROJECT_VERSION_BUILD_METADATA)
  set(PROJECT_VERSION_STRING
    "${PROJECT_VERSION_STRING}+${PROJECT_VERSION_BUILD_METADATA}")
endif()
