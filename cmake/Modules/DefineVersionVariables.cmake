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
#

#
# Release: This file is part of the kim-api.git repository.
#


set(PROJECT_VERSION_PRERELEASE "git" CACHE STRING "Project SemVer prerelease string")  # cache to allow change from command line

find_package(Git)
if(${GIT_FOUND})
  execute_process(COMMAND ${GIT_EXECUTABLE} -C "${CMAKE_SOURCE_DIR}" rev-parse --show-toplevel
    OUTPUT_STRIP_TRAILING_WHITESPACE
    OUTPUT_VARIABLE _toplevel
    RESULT_VARIABLE _isGitRepo
    ERROR_QUIET
    )

  if((_isGitRepo EQUAL 0) AND ("${_toplevel}" STREQUAL "${CMAKE_SOURCE_DIR}"))
    # set configuration to depend on _depend_file; then touch depend with every invocation of make
    #
    # There is one flaw in this scheme.  The project will not be reconfigured
    # during the first execution of 'make', but instead will use the original
    # configuration results.  However, after the first 'make' is executed,
    # reconfiguration will occur with every additional 'make' execution.
    #
    # cmake ...
    # make  # no reconfigure; build
    # make  # reconfigure; build
    # ...
    #
    # This provides the possibility of getting incorrect results if one does:
    #
    # cmake ...
    # <edit files to change results of 'git describe'>
    # make  # no reconfigure, uses old 'git describe' results; build
    # make  # reconfigure, now used current 'git describe' results; build
    #
    # There seems to be no good way to avoid this problem.
    #
    set(_git_describe_sentinel "git-describe-config-sentinel")
    set(_depend_file "${CMAKE_CURRENT_BINARY_DIR}/${_git_describe_sentinel}-file")
    execute_process(COMMAND ${CMAKE_COMMAND} -E touch "${_depend_file}")
    # _depend_file must persist until configuraiton is finalized, or else the
    # below set_property command will have no effect.
    set_property(DIRECTORY "${CURRENT_SOURCE_DIR}" APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS "${_depend_file}")
    add_custom_target(${_git_describe_sentinel}-target ALL COMMAND ${CMAKE_COMMAND} -E touch "${_depend_file}")

    execute_process(COMMAND ${GIT_EXECUTABLE} -C "${CMAKE_SOURCE_DIR}" update-index -q --refresh
      TIMEOUT 5
      OUTPUT_QUIET
      ERROR_QUIET
      )
    if(READTHEDOCS)
      set(_DIRTY "")
    else()
      set(_DIRTY ".dirty")
    endif()
    execute_process(
      COMMAND ${GIT_EXECUTABLE} -C "${CMAKE_SOURCE_DIR}" describe --dirty=${_DIRTY} --broken=.broken --always
      OUTPUT_STRIP_TRAILING_WHITESPACE
      OUTPUT_VARIABLE _git_describe
      )
    set(_build_metadata "${_git_describe}")
  endif()
endif()

if(NOT "${_build_metadata}" STREQUAL "")
  string(APPEND _build_metadata ".")
endif()
string(APPEND _build_metadata "${CMAKE_CXX_COMPILER_ID}.${CMAKE_C_COMPILER_ID}.${CMAKE_Fortran_COMPILER_ID}")
set(PROJECT_VERSION_BUILD_METADATA "${_build_metadata}")  # do not cache


set(_version_string "${PROJECT_VERSION}")
if(PROJECT_VERSION_PRERELEASE)
  set(_version_string "${_version_string}-${PROJECT_VERSION_PRERELEASE}")
endif()
set(PROJECT_VERSION_STRING_WITHOUT_BUILD_METADATA "${_version_string}")  # used by pkg-config; do not cache
if(PROJECT_VERSION_BUILD_METADATA)
  set(_version_string "${_version_string}+${PROJECT_VERSION_BUILD_METADATA}")
endif()
set(PROJECT_VERSION_STRING "${_version_string}")  # do not cache
