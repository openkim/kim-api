#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#
# SPDX-License-Identifier: LGPL-2.1-or-later
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

#
# Release: This file is part of the kim-api-2.4.1 package.
#


function(_write_kim_config_summary)
  set(_log_detailed_in "${CMAKE_CURRENT_LIST_DIR}/kim-api-configuration-detailed.log.in")
  set(_log_detailed "${PROJECT_BINARY_DIR}/kim-api-configuration-detailed.log")

  set(_log_summary_in "${CMAKE_CURRENT_LIST_DIR}/kim-api-configuration-summary.log.in")
  set(_log_summary "${PROJECT_BINARY_DIR}/kim-api-configuration-summary.log")

  if(EXISTS "${_log_summary}")
    file(SHA1 "${_log_summary}" _log_summary_sha1_before)
  else()
    set(_log_summary_sha1_before "")
  endif()
  set(UPDATE_WHEN_CMAKE_GREATER_THAN_3.12 "")  # avoid uninitialized variable warning
  # _log_detailed contains "CMAKE_*_CACHED_VALUE" references for cmake's (< 3.13) that don't have $CACHE{} ;; remove/change once min cmake is > 3.12
  configure_file("${_log_detailed_in}" "${_log_detailed}")
  # _log_summary contains "CMAKE_*_CACHED_VALUE" references for cmake's (< 3.13) that don't have $CACHE{} ;; remove/change once min cmake is > 3.12
  string(TOUPPER "${CMAKE_BUILD_TYPE}" _BUILD_TYPE_UPPER)  # used by _log_summary_in
  configure_file("${_log_summary_in}" "${_log_summary}")

  file(SHA1 "${_log_summary}" _log_summary_sha1_after)
  if(NOT "${_log_summary_sha1_after}" STREQUAL "${_log_summary_sha1_before}")
    file(READ "${_log_summary}" _log_content)
    message(STATUS "\n\n${_log_content}")
    message(STATUS "The above configuration information can also be found in:")
    message(STATUS "    ${_log_summary}")
    message(STATUS "A more detailed configuration listing can be found in:")
    message(STATUS "    ${_log_detailed}")
    message(STATUS "")
  endif()
endfunction(_write_kim_config_summary)

_write_kim_config_summary()
