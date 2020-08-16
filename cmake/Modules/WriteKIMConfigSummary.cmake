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
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

set(_log_detailed_in "${CMAKE_CURRENT_LIST_DIR}/kim-api-configuration-detailed.log.in")
set(_log_detailed "${CMAKE_BINARY_DIR}/kim-api-configuration-detailed.log")

set(_log_summary_in "${CMAKE_CURRENT_LIST_DIR}/kim-api-configuration-summary.log.in")
set(_log_summary "${CMAKE_BINARY_DIR}/kim-api-configuration-summary.log")

if(EXISTS "${_log_summary}")
  file(SHA1 "${_log_summary}" _log_summary_sha1_before)
endif()
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
