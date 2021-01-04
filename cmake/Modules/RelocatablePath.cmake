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
# Copyright (c) 2020--2021, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


# relocatable_path() -- Construct a path "TO" one absolute filepath "FROM"
# another absolute path if their respective "TO_REL" and "FROM_REL" paths are
# both relative.  Supports an optional RPATH_PREFIX to add any necessary
# leading rpath variable name.  If either of "TO_REL" or "FROM_REL" are
# absolute, just return the absolute "FROM" filepath.

function(relocatable_path)
  set(_options "")
  set(_oneValueArgs RESULT_VARIABLE FROM FROM_REL TO TO_REL PREFIX)
  set(_multiValueArgs "")
  cmake_parse_arguments(_rel "${_options}" "${_oneValueArgs}" "${_multiValueArgs}" ${ARGN})
  if(_rel_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unparsed arguments found in 'relocatable_path()'")
  endif()

  if(NOT IS_ABSOLUTE "${_rel_FROM}")
    message(FATAL_ERROR "FROM must be an absolute path in 'relocatable_path()'")
  endif()
  if(NOT IS_ABSOLUTE "${_rel_TO}")
    message(FATAL_ERROR "TO must be an absolute filepath in 'relocatable_path()'")
  endif()

  if(NOT _rel_RESULT_VARIABLE)
    message(FATAL_ERROR "RESULT_VARIABLE not provided in 'relocatable_path()'")
  endif()

  if(NOT _rel_PREFIX)
    set(_rel_PREFIX "")
  else()
    set(_rel_PREFIX "${_rel_PREFIX}/")
  endif()

  if((NOT IS_ABSOLUTE "${_rel_FROM_REL}") AND (NOT IS_ABSOLUTE "${_rel_TO_REL}"))
    file(RELATIVE_PATH _rel_rel "${_rel_FROM}" "${_rel_TO}")
    set(${_rel_RESULT_VARIABLE} "${_rel_PREFIX}${_rel_rel}" PARENT_SCOPE)
  else()
    set(${_rel_RESULT_VARIABLE} "${_rel_TO}" PARENT_SCOPE)
  endif()
endfunction(relocatable_path)
