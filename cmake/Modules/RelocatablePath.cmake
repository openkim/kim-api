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
