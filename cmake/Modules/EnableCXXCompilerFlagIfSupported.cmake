#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
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


include(CheckCXXCompilerFlag)

function(enable_cxx_compiler_flag_if_supported _flag)
  if(NOT KIM_API_CXX_FLAGS)
    set(KIM_API_CXX_FLAGS "")
  endif()
  string(MAKE_C_IDENTIFIER "${_flag}" _cid_flag)
  string(FIND "${KIM_API_CXX_FLAGS}" "${_flag}" _flag_already_set)
  if(_flag_already_set EQUAL -1)
    check_cxx_compiler_flag("${_flag}" cxx_support_for_${_cid_flag})
    if(cxx_support_for_${_cid_flag})
      set(KIM_API_CXX_FLAGS "${KIM_API_CXX_FLAGS} ${_flag}" PARENT_SCOPE)
    endif()
    unset(cxx_support_for_${_cid_flag} CACHE)
  endif()
endfunction(enable_cxx_compiler_flag_if_supported)
