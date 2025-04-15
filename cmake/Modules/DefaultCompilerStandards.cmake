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


# Language standards (also enforced with FLAGS below)
#
set(CMAKE_CXX_STANDARD 98 CACHE STRING "CXX language standard")  # cache to allow command line override
mark_as_advanced(CMAKE_CXX_STANDARD)
set(CMAKE_CXX_STANDARD_REQUIRED ON CACHE BOOL "CXX language standard required")  # cache to allow command line override
mark_as_advanced(CMAKE_CXX_STANDARD_REQUIRED)
set(CMAKE_CXX_EXTENSIONS OFF CACHE BOOL "CXX extensions")  # cache to allow command line override
mark_as_advanced(CMAKE_CXX_EXTENSIONS)
#
set(CMAKE_C_STANDARD 90 CACHE STRING "C language standard")  # cache to allow command line override
mark_as_advanced(CMAKE_C_STANDARD)
set(CMAKE_C_STANDARD_REQUIRED ON CACHE BOOL "C language standard required")  # cache to allow command line override
mark_as_advanced(CMAKE_C_STANDARD_REQUIRED)
set(CMAKE_C_EXTENSIONS OFF "C extensions")  # cache to allow command line override
mark_as_advanced(CMAKE_C_EXTENSIONS)
#
# NO similar setting exist yet in CMake for Fortran
