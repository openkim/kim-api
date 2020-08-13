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
