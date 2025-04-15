#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#    Alexander Stukowski
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


# Set global compiler options
#
include(EnableCXXCompilerFlagIfSupported)
enable_cxx_compiler_flag_if_supported("-Wall")
enable_cxx_compiler_flag_if_supported("-Wextra")
enable_cxx_compiler_flag_if_supported("-pedantic")
if(KIM_API_ENABLE_COVERAGE)
  set(KIM_API_CXX_FLAGS "${KIM_API_CXX_FLAGS} --coverage")
endif()
if(KIM_API_ENABLE_SANITIZE)
  enable_cxx_compiler_flag_if_supported("-fsanitize=address")
endif()
string(STRIP "${KIM_API_CXX_FLAGS}" _s)
set(KIM_API_CXX_FLAGS "${_s}" CACHE STRING "KIM API C++ compiler flags")
unset(_s)
#
include(EnableCCompilerFlagIfSupported)
enable_c_compiler_flag_if_supported("-Wall")
enable_c_compiler_flag_if_supported("-Wextra")
enable_c_compiler_flag_if_supported("-pedantic")
if(KIM_API_ENABLE_COVERAGE)
  set(KIM_API_C_FLAGS "${KIM_API_C_FLAGS} --coverage")
endif()
if(KIM_API_ENABLE_SANITIZE)
  enable_c_compiler_flag_if_supported("-fsanitize=address")
endif()
string(STRIP "${KIM_API_C_FLAGS}" _s)
set(KIM_API_C_FLAGS "${_s}" CACHE STRING "KIM API C compiler flags")
unset(_s)
#
include(EnableFortranCompilerFlagIfSupported)
if(NOT CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  enable_fortran_compiler_flag_if_supported("-std=f2003")
  enable_fortran_compiler_flag_if_supported("-Wall")
  enable_fortran_compiler_flag_if_supported("-Wextra")
  enable_fortran_compiler_flag_if_supported("-Wimplicit-interface")
  enable_fortran_compiler_flag_if_supported("-pedantic")
else()
  enable_fortran_compiler_flag_if_supported("-stand f03")
  enable_fortran_compiler_flag_if_supported("-warn all")
  enable_fortran_compiler_flag_if_supported("-e03")
  enable_fortran_compiler_flag_if_supported("-warn interfaces")
  enable_fortran_compiler_flag_if_supported("-diag-disable 5462")  # disable "Global name too long" warning
endif()
if(KIM_API_ENABLE_COVERAGE)
  set(KIM_API_Fortran_FLAGS "${KIM_API_Fortran_FLAGS} --coverage")
endif()
if(KIM_API_ENABLE_SANITIZE)
  enable_fortran_compiler_flag_if_supported("-fsanitize=address")
endif()
string(STRIP "${KIM_API_Fortran_FLAGS}" _s)
set(KIM_API_Fortran_FLAGS "${_s}" CACHE STRING "KIM API Fortran compiler flags")
unset(_s)
#


# Set global linker flags
#
if(NOT KIM_API_EXE_LINKER_FLAGS)
  set(KIM_API_EXE_LINKER_FLAGS "")
endif()
if(KIM_API_ENABLE_SANITIZE)
  set(KIM_API_EXE_LINKER_FLAGS "${KIM_API_EXE_LINKER_FLAGS} -fsanitize=address")
endif()
if(KIM_API_ENABLE_COVERAGE)
  set(KIM_API_EXE_LINKER_FLAGS "${KIM_API_EXE_LINKER_FLAGS} --coverage")
endif()
string(STRIP "${KIM_API_EXE_LINKER_FLAGS}" _s)
set(KIM_API_EXE_LINKER_FLAGS "${_s}")
unset(_s)
#


# Update CMAKE variables
#
string(STRIP "${KIM_API_CXX_FLAGS} ${CMAKE_CXX_FLAGS}" _s)
set(CMAKE_CXX_FLAGS_CACHED_VALUE "${CMAKE_CXX_FLAGS}")  # for cmake's (< 3.13) that don't have $CACHE{} ;; remove once min cmake is > 3.12
set(CMAKE_CXX_FLAGS "${_s}")
unset(_s)
string(STRIP "${KIM_API_C_FLAGS} ${CMAKE_C_FLAGS}" _s)
set(CMAKE_C_FLAGS_CACHED_VALUE "${CMAKE_C_FLAGS}")  # for cmake's (< 3.13) that don't have $CACHE{} ;; remove once min cmake is > 3.12
set(CMAKE_C_FLAGS "${_s}")
unset(_s)
string(STRIP "${KIM_API_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS}" _s)
set(CMAKE_Fortran_FLAGS_CACHED_VALUE "${CMAKE_Fortran_FLAGS}")  # for cmake's (< 3.13) that don't have $CACHE{} ;; remove once min cmake is > 3.12
set(CMAKE_Fortran_FLAGS "${_s}")
unset(_s)
#
string(STRIP "${KIM_API_EXE_LINKER_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}" _s)
set(CMAKE_EXE_LINKER_FLAGS_CACHED_VALUE "${CMAKE_EXE_LINKER_FLAGS}")  # for cmake's (< 3.13) that don't have $CACHE{} ;; remove once min cmake is > 3.12
set(CMAKE_EXE_LINKER_FLAGS "${_s}")
unset(_s)
#
