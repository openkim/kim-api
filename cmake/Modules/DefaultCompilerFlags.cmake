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


# Set global compiler options
#
include(EnableCXXCompilerFlagIfSupported)
enable_cxx_compiler_flag_if_supported("-std=c++${CMAKE_CXX_STANDARD}")
enable_cxx_compiler_flag_if_supported("-Wall")
enable_cxx_compiler_flag_if_supported("-Wextra")
enable_cxx_compiler_flag_if_supported("-pedantic")
set(KIM_API_CXX_FLAGS "${KIM_API_CXX_FLAGS}" CACHE STRING "TODO add description")
set(CMAKE_CXX_FLAGS "${KIM_API_CXX_FLAGS} ${CMAKE_CXX_FLAGS}")
#
include(EnableCCompilerFlagIfSupported)
enable_c_compiler_flag_if_supported("-std=c${CMAKE_C_STANDARD}")
enable_c_compiler_flag_if_supported("-Wall")
enable_c_compiler_flag_if_supported("-Wextra")
enable_c_compiler_flag_if_supported("-pedantic")
set(KIM_API_C_FLAGS "${KIM_API_C_FLAGS}" CACHE STRING "TODO add description")
set(CMAKE_C_FLAGS "${KIM_API_C_FLAGS} ${CMAKE_C_FLAGS}")
#
include(EnableFortranCompilerFlagIfSupported)
enable_fortran_compiler_flag_if_supported("-std=f2003")
enable_fortran_compiler_flag_if_supported("-Wall")
enable_fortran_compiler_flag_if_supported("-Wextra")
enable_fortran_compiler_flag_if_supported("-Wimplicit-interface")
enable_fortran_compiler_flag_if_supported("-pedantic")
enable_fortran_compiler_flag_if_supported("-ffree-line-length-none")
set(KIM_API_Fortran_FLAGS "${KIM_API_Fortran_FLAGS}" CACHE STRING "TODO add description")
set(CMAKE_Fortran_FLAGS "${KIM_API_Fortran_FLAGS} ${CMAKE_Fortran_FLAGS}")

#
if(KIM_API_ENABLE_COVERAGE)
  set(KIM_API_C_FLAGS "${KIM_API_C_FLAGS} --coverage")
  set(KIM_API_CXX_FLAGS "${KIM_API_CXX_FLAGS} --coverage")
  set(KIM_API_Fortran_FLAGS "${KIM_API_Fortran_FLAGS} --coverage")
endif()
