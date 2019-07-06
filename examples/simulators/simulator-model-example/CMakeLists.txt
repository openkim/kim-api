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
# Copyright (c) 2013--2019, Regents of the University of Minnesota.
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


#
# Required preamble
#

cmake_minimum_required(VERSION 3.4)

list(APPEND CMAKE_PREFIX_PATH $ENV{KIM_API_CMAKE_PREFIX_DIR})
find_package(KIM-API 2.0 REQUIRED CONFIG)
if(NOT TARGET kim-api)
  enable_testing()
  project("${KIM_API_PROJECT_NAME}" VERSION "${KIM_API_VERSION}"
    LANGUAGES CXX C Fortran)
  add_kim_api_imported_library()
  add_kim_api_compiler_flags()
endif()

# End preamble


set(TEST_NAME "simulator-model-example")

add_executable(${TEST_NAME} ${TEST_NAME}.cpp)
target_link_libraries(${TEST_NAME} kim-api)

add_executable(${TEST_NAME}-c ${TEST_NAME}-c.cpp)
target_link_libraries(${TEST_NAME}-c kim-api)

add_executable(${TEST_NAME}-fortran ${TEST_NAME}-fortran.f90)
target_link_libraries(${TEST_NAME}-fortran kim-api)
target_compile_options(${TEST_NAME}-fortran PRIVATE "-std=gnu")
