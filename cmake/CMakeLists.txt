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


set(CONFIG_FILE_IN config.cmake.in)
set(CONFIG_FILE ${PROJECT_NAME}-config.cmake)
set(CONFIG_VERSION_FILE_IN config-version.cmake.in)
set(CONFIG_VERSION_FILE ${PROJECT_NAME}-config-version.cmake)

configure_file("${CONFIG_FILE_IN}" "${CMAKE_CURRENT_BINARY_DIR}/${CONFIG_FILE}" @ONLY)
configure_file("${CONFIG_VERSION_FILE_IN}"
  "${CMAKE_CURRENT_BINARY_DIR}/${CONFIG_VERSION_FILE}" @ONLY)

install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${CONFIG_FILE}"
  "${CMAKE_CURRENT_BINARY_DIR}/${CONFIG_VERSION_FILE}"
  DESTINATION
  ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_CMAKE_DIR_IDENTIFIER}
  )

set(WRAPPER_SOURCES
  model-driver-wrapper.cpp.in
  parameterized-model-wrapper.cpp.in
  simulator-model-wrapper.cpp.in
  stand-alone-model-wrapper.cpp.in
  )

set(INFORMATION_FILES
  model-driver-info.txt.in
  parameterized-model-info.txt.in
  simulator-model-info.txt.in
  stand-alone-model-info.txt.in
  )

install(
  FILES ${WRAPPER_SOURCES} ${INFORMATION_FILES}
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_CMAKE_DIR_IDENTIFIER}
  )

add_subdirectory(Modules)
