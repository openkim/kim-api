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


set(SIMULATOR_MODEL        ${PROJECT_NAME}-simulator-model)
set(COLLECTIONS_INFO       ${PROJECT_NAME}-collections-info)
set(COLLECTIONS_MANAGEMENT ${PROJECT_NAME}-collections-management)
set(SHARED_LIBRARY_TEST    ${PROJECT_NAME}-shared-library-test)
set(ACTIVATE_SCRIPT        ${PROJECT_NAME}-activate)
set(DEACTIVATE_SCRIPT      ${PROJECT_NAME}-deactivate)

# Set RPATH to support relocatable behavior for utilities
if((NOT ("${CMAKE_INSTALL_PREFIX}" IN_LIST KIM_API_STANDARD_INSTALL_PREFIXES))
    AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    AND (NOT IS_ABSOLUTE "${CMAKE_INSTALL_LIBDIR}")
    AND (NOT IS_ABSOLUTE "${CMAKE_INSTALL_LIBEXECDIR}"))
  # determine location of LIBDIR relative to LIBEXECDIR/PROJECT_NAME
  string(REGEX REPLACE "[^/]+" ".." _rel_dir "${CMAKE_INSTALL_LIBEXECDIR}/${PROJECT_NAME}")
  set(_rel_rpath "${_rel_dir}/${CMAKE_INSTALL_LIBDIR}")
  unset(_rel_dir)
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    list(APPEND CMAKE_INSTALL_RPATH "@loader_path/${_rel_rpath}")
  else()
    list(APPEND CMAKE_INSTALL_RPATH "$ORIGIN/${_rel_rpath}")
  endif()
  unset(_rel_rpath)
endif()

if((NOT IS_ABSOLUTE "${CMAKE_INSTALL_BINDIR}")
    AND (NOT IS_ABSOLUTE "${CMAKE_INSTALL_LIBEXECDIR}"))
  string(REGEX REPLACE "[^/]+" ".." _rel_dir "${CMAKE_INSTALL_BINDIR}")
  set(_collections_info_path "\${ORIGIN}/${_rel_dir}/${CMAKE_INSTALL_LIBEXECDIR}/${PROJECT_NAME}/${COLLECTIONS_INFO}")
  unset(_rel_dir)
else()
  set(_collections_info_path "${CMAKE_INSTALL_FULL_LIBEXECDIR}/${PROJECT_NAME}/${COLLECTIONS_INFO}")
endif()
#
configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT} @ONLY)
unset(_collections_info_path)
configure_file(activate.in ${ACTIVATE_SCRIPT} @ONLY)
configure_file(deactivate.in ${DEACTIVATE_SCRIPT} @ONLY)

add_executable(simulator-model simulator-model.cpp)
target_link_libraries(simulator-model kim-api)
set_target_properties(simulator-model PROPERTIES OUTPUT_NAME ${SIMULATOR_MODEL})

add_executable(collections-info collections-info.cpp)
target_include_directories(collections-info PRIVATE "${PROJECT_SOURCE_DIR}/cpp/src/")
target_link_libraries(collections-info kim-api)
set_target_properties(collections-info PROPERTIES OUTPUT_NAME ${COLLECTIONS_INFO})

add_executable(shared-library-test shared-library-test.cpp)
target_include_directories(shared-library-test PRIVATE "${PROJECT_BINARY_DIR}/cpp/include")
target_link_libraries(shared-library-test ${CMAKE_DL_LIBS})
set_target_properties(shared-library-test PROPERTIES OUTPUT_NAME ${SHARED_LIBRARY_TEST})

# use CMAKE_INSTALL_FULL_* to get GNUInstallDir behavior
if(NOT WIN32 OR CYGWIN)
  set(_dir "${CMAKE_INSTALL_FULL_LIBEXECDIR}/${PROJECT_NAME}")
else()
  set(_dir "${CMAKE_INSTALL_FULL_BINDIR}")
endif()
install(
  TARGETS simulator-model collections-info shared-library-test
  EXPORT KIM_API_Targets
  DESTINATION "${_dir}"
  )
unset(_dir)

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${COLLECTIONS_MANAGEMENT}
  # use CMAKE_INSTALL_FULL_* to get GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}
  )

if(NOT ("${CMAKE_INSTALL_PREFIX}" IN_LIST KIM_API_STANDARD_INSTALL_PREFIXES))
  install(
    PROGRAMS
    ${CMAKE_CURRENT_BINARY_DIR}/${ACTIVATE_SCRIPT}
    ${CMAKE_CURRENT_BINARY_DIR}/${DEACTIVATE_SCRIPT}
    DESTINATION
    # use CMAKE_INSTALL_FULL_* to get GNUInstallDir behavior
    ${CMAKE_INSTALL_FULL_BINDIR}
    )
endif()
