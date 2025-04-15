#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Christoph Junghans
#    Ryan S. Elliott
#    Daniel S. Karls
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


include(RelocatablePath)

set(PORTABLE_MODEL_INFO    ${PROJECT_NAME}-portable-model-info)
set(SIMULATOR_MODEL        ${PROJECT_NAME}-simulator-model)
set(COLLECTIONS_INFO       ${PROJECT_NAME}-collections-info)
set(BASE64_ENCODE          ${PROJECT_NAME}-base64-encode)
set(COLLECTIONS_MANAGEMENT ${PROJECT_NAME}-collections-management)
set(SHARED_LIBRARY_TEST    ${PROJECT_NAME}-shared-library-test)
set(ACTIVATE_SCRIPT        ${PROJECT_NAME}-activate)
set(DEACTIVATE_SCRIPT      ${PROJECT_NAME}-deactivate)

# Set RPATH to support relocatable behavior for utilities
if((NOT ("${CMAKE_INSTALL_PREFIX}" IN_LIST KIM_API_STANDARD_INSTALL_PREFIXES))
    AND (NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Windows"))
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set(_rpath_prefix "@loader_path")
  else()
    set(_rpath_prefix "$ORIGIN")
  endif()
  relocatable_path(
    FROM "${CMAKE_INSTALL_FULL_LIBEXECDIR}/${PROJECT_NAME}" FROM_REL "${CMAKE_INSTALL_RELOC_LIBEXECDIR}"
    TO "${CMAKE_INSTALL_FULL_LIBDIR}" TO_REL "${CMAKE_INSTALL_RELOC_LIBDIR}"
    PREFIX "${_rpath_prefix}"
    RESULT_VARIABLE _rel_rpath
    )
  unset(_rpath_prefix)
  if(NOT IS_ABSOLUTE "${_rel_rpath}")
    list(APPEND CMAKE_INSTALL_RPATH "${_rel_rpath}")
  endif()
  unset(_rel_rpath)
endif()


if(NOT WIN32 OR CYGWIN)
  relocatable_path(
    FROM "${CMAKE_INSTALL_FULL_BINDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
    TO "${CMAKE_INSTALL_FULL_LIBEXECDIR}/${PROJECT_NAME}/${COLLECTIONS_INFO}" TO_REL "${CMAKE_INSTALL_RELOC_LIBEXECDIR}"
    PREFIX "\${ORIGIN}"
    RESULT_VARIABLE _collections_info_path
    )
else()
  # Note: On WIN32, 'collections-info.exe' and other utilities get installed under ${CMAKE_INSTALL_PREFIX}/bin/.
  relocatable_path(
    FROM "${CMAKE_INSTALL_FULL_BINDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
    TO "${CMAKE_INSTALL_FULL_BINDIR}/${COLLECTIONS_INFO}" TO_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
    PREFIX "\${ORIGIN}"
    RESULT_VARIABLE _collections_info_path
    )
endif()
configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT} @ONLY)
unset(_collections_info_path)

relocatable_path(
  FROM "${CMAKE_INSTALL_FULL_BINDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
  TO "${CMAKE_INSTALL_PREFIX}" TO_REL "./"
  PREFIX "\${ORIGIN}"
  RESULT_VARIABLE _installdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_FULL_BINDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
  TO "${CMAKE_INSTALL_FULL_LIBDIR}" TO_REL "${CMAKE_INSTALL_RELOC_LIBDIR}"
  PREFIX "\${ORIGIN}"
  RESULT_VARIABLE _libdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_FULL_BINDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
  TO "${BASH_COMPLETION_FULL_COMPLETIONSDIR}" TO_REL "${BASH_COMPLETION_RELOC_COMPLETIONSDIR}"
  PREFIX "\${ORIGIN}"
  RESULT_VARIABLE _bashcompdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_FULL_BINDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
  TO "${ZSH_COMPLETION_FULL_COMPLETIONSDIR}" TO_REL "${ZSH_COMPLETION_RELOC_COMPLETIONSDIR}"
  PREFIX "\${ORIGIN}"
  RESULT_VARIABLE _zshcompdir_rel
  )
configure_file(activate.in ${ACTIVATE_SCRIPT} @ONLY)
configure_file(deactivate.in ${DEACTIVATE_SCRIPT} @ONLY)
unset(_installdir_rel)
unset(_libdir_rel)
unset(_bashcompdir_rel)
unset(_zshcompdir_rel)

add_executable(portable-model-info portable-model-info.cpp)
target_link_libraries(portable-model-info kim-api)
target_include_directories(portable-model-info PRIVATE "${PROJECT_SOURCE_DIR}/cpp/src")
set_target_properties(portable-model-info PROPERTIES OUTPUT_NAME ${PORTABLE_MODEL_INFO})

add_executable(simulator-model simulator-model.cpp)
target_link_libraries(simulator-model kim-api)
set_target_properties(simulator-model PROPERTIES OUTPUT_NAME ${SIMULATOR_MODEL})

add_executable(collections-info collections-info.cpp)
target_include_directories(collections-info PRIVATE "${PROJECT_SOURCE_DIR}/cpp/src")
target_link_libraries(collections-info kim-api)
set_target_properties(collections-info PROPERTIES OUTPUT_NAME ${COLLECTIONS_INFO})

add_executable(base64-encode base64-encode.cpp ${PROJECT_SOURCE_DIR}/cpp/src/libb64/libb64/src/cencode.c)
target_include_directories(base64-encode PRIVATE "${PROJECT_BINARY_DIR}/cpp/include")
target_include_directories(base64-encode PRIVATE "${PROJECT_SOURCE_DIR}/cpp/src/libb64/libb64/include")
set_target_properties(base64-encode PROPERTIES OUTPUT_NAME ${BASE64_ENCODE})
target_compile_definitions(base64-encode PRIVATE BUFFERSIZE=${KIM_API_BASE64_BUFFERSIZE})

add_executable(shared-library-test shared-library-test.cpp)
target_include_directories(shared-library-test PRIVATE "${PROJECT_BINARY_DIR}/cpp/include")
target_link_libraries(shared-library-test ${CMAKE_DL_LIBS})
set_target_properties(shared-library-test PROPERTIES OUTPUT_NAME ${SHARED_LIBRARY_TEST})

# use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
if(NOT WIN32 OR CYGWIN)
  set(_dir "${CMAKE_INSTALL_RELOC_LIBEXECDIR}/${PROJECT_NAME}")
else()
  set(_dir "${CMAKE_INSTALL_RELOC_BINDIR}")
endif()
install(
  TARGETS portable-model-info simulator-model collections-info base64-encode shared-library-test
  EXPORT KIM_API_Targets
  DESTINATION "${_dir}"
  )
unset(_dir)

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${COLLECTIONS_MANAGEMENT}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_BINDIR}
  )

if(NOT ("${CMAKE_INSTALL_PREFIX}" IN_LIST KIM_API_STANDARD_INSTALL_PREFIXES))
  install(
    PROGRAMS
    ${CMAKE_CURRENT_BINARY_DIR}/${ACTIVATE_SCRIPT}
    ${CMAKE_CURRENT_BINARY_DIR}/${DEACTIVATE_SCRIPT}
    DESTINATION
    # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
    ${CMAKE_INSTALL_RELOC_BINDIR}
    )
endif()
