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


include(CMakeParseArguments)
include(CMakePackageConfigHelpers)

function(_configure_common_package_components)
  set(_options "")
  set(_oneValueArgs
    INSTALL_TREE_DIR BUILD_TREE_DIR
    INSTALL_CONFIG_IN BUILD_CONFIG_IN CONFIG
    MACROS_IN MACROS
    VERSION_FILE VERSION_COMPATIBILITY
    )
  set(_multiValueArgs "")
  cmake_parse_arguments(_cpkg "${_options}" "${_oneValueArgs}" "${_multiValueArgs}" ${ARGN})
  if(_cpkg_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Unparsed arguments found in '_configure_common_pacakge_components'")
  endif()

  if(NOT CMAKE_INSTALL_RPATH)
    set(CMAKE_INSTALL_RPATH "")  # avoid uninitialized variable warning
  endif()

  # Create package files
  configure_file("${_cpkg_INSTALL_CONFIG_IN}" "${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_CONFIG}" @ONLY)
  configure_file("${_cpkg_MACROS_IN}" "${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_MACROS}" @ONLY)
  write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_VERSION_FILE}"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY ${_cpkg_VERSION_COMPATIBILITY}
    )
  # Package files for *build tree*
  configure_file("${_cpkg_BUILD_CONFIG_IN}" "${_cpkg_BUILD_TREE_DIR}/${_cpkg_CONFIG}" @ONLY)
  configure_file("${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_VERSION_FILE}" "${_cpkg_BUILD_TREE_DIR}/${_cpkg_VERSION_FILE}" COPYONLY)
  configure_file("${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_MACROS}" "${_cpkg_BUILD_TREE_DIR}/${_cpkg_MACROS}" COPYONLY)
  # Package files for *install tree*
  install(
    FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_CONFIG}"
    "${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_MACROS}"
    "${CMAKE_CURRENT_BINARY_DIR}/${_cpkg_VERSION_FILE}"
    DESTINATION
    "${_cpkg_INSTALL_TREE_DIR}"
    )
endfunction(_configure_common_package_components)


###############################################################################


set(_BUILD_TREE_CONFIG_DIR "${CMAKE_CURRENT_BINARY_DIR}/${KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER}")
set(_BUILD_TREE_ITEMS_CONFIG_DIR "${CMAKE_CURRENT_BINARY_DIR}/${KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER}-items")
# use CMAKE_INSTALL_FULL_* to get GNUInstallDir behavior
set(_INSTALL_TREE_CONFIG_DIR "${CMAKE_INSTALL_FULL_DATADIR}/cmake/${PROJECT_NAME}")
set(_INSTALL_TREE_ITEMS_CONFIG_DIR "${CMAKE_INSTALL_FULL_DATADIR}/cmake/${PROJECT_NAME}-items")


# KIM-API package
set(CONFIG_FILE_IN config.cmake.in)
set(CONFIG_FILE kim-api-config.cmake)  # should _not_ use PROJECT_NAME here
set(BUILD_TREE_CONFIG_FILE_IN build-tree-config.cmake.in)
set(MACROS_FILE_IN macros.cmake.in)
set(MACROS_FILE kim-api-macros.cmake)  # should _not_ use PROJECT_NAME here
set(TARGETS_FILE kim-api-targets.cmake)  # should _not_ use PROJECT_NAME here
set(CONFIG_VERSION_FILE kim-api-config-version.cmake)  # should _not_ use PROJECT_NAME here

_configure_common_package_components(
  INSTALL_TREE_DIR ${_INSTALL_TREE_CONFIG_DIR}
  BUILD_TREE_DIR ${_BUILD_TREE_CONFIG_DIR}
  INSTALL_CONFIG_IN ${CONFIG_FILE_IN}
  BUILD_CONFIG_IN ${BUILD_TREE_CONFIG_FILE_IN}
  CONFIG ${CONFIG_FILE}
  MACROS_IN ${MACROS_FILE_IN}
  MACROS ${MACROS_FILE}
  VERSION_FILE ${CONFIG_VERSION_FILE}
  VERSION_COMPATIBILITY SameMajorVersion
  )
# KIM-API Targets -- install tree only
install(EXPORT KIM_API_Targets
  FILE "${TARGETS_FILE}"
  NAMESPACE KIM-API::
  DESTINATION "${_INSTALL_TREE_CONFIG_DIR}"
)
# REMOVE THE BELOW LINES FOR 3.0.0 RELEASE!!!
set(PRE_2_2_COMPATIBILITY_FILE_IN pre-2.2-compatibility.cmake.in)  # REMOVE THIS LINE FOR 3.0.0 RELEASE!!!
set(PRE_2_2_COMPATIBILITY_FILE kim-api-pre-2.2-compatibility.cmake)  # REMOVE THIS LINE FOR 3.0.0 RELEASE!!!
configure_file("${CMAKE_CURRENT_SOURCE_DIR}/${PRE_2_2_COMPATIBILITY_FILE_IN}" "${CMAKE_CURRENT_BINARY_DIR}/${PRE_2_2_COMPATIBILITY_FILE}" @ONLY)  # REMOVE THIS LINE FOR 3.0.0 RELEASE!!!
configure_file("${CMAKE_CURRENT_BINARY_DIR}/${PRE_2_2_COMPATIBILITY_FILE}" "${_BUILD_TREE_CONFIG_DIR}/${PRE_2_2_COMPATIBILITY_FILE}" COPYONLY)  # REMOVE THIS LINE FOR 3.0.0 RELEASE!!!
install(  # REMOVE THESE LINES FOR 3.0.0 RELEASE!!!
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PRE_2_2_COMPATIBILITY_FILE}"
  DESTINATION
  "${_INSTALL_TREE_CONFIG_DIR}"
  )


# KIM-API-ITEMS package
set(ITEMS_CONFIG_FILE_IN items-config.cmake.in)
set(ITEMS_CONFIG_FILE kim-api-items-config.cmake)  # should _not_ use PROJECT_NAME here
set(BUILD_TREE_ITEMS_CONFIG_FILE_IN build-tree-items-config.cmake.in)
set(ITEMS_MACROS_FILE_IN items-macros.cmake.in)
set(ITEMS_MACROS_FILE kim-api-items-macros.cmake)  # should _not_ use PROJECT_NAME here
set(ITEMS_CONFIG_VERSION_FILE kim-api-items-config-version.cmake)  # should _not_ use PROJECT_NAME here

set(ITEM_COMPILED_WITH_VERSION_IN item-compiled-with-version.txt.in)
set(ITEM_COMPILED_WITH_VERSION item-compiled-with-version.txt)

set(ITEM_BUILD_SOURCES
  KIM_SharedLibrarySchema.hpp
  item-wrapper.cpp.in
  item-info.txt.in
  )

_configure_common_package_components(
  INSTALL_TREE_DIR ${_INSTALL_TREE_ITEMS_CONFIG_DIR}
  BUILD_TREE_DIR ${_BUILD_TREE_ITEMS_CONFIG_DIR}
  INSTALL_CONFIG_IN ${ITEMS_CONFIG_FILE_IN}
  BUILD_CONFIG_IN ${BUILD_TREE_ITEMS_CONFIG_FILE_IN}
  CONFIG ${ITEMS_CONFIG_FILE}
  MACROS_IN ${ITEMS_MACROS_FILE_IN}
  MACROS ${ITEMS_MACROS_FILE}
  VERSION_FILE ${ITEMS_CONFIG_VERSION_FILE}
  VERSION_COMPATIBILITY AnyNewerVersion  # allow newer major versions for KIM Items as convenience
  )
# KIM-API-ITEMS extra files
configure_file("${ITEM_COMPILED_WITH_VERSION_IN}" "${CMAKE_CURRENT_BINARY_DIR}/${ITEM_COMPILED_WITH_VERSION}" @ONLY)
foreach(_FL IN LISTS ITEM_BUILD_SOURCES)
  configure_file("${CMAKE_CURRENT_SOURCE_DIR}/${_FL}" "${_BUILD_TREE_ITEMS_CONFIG_DIR}/${_FL}" COPYONLY)
endforeach()
unset(_FL)
# build tree
configure_file("${CMAKE_CURRENT_BINARY_DIR}/${ITEM_COMPILED_WITH_VERSION}" "${_BUILD_TREE_ITEMS_CONFIG_DIR}/${ITEM_COMPILED_WITH_VERSION}" COPYONLY)
# install tree
install(
  FILES
  ${ITEM_BUILD_SOURCES}
  "${CMAKE_CURRENT_BINARY_DIR}/${ITEM_COMPILED_WITH_VERSION}"
  DESTINATION
  "${_INSTALL_TREE_ITEMS_CONFIG_DIR}"
  )



add_subdirectory(Modules)
