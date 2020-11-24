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
#    Alexander Stukowski
#

#
# Release: This file is part of the kim-api.git repository.
#


# use 'lib' instead of '${CMAKE_SHARED_LIBRARY_PREFIX}' here to have consistency
# of pkg-config usage across systems.
set(PKG_CONFIG_FILE "lib${PROJECT_NAME}.pc")

relocatable_path(
  FROM "${CMAKE_INSTALL_FULL_LIBDIR}/${KIM_API_PKG_CONFIG_DIR_IDENTIFIER}" FROM_REL "${CMAKE_INSTALL_RELOC_LIBDIR}"
  TO "${CMAKE_INSTALL_PREFIX}" TO_REL "./"
  PREFIX "\${pcfiledir}"
  RESULT_VARIABLE _installdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_BINDIR}" TO_REL "${CMAKE_INSTALL_RELOC_BINDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _bindir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_LIBEXECDIR}" TO_REL "${CMAKE_INSTALL_RELOC_LIBEXECDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _libexecdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_SYSCONFDIR}" TO_REL "${CMAKE_INSTALL_RELOC_SYSCONFDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _sysconfdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_LIBDIR}" TO_REL "${CMAKE_INSTALL_RELOC_LIBDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _libdir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_INCLUDEDIR}" TO_REL "${CMAKE_INSTALL_RELOC_INCLUDEDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _includedir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_LIBDIR}/${PROJECT_NAME}/${KIM_API_Fortran_MODULE_DIR_IDENTIFIER}" TO_REL "${CMAKE_INSTALL_RELOC_LIBDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _moddir_rel
  )
relocatable_path(
  FROM "${CMAKE_INSTALL_PREFIX}" FROM_REL "./"
  TO "${CMAKE_INSTALL_FULL_DOCDIR}" TO_REL "${CMAKE_INSTALL_RELOC_DOCDIR}"
  PREFIX "\${prefix}"
  RESULT_VARIABLE _docdir_rel
  )
list(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES "${CMAKE_INSTALL_FULL_LIBDIR}" _isSystemDir)
if("${_isSystemDir}" STREQUAL "-1")
  set(KIM_API_RPATH_FLAG [=[-Wl,-rpath,${libdir}]=])
else()
  set(KIM_API_RPATH_FLAG "")
endif()
unset(_isSystemDir)
configure_file(libkim-api.pc.in ${PKG_CONFIG_FILE} @ONLY)
unset(_installdir_rel)
unset(_bindir_rel)
unset(_libexecdir_rel)
unset(_sysconfdir_rel)
unset(_libdir_rel)
unset(_includedir_rel)
unset(_moddir_rel)
unset(_docdir_rel)


install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PKG_CONFIG_FILE}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${CMAKE_INSTALL_RELOC_LIBDIR}/${KIM_API_PKG_CONFIG_DIR_IDENTIFIER}
  )
