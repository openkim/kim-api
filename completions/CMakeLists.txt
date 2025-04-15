#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#    Jim Madge
#    Yaser Afshar
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

set(BASH_COMPLETIONS ${PROJECT_NAME}-collections-management.bash)
set(ZSH_COMPLETIONS ${PROJECT_NAME}-collections-management.zsh)

relocatable_path(
  FROM "${BASH_COMPLETION_FULL_COMPLETIONSDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_SYSCONFDIR}"
  TO "${CMAKE_INSTALL_FULL_LIBEXECDIR}/${PROJECT_NAME}/${PROJECT_NAME}-collections-info" TO_REL "${CMAKE_INSTALL_RELOC_LIBEXECDIR}"
  PREFIX "\${ORIGIN}"
  RESULT_VARIABLE _collections_info_rel
  )
configure_file(collections-management.bash.in ${BASH_COMPLETIONS} @ONLY)
unset(_collections_info_rel)

relocatable_path(
  FROM "${ZSH_COMPLETION_FULL_COMPLETIONSDIR}" FROM_REL "${CMAKE_INSTALL_RELOC_SYSCONFDIR}"
  TO "${BASH_COMPLETION_FULL_COMPLETIONSDIR}/${BASH_COMPLETIONS}" TO_REL "${CMAKE_INSTALL_RELOC_SYSCONFDIR}"
  PREFIX "\${ORIGIN}"
  RESULT_VARIABLE _bashcomp_rel
  )
configure_file(collections-management.zsh.in ${ZSH_COMPLETIONS} @ONLY)
unset(_bashcomp_rel)


install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${BASH_COMPLETIONS}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${BASH_COMPLETION_RELOC_COMPLETIONSDIR}
  )

install(
  FILES
  ${CMAKE_CURRENT_BINARY_DIR}/${ZSH_COMPLETIONS}
  # use CMAKE_INSTALL_RELOC_* to get relocatable GNUInstallDir behavior
  DESTINATION ${ZSH_COMPLETION_RELOC_COMPLETIONSDIR}
  RENAME "_${PROJECT_NAME}-collections-management"
  )
