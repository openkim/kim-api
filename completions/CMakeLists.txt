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
#    Jim Madge
#    Yaser Afshar
#

#
# Release: This file is part of the kim-api-2.2.1 package.
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
