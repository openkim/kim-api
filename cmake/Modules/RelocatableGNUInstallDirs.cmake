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
# Copyright (c) 2020--2020, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#


macro(RelocatableGNUInstallDirs_get_relocatable_dir relocvar absvar var)
  if("${${absvar}}" STREQUAL "${CMAKE_INSTALL_PREFIX}/${${var}}")
    set(${relocvar} "${${var}}")
  else()
    set(${relocvar} "${${absvar}}")
  endif()
endmacro(RelocatableGNUInstallDirs_get_relocatable_dir)

# Result directories
#
foreach(dir
    BINDIR
    SBINDIR
    LIBEXECDIR
    SYSCONFDIR
    SHAREDSTATEDIR
    LOCALSTATEDIR
    RUNSTATEDIR
    LIBDIR
    INCLUDEDIR
    OLDINCLUDEDIR
    DATAROOTDIR
    DATADIR
    INFODIR
    LOCALEDIR
    MANDIR
    DOCDIR
    )
  RelocatableGNUInstallDirs_get_relocatable_dir(CMAKE_INSTALL_RELOC_${dir} CMAKE_INSTALL_FULL_${dir} CMAKE_INSTALL_${dir})
endforeach()
