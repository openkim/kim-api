#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
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
