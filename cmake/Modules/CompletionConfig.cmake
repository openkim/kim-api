#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2021, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Jim Madge
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


# - CompletionConfig
#
# Sets the install paths for completions.
#
# If the user defines a location use it.  If installing to "standard" loc, use
# system bash-completion settings if available Otherwise, install into
# sysconfdir (but do not cache).


# bash completions
if((NOT BASH_COMPLETION_COMPLETIONSDIR) AND ("${CMAKE_INSTALL_PREFIX}" IN_LIST KIM_API_STANDARD_INSTALL_PREFIXES))
  find_package(bash-completion QUIET)  # sets BASH_COMPLETION_COMPLETIONSDIR
  if(bash-completion_FOUND)
    include(FindPackageMessage)
    find_package_message(bash-completion "Found bash-completion: (${BASH_COMPLETION_COMPLETIONSDIR})" "found")
  endif()
endif()
set_cache_with_fallback(BASH_COMPLETION_COMPLETIONSDIR "${CMAKE_INSTALL_RELOC_SYSCONFDIR}/bash_completion.d" PATH "Directory where bash completions are installed")
set(dir SYSCONFDIR)
GNUInstallDirs_get_absolute_install_dir(BASH_COMPLETION_FULL_COMPLETIONSDIR BASH_COMPLETION_COMPLETIONSDIR)
unset(dir)
RelocatableGNUInstallDirs_get_relocatable_dir(BASH_COMPLETION_RELOC_COMPLETIONSDIR BASH_COMPLETION_FULL_COMPLETIONSDIR BASH_COMPLETION_COMPLETIONSDIR)

# zsh completions
set_cache_with_fallback(ZSH_COMPLETION_COMPLETIONSDIR "${CMAKE_INSTALL_RELOC_SYSCONFDIR}/zsh_completion.d" PATH "Directory where zsh completions are installed")
set(dir SYSCONFDIR)
GNUInstallDirs_get_absolute_install_dir(ZSH_COMPLETION_FULL_COMPLETIONSDIR ZSH_COMPLETION_COMPLETIONSDIR)
unset(dir)
RelocatableGNUInstallDirs_get_relocatable_dir(ZSH_COMPLETION_RELOC_COMPLETIONSDIR ZSH_COMPLETION_FULL_COMPLETIONSDIR ZSH_COMPLETION_COMPLETIONSDIR)
