#
# KIM-API: An API for interatomic models
# Copyright (c) 2013--2022, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#    Alexander Stukowski
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


# Define other internal variables
#
set(KIM_API_UID "${PROJECT_VERSION_STRING}.${KIM_API_CONFIGURATION_TIMESTAMP}" CACHE INTERNAL "Unique ID value for kim-api installation")
#
if(WIN32 AND NOT CYGWIN)
  # Use ';' as path list separator on native Windows platform (including MinGW).
  set(KIM_API_PATH_SEPARATOR ";" CACHE INTERNAL "Canonical separator character used in path lists")
  set(KIM_API_STANDARD_INSTALL_PREFIXES "c:/Program Files/${PROJECT_NAME}" CACHE INTERNAL "Canonical list of standard install prefixes")
else()
  # Use ':' as path list separator on Linux/Unix platforms (including Cygwin and Windows Subsystem for Linux).
  set(KIM_API_PATH_SEPARATOR ":" CACHE INTERNAL "Canonical separator character used in path lists")
  set(KIM_API_STANDARD_INSTALL_PREFIXES "/" "/usr" "/usr/local" CACHE INTERNAL "Canonical list of standard install prefixes")
endif()
#
set(KIM_API_BUILD_TREE_CONFIG_DIR_IDENTIFIER "build-tree-package-config-files" CACHE INTERNAL "Cononical id for build tree config dir")
#
set(KIM_API_PKG_CONFIG_DIR_IDENTIFIER "pkgconfig" CACHE INTERNAL "Canonical id for pkg-config dir")
#
set(KIM_API_Fortran_MODULE_DIR_IDENTIFIER "mod" CACHE INTERNAL "Canonical id for the Fortran module dir")
#
set(KIM_API_MODEL_DRIVER_IDENTIFIER "model-driver" CACHE INTERNAL "Canonical id for a model driver")
set(KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER "${KIM_API_MODEL_DRIVER_IDENTIFIER}s" CACHE INTERNAL "plural")
#
set(KIM_API_PORTABLE_MODEL_IDENTIFIER "portable-model" CACHE INTERNAL "Canonical id for a portable model")
set(KIM_API_PORTABLE_MODEL_PLURAL_IDENTIFIER "${KIM_API_PORTABLE_MODEL_IDENTIFIER}s" CACHE INTERNAL "plural")
#
set(KIM_API_SIMULATOR_MODEL_IDENTIFIER "simulator-model" CACHE INTERNAL "Canonical id for a simulator model")
set(KIM_API_SIMULATOR_MODEL_PLURAL_IDENTIFIER "${KIM_API_SIMULATOR_MODEL_IDENTIFIER}s" CACHE INTERNAL "plural")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_CONFIGURATION_FILE" _KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
string(TOUPPER ${_KIM_API_ENVIRONMENT_CONFIGURATION_FILE} _KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
set(KIM_API_ENVIRONMENT_CONFIGURATION_FILE "${_KIM_API_ENVIRONMENT_CONFIGURATION_FILE}" CACHE INTERNAL "Configuration file environment variable name")
unset(_KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
#
string(MAKE_C_IDENTIFIER "KIM_API_CMAKE_PREFIX_DIR" _KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR)  # should _not_ use PROJECT_NAME here
string(TOUPPER ${_KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR} _KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR)
set(KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR "${_KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR}" CACHE INTERNAL "CMake prefix dir environment variable name")
unset(_KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR)
#
set(KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER "${KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Model drivers directory identifier")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" _KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
string(TOUPPER ${_KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR} _KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR "${_KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR}" CACHE INTERNAL "Model drivers directory environment variable name")
unset(_KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
#
set(KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER "${KIM_API_PORTABLE_MODEL_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Portable models directory identifier")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER}" _KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR)
string(TOUPPER ${_KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR} _KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR "${_KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR}" CACHE INTERNAL "Portable Models directory environment variable name")
unset(_KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR)
#
set(KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER "${KIM_API_SIMULATOR_MODEL_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Simulator Models directory identifier")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER}" _KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR)
string(TOUPPER ${_KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR} _KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR "${_KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR}" CACHE INTERNAL "Simulator Models directory environment variable name")
unset(_KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR)
