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
# Copyright (c) 2013--2019, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Richard Berger
#    Christoph Junghans
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api-2.1.2 package.
#


# Define other internal variables
#
set(KIM_API_STANDARD_INSTALL_PREFIXES "/" "/usr" "/usr/local" CACHE INTERNAL "Cononical list of standard install prefixes")
#
set(KIM_API_CMAKE_DIR_IDENTIFIER "cmake" CACHE INTERNAL "Canonical id for the CMake dir")
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
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_CONFIGURATION_FILE" KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
string(TOUPPER ${KIM_API_ENVIRONMENT_CONFIGURATION_FILE} KIM_API_ENVIRONMENT_CONFIGURATION_FILE)
set(KIM_API_ENVIRONEMT_CONFIGURATION_FILE "${KIM_API_ENVIRONMENT_CONFIGURATION_FILE}" CACHE INTERNAL "Configuration file environment variable name")
#
string(MAKE_C_IDENTIFIER "KIM_API_CMAKE_PREFIX_DIR" KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR)  # should _not_ use PROJECT_NAME here
string(TOUPPER ${KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR} KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR)
set(KIM_API_ENVIRONEMT_CMAKE_PREFIX_DIR "${KIM_API_ENVIRONMENT_CMAKE_PREFIX_DIR}" CACHE INTERNAL "CMake prefix dir environment variable name")
#
set(KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER "${KIM_API_MODEL_DRIVER_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Model drivers directory identifier")
set(KIM_API_USER_MODEL_DRIVER_PLURAL_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" CACHE INTERNAL "Default user collection model drivers dir")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_MODEL_DRIVER_PLURAL_DIR_IDENTIFIER}" KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
string(TOUPPER ${KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR} KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR "${KIM_API_ENVIRONMENT_MODEL_DRIVER_PLURAL_DIR}" CACHE INTERNAL "Model drivers directory environment variable name")
#
set(KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER "${KIM_API_PORTABLE_MODEL_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Portable models directory identifier")
set(KIM_API_USER_PORTABLE_MODEL_PLURAL_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER}" CACHE INTERNAL "Default user collection portable models dir")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_PORTABLE_MODEL_PLURAL_DIR_IDENTIFIER}" KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR)
string(TOUPPER ${KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR} KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR "${KIM_API_ENVIRONMENT_PORTABLE_MODEL_PLURAL_DIR}" CACHE INTERNAL "Portable Models directory environment variable name")
#
set(KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER "${KIM_API_SIMULATOR_MODEL_PLURAL_IDENTIFIER}-dir" CACHE INTERNAL "Simulator Models directory identifier")
set(KIM_API_USER_SIMULATOR_MODEL_PLURAL_DIR_DEFAULT "~/.${PROJECT_NAME}/${KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER}" CACHE INTERNAL "Default user collection simulator models dir")
#
string(MAKE_C_IDENTIFIER "${PROJECT_NAME}_${KIM_API_SIMULATOR_MODEL_PLURAL_DIR_IDENTIFIER}" KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR)
string(TOUPPER ${KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR} KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR)
set(KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR "${KIM_API_ENVIRONMENT_SIMULATOR_MODEL_PLURAL_DIR}" CACHE INTERNAL "Simulator Models directory environment variable name")
