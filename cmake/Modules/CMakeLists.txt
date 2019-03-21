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
# Release: This file is part of the kim-api.git repository.
#


set(FIND_FILE_IN FindKIM-API.cmake.in)  # should _not_ use PROJECT_NAME here
set(FIND_FILE FindKIM-API.cmake)  # should _not_ use PROJECT_NAME here
configure_file("${FIND_FILE_IN}" "${CMAKE_CURRENT_BINARY_DIR}/${FIND_FILE}" @ONLY)
install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/${FIND_FILE}"
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/${PROJECT_NAME}/${KIM_API_CMAKE_DIR_IDENTIFIER}/Modules
  )
