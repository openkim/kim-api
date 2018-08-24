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
# Copyright (c) 2013--2018, Regents of the University of Minnesota.
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


set(PKG_CONFIG_FILE "${CMAKE_SHARED_LIBRARY_PREFIX}${PROJECT_NAME}.pc")

configure_file(libkim-api.pc.in ${PKG_CONFIG_FILE} @ONLY)

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PKG_CONFIG_FILE}
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/${KIM_API_PKG_CONFIG_DIR_IDENTIFIER}
  )
