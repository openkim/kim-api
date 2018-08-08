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


set(SIMULATOR_MODEL        ${PROJECT_NAME}-simulator-model)
set(COLLECTIONS_INFO       ${PROJECT_NAME}-collections-info)
set(COLLECTIONS_MANAGEMENT ${PROJECT_NAME}-collections-management)
set(ACTIVATE_SCRIPT        ${PROJECT_NAME}-activate)
set(DEACTIVATE_SCRIPT      ${PROJECT_NAME}-deactivate)

configure_file(collections-management.in ${COLLECTIONS_MANAGEMENT} @ONLY)
configure_file(activate.in ${ACTIVATE_SCRIPT} @ONLY)
configure_file(deactivate.in ${DEACTIVATE_SCRIPT} @ONLY)

add_executable(simulator-model simulator-model.cpp)
target_link_libraries(simulator-model kim-loader)
set_target_properties(simulator-model PROPERTIES OUTPUT_NAME ${SIMULATOR_MODEL})

add_executable(collections-info collections-info.cpp)
target_link_libraries(collections-info kim-loader)
set_target_properties(collections-info PROPERTIES OUTPUT_NAME ${COLLECTIONS_INFO})

install(
  TARGETS simulator-model collections-info
  DESTINATION ${CMAKE_INSTALL_LIBEXECDIR}/${PROJECT_NAME}
  )

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/${ACTIVATE_SCRIPT}
           ${CMAKE_CURRENT_BINARY_DIR}/${COLLECTIONS_MANAGEMENT}
           ${CMAKE_CURRENT_BINARY_DIR}/${DEACTIVATE_SCRIPT}
  DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
