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
#    Jim Madge
#

#
# Release: This file is part of the kim-api.git repository.
#

# - bash-completionConfig
#
# Sets the install path for bash completions
find_package(bash-completion)
if(BASH_COMPLETION_FOUND)
  message(STATUS "Using bash completion dir ${CMAKE_INSTALL_PREFIX}${BASH_COMPLETION_COMPLETIONSDIR}")
else()
  set(BASH_COMPLETION_COMPLETIONSDIR "/etc/bash_completion.d")
  message(STATUS "Using fallback bash completion dir ${CMAKE_INSTALL_PREFIX}${BASH_COMPLETION_COMPLETIONSDIR}")
endif()
