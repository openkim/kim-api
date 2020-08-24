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
#    Jim Madge
#    Ryan S. Elliott
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
endif()
set_cache_with_fallback(BASH_COMPLETION_COMPLETIONSDIR "${CMAKE_INSTALL_SYSCONFDIR}/bash_completion.d" PATH "Directory where bash completions are installed")

# zsh completions
set_cache_with_fallback(ZSH_COMPLETION_COMPLETIONSDIR "${CMAKE_INSTALL_SYSCONFDIR}/zsh_completion.d" PATH "Directory where zsh completions are installed")
