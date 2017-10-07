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
# Copyright (c) 2013--2017, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

ifneq (clean,$(MAKECMDGOALS))
  ifeq ($(wildcard ../Makefile.KIM_Config_Helper),)
    KIM_API_BUILD_CONFIG = kim-api-v2-build-config
  else
    include ../Makefile.KIM_Config_Helper
  endif
  ifeq ($(shell $(KIM_API_BUILD_CONFIG) --version 2> /dev/null),)
    $(error $(KIM_API_BUILD_CONFIG) utility is not available.  Something is wrong with your KIM API package setup)
  endif
endif

CC           = $(shell $(KIM_API_BUILD_CONFIG) --cc)
CXX          = $(shell $(KIM_API_BUILD_CONFIG) --cxx)
FC           = $(shell $(KIM_API_BUILD_CONFIG) --fc)
LD           = $(shell $(KIM_API_BUILD_CONFIG) --ld)
INCLUDES     = $(shell $(KIM_API_BUILD_CONFIG) --includes)
CFLAGS       = $(shell $(KIM_API_BUILD_CONFIG) --cflags)
CXXFLAGS     = $(shell $(KIM_API_BUILD_CONFIG) --cxxflags)
FFLAGS       = $(shell $(KIM_API_BUILD_CONFIG) --fflags)
LDFLAGS      = $(shell $(KIM_API_BUILD_CONFIG) --ldflags)
LDLIBS       = $(shell $(KIM_API_BUILD_CONFIG) --ldlibs)
XLANGLDLIBS  = $(shell $(KIM_API_BUILD_CONFIG) --xlangldlibs)
FNOMAINFLAG  = $(shell $(KIM_API_BUILD_CONFIG) --fnomainflag)
OBJONLYFLAG  = $(shell $(KIM_API_BUILD_CONFIG) --objonlyflag)
OUTPUTINFLAG = $(shell $(KIM_API_BUILD_CONFIG) --outputinflag)


.PHONY: all clean

TEST_NAME := vc_forces_numer_deriv

all: $(TEST_NAME)

$(TEST_NAME).o: $(TEST_NAME).F03 Makefile
	$(FC) $(INCLUDES) $(FFLAGS) $(OBJONLYFLAG) $<

$(TEST_NAME): $(TEST_NAME).o
	$(LD) $(LDFLAGS) $< $(LDLIBS) $(XLANGLDLIBS) $(OUTPUTINFLAG) $@

clean:
	rm -f $(TEST_NAME) $(TEST_NAME).o kim.log *.mod
