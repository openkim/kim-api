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

ifeq ($(wildcard ../../Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include ../../Makefile.KIM_Config

INCLDS = KIM_LOG_MAXIMUM_LEVEL.inc

#
# Main build settings and rules
#
.PHONY: all includes

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += $(KIM_DIR)/cpp/include/Makefile

all: includes

includes: $(INCLDS)

KIM_LOG_MAXIMUM_LEVEL.inc: $(KIM_MAKE_FILES)
	@printf "Creating... include file....... $@.\n";
	$(QUELL)printf "#define KIM_LOG_MAXIMUM_LEVEL $(KIM_LOG_MAXIMUM_LEVEL)\n" > $@


#
# Main clean settings and rules
#
.PHONY: clean

clean:
	@printf "Cleaning... cpp/include.\n"
	$(QUELL)rm -f $(INCLDS)
