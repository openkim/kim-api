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
# Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Ellad B. Tadmor
#    Valeriu Smirichinski
#

#
# Release: This file is part of the openkim-api.git repository.
#


MODEL_NAME := ex_model_Al_PF_ErcolessiAdams

LOCALOBJ =

LOCALCLEAN =

# No changes should be needed below this line.

MODEL_BUILD_TARGET := $(strip $(MODEL_NAME)).a

ifndef KIM_API_DIR
   include $(KIM_DIR)/KIM_API/Include.mk
else
   include $(KIM_API_DIR)/Include.mk
endif


all: $(MODEL_BUILD_TARGET)

$(strip $(MODEL_NAME)).a: $(LOCALOBJ) $(strip $(MODEL_NAME)).o $(strip $(MODEL_NAME))_kim_str.o
	ar rcs $@ *.o

$(strip $(MODEL_NAME)).o: Makefile

Makefile: $(KIM_API_DIR)/Include.mk
	@touch Makefile

clean:
	rm -f *.o *.mod *.a *.so $(addprefix $(strip $(MODEL_NAME))_kim_str.,o cpp) $(LOCALCLEAN)
