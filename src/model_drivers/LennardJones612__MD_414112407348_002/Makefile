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
# Copyright (c) 2015, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Andrew Akerson
#


# load all basic KIM make configuration
include ../Makefile.KIM_Config

# set model driver specific details
MODEL_DRIVER_NAME := LennardJones612__MD_414112407348_002
MODEL_DRIVER_INIT_FUNCTION_NAME := model_driver_init
MODEL_DRIVER_INIT_FUNCTION_LANG := C++

LOCALOBJ = LennardJones612.o LennardJones612Implementation.o

LennardJones612.o: LennardJones612.hpp LennardJones612Implementation.hpp
LennardJones612Implementation.o: LennardJones612Implementation.hpp \
                                 LennardJones612ImplementationComputeDispatch.cpp
LennardJones612ImplementationComputeDispatch.cpp: CreateDispatch.sh
	@./CreateDispatch.sh
	@printf "Creating... $@.\n"

LOCALCLEAN = LennardJones612ImplementationComputeDispatch.cpp

# APPEND to compiler option flag lists
#FFLAGS   +=
#CFLAGS   +=
#CXXFLAGS +=
#LDFLAGS  +=

# load remaining KIM make configuration
include $(KIM_DIR)/$(builddir)/Makefile.ModelDriver
