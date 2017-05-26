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
#    Ellad B. Tadmor
#    Valeriu Smirichinski
#

# load all basic KIM make configuration
ifeq ($(wildcard ../Makefile.KIM_Config),)
  ifeq ($(wildcard ../Makefile.KIM_Config_Helper),)
    KIM_CONFIG_HELPER = kim-api-build-config
  else
    include ../Makefile.KIM_Config_Helper
  endif
  ifeq ($(shell $(KIM_CONFIG_HELPER) --version 2> /dev/null),)
    $(error ../Makefile.KIM_Config does not exist and $(KIM_CONFIG_HELPER) utility is not available.  Something is wrong with your KIM API package setup)
  else
    MASTER_CONFIG = $(shell $(KIM_CONFIG_HELPER) --master-config)
  endif
else
  MASTER_CONFIG = ../Makefile.KIM_Config
endif
include $(MASTER_CONFIG)

# set model driver specific details
MODEL_DRIVER_NAME := ex_model_driver_P_Morse
MODEL_DRIVER_INIT_FUNCTION_NAME := model_driver_init
MODEL_DRIVER_INIT_FUNCTION_LANG := C

LOCALOBJ = ex_model_driver_P_Morse.o

LOCALCLEAN =

# APPEND to compiler option flag lists
#FFLAGS   +=
#CFLAGS   +=
#CXXFLAGS +=
#LDFLAGS  +=
#LDLIBS   +=

# load remaining KIM make configuration
include $(KIM_DIR)/$(builddir)/Makefile.ModelDriver
