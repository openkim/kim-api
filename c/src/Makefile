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

VPATH = ../include ../../cpp/include
KIMINCLUDEFLAGS = -I../include -I../../cpp/include

# Definition of c object files
OBJC = KIM_ArgumentName_c.o \
       KIM_CallbackName_c.o \
       KIM_ChargeUnit_c.o \
       KIM_DataType_c.o \
       KIM_EnergyUnit_c.o \
       KIM_LanguageName_c.o \
       KIM_LengthUnit_c.o \
       KIM_LogVerbosity_c.o \
       KIM_Log_c.o \
       KIM_ModelCompute_c.o \
       KIM_ModelCreate_c.o \
       KIM_ModelDestroy_c.o \
       KIM_ModelDriverCreate_c.o \
       KIM_ModelRefresh_c.o \
       KIM_Model_c.o \
       KIM_Numbering_c.o \
       KIM_SpeciesName_c.o \
       KIM_SupportStatus_c.o \
       KIM_TemperatureUnit_c.o \
       KIM_TimeUnit_c.o \
       KIM_semver_c.o

#
# Main build settings and rules
#
.PHONY: all objects lib

all: objects lib

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += $(KIM_DIR)/c/include/Makefile.dependencies \
                  $(KIM_DIR)/cpp/include/Makefile.dependencies \
                  $(KIM_DIR)/c/src/Makefile

all: objects lib

objects: $(OBJC)
lib: lib-c-$(KIM_LIB_BUILD).a

lib-c-$(KIM_LIB_BUILD).a: $(OBJC)
	@printf "Creating... C KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(OBJC)

# Include file dependency lists
include ../include/Makefile.dependencies
include ../../cpp/include/Makefile.dependencies

# Source file dependencies
KIM_ArgumentName_c.o: KIM_ArgumentName_c.cpp $(KIM_DATA_TYPE_C) $(KIM_DATA_TYPE_CPP) $(KIM_ARGUMENT_NAME_C) $(KIM_ARGUMENT_NAME_CPP) $(KIM_MAKE_FILES)
KIM_CallbackName_c.o: KIM_CallbackName_c.cpp $(KIM_CALLBACK_NAME_C) $(KIM_CALLBACK_NAME_CPP) $(KIM_MAKE_FILES)
KIM_ChargeUnit_c.o: KIM_ChargeUnit_c.cpp $(KIM_CHARGE_UNIT_C) $(KIM_CHARGE_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_DataType_c.o: KIM_DataType_c.cpp $(KIM_DATA_TYPE_C) $(KIM_DATA_TYPE_CPP) $(KIM_MAKE_FILES)
KIM_EnergyUnit_c.o: KIM_EnergyUnit_c.cpp $(KIM_ENERGY_UNIT_C) $(KIM_ENERGY_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_LanguageName_c.o: KIM_LanguageName_c.cpp $(KIM_LANGUAGE_NAME_C) $(KIM_LANGUAGE_NAME_CPP) $(KIM_MAKE_FILES)
KIM_LengthUnit_c.o: KIM_LengthUnit_c.cpp $(KIM_LENGTH_UNIT_C) $(KIM_LENGTH_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_LogVerbosity_c.o: KIM_LogVerbosity_c.cpp $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_Log_c.o: KIM_Log_c.cpp $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) $(KIM_LOG_C) $(KIM_LOG_CPP) $(KIM_MAKE_FILES)
KIM_ModelCompute_c.o: KIM_ModelCompute_c.cpp $(KIM_MODEL_COMPUTE_C) $(KIM_MODEL_COMPUTE_CPP) \
                      $(KIM_CALLBACK_NAME_C) $(KIM_CALLBACK_NAME_CPP) $(KIM_ARGUMENT_NAME_C) $(KIM_ARGUMENT_NAME_CPP) \
                      $(KIM_LOG_VEBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_ModelCreate_c.o: KIM_ModelCreate_c.cpp $(KIM_MODEL_CREATE_C) $(KIM_MODEL_CREATE_CPP) \
                     $(KIM_CALLBACK_NAME_C) $(KIM_CALLBACK_NAME_CPP) $(KIM_ARGUMENT_NAME_C) $(KIM_ARGUMENT_NAME_CPP) \
                     $(KIM_SUPPORT_STATUS_C) $(KIM_SUPPORT_STATUS_CPP) $(KIM_UNIT_SYSTEM_C) $(KIM_UNIT_SYSTEM_CPP) \
                     $(KIM_NUMBERING_C) $(KIM_NUMBERING_CPP) $(KIM_LANGUAGE_NAME_C) $(KIM_LANGUAGE_NAME_CPP) \
                     $(KIM_SPECIES_NAME_C) $(KIM_SPECIES_NAME_CPP) $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) \
                     $(KIM_MAKE_FILES)
KIM_ModelDestroy_c.o: KIM_ModelDestroy_c.cpp $(KIM_MODEL_DESTROY_C) $(KIM_MODEL_DESTROY_CPP) $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_ModelDriverCreate_c.o: KIM_ModelDriverCreate_c.cpp $(KIM_MODEL_DRIVER_CREATE_C) $(KIM_MODEL_DRIVER_CREATE_CPP) \
                     $(KIM_CALLBACK_NAME_C) $(KIM_CALLBACK_NAME_CPP) $(KIM_ARGUMENT_NAME_C) $(KIM_ARGUMENT_NAME_CPP) \
                     $(KIM_SUPPORT_STATUS_C) $(KIM_SUPPORT_STATUS_CPP) $(KIM_UNIT_SYSTEM_C) $(KIM_UNIT_SYSTEM_CPP) \
                     $(KIM_NUMBERING_C) $(KIM_NUMBERING_CPP) $(KIM_LANGUAGE_NAME_C) $(KIM_LANGUAGE_NAME_CPP) \
                     $(KIM_SPECIES_NAME_C) $(KIM_SPECIES_NAME_CPP) $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) \
                     $(KIM_MAKE_FILES)
KIM_ModelRefresh_c.o: KIM_ModelRefresh_c.cpp $(KIM_MODEL_REFRESH_C) $(KIM_MODEL_REFRESH_CPP) $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_Model_c.o: KIM_Model_c.cpp $(KIM_MODEL_C) $(KIM_MODEL_CPP) \
                     $(KIM_CALLBACK_NAME_C) $(KIM_CALLBACK_NAME_CPP) $(KIM_ARGUMENT_NAME_C) $(KIM_ARGUMENT_NAME_CPP) \
                     $(KIM_SUPPORT_STATUS_C) $(KIM_SUPPORT_STATUS_CPP) $(KIM_UNIT_SYSTEM_C) $(KIM_UNIT_SYSTEM_CPP) \
                     $(KIM_NUMBERING_C) $(KIM_NUMBERING_CPP) $(KIM_LANGUAGE_NAME_C) $(KIM_LANGUAGE_NAME_CPP) \
                     $(KIM_SPECIES_NAME_C) $(KIM_SPECIES_NAME_CPP) $(KIM_DATA_TYPE_C) $(KIM_DATA_TYPE_CPP) \
                     $(KIM_LOG_VERBOSITY_C) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_Numbering_c.o: KIM_Numbering_c.cpp $(KIM_NUMBERING_C) $(KIM_NUMBERING_CPP) $(KIM_MAKE_FILES)
KIM_SpeciesName_c.o: KIM_SpeciesName_c.cpp $(KIM_SPECIES_NAME_C) $(KIM_SPECIES_NAME_CPP) $(KIM_MAKE_FILES)
KIM_SupportStatus_c.o: KIM_SupportStatus_c.cpp $(KIM_SUPPORT_STATUS_C) $(KIM_SUPPORT_STATUS_CPP) $(KIM_MAKE_FILES)
KIM_TemperatureUnit_c.o: KIM_TemperatureUnit_c.cpp $(KIM_TEMPERATURE_UNIT_C) $(KIM_TEMPERATURE_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_TimeUnit_c.o: KIM_TimeUnit_c.cpp $(KIM_TIME_UNIT_C) $(KIM_TIME_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_semver_c.o: KIM_semver_c.cpp $(KIM_SEMVER_C) $(KIM_SEMBER_CPP) $(KIM_MAKE_FILES)


#
# Main clean settings and rules
#
.PHONY: clean

clean:
	@printf "Cleaning... c/src.\n"
	$(QUELL)rm -f *.o *.a


#
# Main install settings and rules
#
.PHONY: install dirs-install headers-install

install_includedir = $(dest_package_dir)/include
install_include_c = KIM_ArgumentName.h \
                    KIM_CallbackName.h \
                    KIM_ChargeUnit.h \
                    KIM_DataType.h \
                    KIM_EnergyUnit.h \
                    KIM_LanguageName.h \
                    KIM_LengthUnit.h \
                    KIM_Log.h \
                    KIM_LogVerbosity.h \
                    KIM_Model.h \
                    KIM_ModelCompute.h KIM_ModelComputeLogMacros.h \
                    KIM_ModelCreate.h KIM_ModelCreateLogMacros.h \
                    KIM_ModelDestroy.h KIM_ModelDestroyLogMacros.h \
                    KIM_ModelDriverCreate.h KIM_ModelDriverCreateLogMacros.h \
                    KIM_ModelRefresh.h \
                    KIM_ModelRefreshLogMacros.h \
                    KIM_Numbering.h \
                    KIM_SpeciesName.h \
                    KIM_SupportStatus.h \
                    KIM_TemperatureUnit.h \
                    KIM_TimeUnit.h \
                    KIM_UnitSystem.h \
                    KIM_func.h \
                    KIM_semver.h

install: dirs-install headers-install

dirs-install:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_includedir)"

headers-install:
	@printf "Installing...($(dest_package_dir))................................. c/include.\n"
        # Install include files
	$(QUELL)for fl in $(install_include_c); do $(INSTALL_PROGRAM) -m 0644 "../include/$$fl" "$(install_includedir)/$$fl"; done

#
# Main uninstall settings and rules
#
.PHONY: uninstall headers-uninstall dirs-unistall

uninstall: headers-uninstall dirs-uninstall

headers-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. c/include.\n"
        # Uninstall include files
	$(QUELL)for fl in $(install_include_c); do rm -f "$(install_includedir)/$$fl"; done

dirs-uninstall:
	$(QUELL)if test -d "$(install_includedir)"; then rmdir "$(install_includedir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(dest_package_dir)"; then rmdir "$(dest_package_dir)" > /dev/null 2>&1 || true; fi
