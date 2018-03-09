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
#    Ryan S. Elliott
#

#
# Release: This file is part of the kim-api.git repository.
#

ifeq ($(wildcard ../../Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include ../../Makefile.KIM_Config

VPATH = ../include ../../c/include
KIMINCLUDEFLAGS = -I../include -I../../c/include

# Definition of cpp object files
OBJCPP = KIM_ComputeArgumentName.o \
         KIM_ComputeArguments.o \
         KIM_ComputeCallbackName.o \
         KIM_ChargeUnit.o \
         KIM_DataType.o \
         KIM_EnergyUnit.o \
         KIM_LanguageName.o \
         KIM_LengthUnit.o \
         KIM_Log.o \
         KIM_LogVerbosity.o \
         KIM_Model.o \
         KIM_ModelCompute.o \
         KIM_ModelComputeArguments.o \
         KIM_ModelComputeArgumentsCreate.o \
         KIM_ModelComputeArgumentsDestroy.o \
         KIM_ModelCreate.o \
         KIM_ModelDestroy.o \
         KIM_ModelDriverCreate.o \
         KIM_ModelRefresh.o \
         KIM_Numbering.o \
         KIM_SemVer.o \
         KIM_SpeciesName.o \
         KIM_SupportStatus.o \
         KIM_TemperatureUnit.o \
         KIM_TimeUnit.o \
         \
         KIM_LogImplementation.o \
         KIM_ComputeArgumentsImplementation.o \
         KIM_ModelImplementation.o \
         KIM_ModelLibrary.o \
         old_KIM_API_DIRS.o

#
# Main build settings and rules
#
.PHONY: all objects lib

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += $(KIM_DIR)/cpp/include/Makefile.dependencies \
                  $(KIM_DIR)/c/include/Makefile.dependencies \
                  $(KIM_DIR)/cpp/src/Makefile

all: objects lib

objects: $(OBJCPP)
lib: lib$(KIM_LIB).so

lib$(KIM_LIB).so: lib$(KIM_LIB_BUILD).so
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).so" "lib$(KIM_LIB).so"

lib-cpp-$(KIM_LIB_BUILD).a: $(OBJCPP)
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(OBJCPP)

# Update LINKSOPATH, if prefix is non-standard
ifeq ($(filter $(standard_system_prefixes),$(prefix)),)
   LINKSOPATH = $(package_dir)/
endif

lib$(KIM_LIB_BUILD).so: lib-cpp-$(KIM_LIB_BUILD).a #../../c/src/lib-c-$(KIM_LIB_BUILD).a ../../fortran/src/lib-fortran-$(KIM_LIB_BUILD).a
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)$(LD) $(ARCHFLAG) $(FNOMAINFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(LINKSOPATH)lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) $@ \
                      $(LDWHOLEARCHIVESTARTFLAG) $^ $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(LDLIBS)


# Include file dependency lists
include ../include/Makefile.dependencies
include ../../c/include/Makefile.dependencies

KIM_ComputeArgumentName.o: KIM_ComputeArgumentName.cpp $(KIM_COMPUTE_ARGUMENT_NAME_CPP) $(KIM_DATA_TYPE_CPP) $(KIM_MAKE_FILES)
KIM_ComputeArguments.o: KIM_ComputeArguments.cpp $(KIM_COMPUTE_ARGUMENTS_CPP) $(KIM_COMPUTE_ARGUMENT_NAME_CPP) \
                        $(KIM_COMPUTE_CALLBACK_NAME_CPP) $(KIM_SUPPORT_STATUS_CPP) $(KIM_LANGUAGE_NAME_CPP) $(KIM_MAKE_FILES)
KIM_ComputeCallbackName.o: KIM_ComputeCallbackName.cpp $(KIM_COMPUTE_CALLBACK_NAME_CPP) $(KIM_MAKE_FILES)
KIM_ChargeUnit.o: KIM_ChargeUnit.cpp $(KIM_CHARGE_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_DataType.o: KIM_DataType.cpp $(KIM_DATA_TYPE_CPP) $(KIM_MAKE_FILES)
KIM_EnergyUnit.o: KIM_EnergyUnit.cpp $(KIM_ENERGY_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_LanguageName.o: KIM_LanguageName.cpp $(KIM_LANGUAGE_NAME_CPP) $(KIM_MAKE_FILES)
KIM_LengthUnit.o: KIM_LengthUnit.cpp $(KIM_LENGTH_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_Log.o: KIM_Log.cpp $(KIM_LOG_CPP) $(KIM_LOG_VERBOSITY_CPP) $(KIM_LOG_IMPLEMENTATION_CPP) $(KIM_MAKE_FILES)
KIM_LogVerbosity.o: KIM_LogVerbosity.cpp $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_Model.o: KIM_Model.cpp $(KIM_MODEL_IMPLEMENTATION_CPP) $(KIM_MODEL_CPP) $(KIM_UNIT_SYSTEM_CPP) \
             $(KIM_COMPUTE_ARGUMENTS_CPP) $(KIM_NUMBERING_CPP) $(KIM_SPECIES_NAME_CPP) $(KIM_DATA_TYPE_CPP) $(KIM_MAKE_FILES)
KIM_ModelCompute.o: KIM_ModelCompute.cpp $(KIM_MODEL_IMPLEMENTATION_CPP) $(KIM_MODEL_COMPUTE_CPP) \
                    $(KIM_LOG_CPP) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_ModelComputeArguments.o: KIM_ModelComputeArguments.cpp $(KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_CPP) \
                             $(KIM_MODEL_COMPUTE_ARGUMENTS_CPP) $(KIM_COMPUTE_CALLBACK_NAME_CPP) \
                             $(KIM_COMPUTE_ARGUMENT_NAME_CPP) $(KIM_MAKE_FILES)
KIM_ModelComputeArgumentsCreate.o: KIM_ModelComputeArgumentsCreate.cpp $(KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_CPP) \
                             $(KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_CPP) $(KIM_COMPUTE_CALLBACK_NAME_CPP) \
                             $(KIM_COMPUTE_ARGUMENT_NAME_CPP) $(KIM_MAKE_FILES)
KIM_ModelComputeArgumentsDestroy.o: KIM_ModelComputeArgumentsDestroy.cpp $(KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_CPP) \
                             $(KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_CPP) $(KIM_MAKE_FILES)
KIM_ModelCreate.o: KIM_ModelCreate.cpp $(KIM_MODEL_IMPLEMENTATION_CPP) $(KIM_MODEL_CREATE_CPP) \
                   $(KIM_UNIT_SYSTEM_CPP) $(KIM_SPECIES_NAME_CPP) $(KIM_LANGUAGE_NAME_CPP) \
                   $(KIM_LOG_CPP) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
KIM_ModelDestroy.o: KIM_ModelDestroy.cpp $(KIM_MODEL_IMPLEMENTATION_CPP) $(KIM_MODEL_DESTROY_CPP) \
                    $(KIM_LANGUAGE_NAME_CPP) $(KIM_LOG_CPP) $(KIM_LOG_VERBOSITY_CPP) \
                    $(KIM_FUNC_CPP) $(KIM_MAKE_FILES)
KIM_ModelDriverCreate.o: KIM_ModelDriverCreate.cpp $(KIM_MODEL_IMPLEMENTATION_CPP) \
                         $(KIM_MODEL_DRIVER_CREATE_CPP) $(KIM_CALLBACK_NAME_CPP) \
                         $(KIM_ARGUMENT_NAME_CPP) $(KIM_SUPPORT_STATUS_CPP) $(KIM_UNIT_SYSTEM_CPP) \
                         $(KIM_SPECIES_NAME_CPP) $(KIM_LANGUAGE_NAME_CPP) $(KIM_LOG_CPP) \
                         $(KIM_LOG_VERGOSITY_CPP) $(KIM_MAKE_FILES)
KIM_ModelRefresh.o: KIM_ModelRefresh.cpp $(KIM_MODEL_IMPLEMENTATION_CPP) $(KIM_UNIT_SYSTEM_CPP) \
                    $(KIM_MODEL_REFERESH_CPP) $(KIM_LOG_CPP) $(KIM_MAKE_FILES)
KIM_Numbering.o: KIM_Numbering.cpp $(KIM_NUMBERING_CPP) $(KIM_MAKE_FILES)
KIM_SpeciesName.o: KIM_SpeciesName.cpp $(KIM_SPECIES_NAME_CPP) $(KIM_MAKE_FILES)
KIM_SupportStatus.o: KIM_SupportStatus.cpp $(KIM_SUPPORT_STATUS_CPP) $(KIM_MAKE_FILES)
KIM_TemperatureUnit.o: KIM_TemperatureUnit.cpp $(KIM_TEMPERATURE_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_TimeUnit.o: KIM_TimeUnit.cpp $(KIM_TIME_UNIT_CPP) $(KIM_MAKE_FILES)
KIM_SemVer.o: KIM_SemVer.cpp $(KIM_SEM_VER_CPP) $(KIM_MAKE_FILES)


KIM_ModelLibrary.o: KIM_ModelLibrary.cpp KIM_ModelLibraryLogMacros.hpp \
                    $(KIM_MODEL_LIBRARY_CPP) $(KIM_LOG_VERBOSITY_CPP) $(KIM_LANGUAGE_NAME_CPP) \
                    $(KIM_API_DIRS_CPP) $(KIM_MAKE_FILES)
KIM_ModelLibrary.o: KIMDEFINES = -D MODELLIBFILE=\"lib$(MODEL_LIB)\" -D MODELDRIVERLIBFILE=\"lib$(MODEL_DRIVER_LIB)\"

KIM_LogImplementation.o: KIM_LogImplementation.cpp $(KIM_LOG_IMPLEMENTATION_CPP) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)

KIM_ComputeArgumentsImplementation.o: KIM_ComputeArgumentsImplementation.cpp \
                           $(KIM_COMPUTE_ARGUMENTS_CTEATE_C) $(KIM_COMPUTE_ARGUMENTS_DESTROY_C) \
                           $(KI_LANGUAGE_NAME_CPP) $(KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_CPP) $(KIM_MAKE_FILES)

KIM_ModelImplementation.o: KIM_ModelImplementation.cpp KIM_ModelImplementationLogMacros.hpp \
                           $(KIM_MODEL_DESTROY_C) $(KIM_MODEL_COMPUTE_C) \
                           $(KIM_MODEL_REFRESH_C) $(KIM_MODEL_DRIVER_CREATE_C) $(KIM_MODEL_CREATE_C) \
                           $(KIM_UNIT_SYSTEM_C) $(KI_LANGUAGE_NAME_CPP) $(KIM_MODEL_LIBRARY_CPP) \
                           $(KIM_MODEL_IMPLEMENTATION_CPP) $(KIM_MAKE_FILES)

old_KIM_API_DIRS.o: $(KIM_API_DIRS_CPP) $(KIM_LOG_VERBOSITY_CPP) $(KIM_MAKE_FILES)
old_KIM_API_DIRS.o: KIMDEFINES = -D PACKAGEDIR=\"$(package_dir)\" \
                                 -D KIMDIR=\"$(KIM_DIR)\" \
                                 -D KIMLIBBUILD=\"$(KIM_LIB_BUILD).so\" \
                                 -D MODELDRIVERSDIR=\"$(modeldriversdir)\" \
                                 -D MODELSDIR=\"$(modelsdir)\" \
                                 -D PACKAGENAME=\"$(package_name)\" \
                                 -D USERROOT=$(if $(filter-out $(HOME),$(user_config_file_root_name)),1,0) \
                                 -D USERCONFIGFILEROOTNAME=\"$(user_config_file_root_name)\" \
                                 -D USERCONFIGFILEDIRNAME=\"$(user_config_file_dir_name)\" \
                                 -D VERSION_MAJOR=\"$(VERSION_MAJOR)\" \
                                 -D MODELLIBFILE=\"lib$(MODEL_LIB)\" \
                                 -D MODELDRIVERLIBFILE=\"lib$(MODEL_DRIVER_LIB)\"

#
# Main clean settings and rules
#
.PHONY: clean

clean:
	@printf "Cleaning... cpp/src.\n"
	$(QUELL)rm -f *.o *.a *.so


#
# Main install settings and rules
#
.PHONY: install dirs-install headers-install lib-install

install_modelsdir = $(dest_package_dir)/$(modelsdir)
install_modeldriversdir = $(dest_package_dir)/$(modeldriversdir)
install_includedir = $(dest_package_dir)/include
install_include_cpp = KIM_ComputeArgumentName.hpp \
                      KIM_ComputeArguments.hpp \
                      KIM_ComputeCallbackName.hpp \
                      KIM_ChargeUnit.hpp \
                      KIM_DataType.hpp \
                      KIM_EnergyUnit.hpp \
                      KIM_LOG_DEFINES.inc \
                      KIM_LOG_MAXIMUM_LEVEL.inc \
                      KIM_LanguageName.hpp \
                      KIM_LengthUnit.hpp \
                      KIM_Log.hpp \
                      KIM_LogVerbosity.hpp \
                      KIM_Model.hpp \
                      KIM_ModelCompute.hpp KIM_ModelComputeLogMacros.hpp \
                      KIM_ModelComputeArguments.hpp KIM_ModelComputeArgumentsLogMacros.hpp \
                      KIM_ModelComputeArgumentsCreate.hpp KIM_ModelComputeArgumentsCreateLogMacros.hpp \
                      KIM_ModelComputeArgumentsDestroy.hpp KIM_ModelComputeArgumentsDestroyLogMacros.hpp \
                      KIM_ModelCreate.hpp KIM_ModelCreateLogMacros.hpp \
                      KIM_ModelDestroy.hpp KIM_ModelDestroyLogMacros.hpp \
                      KIM_ModelDriverCreate.hpp KIM_ModelDriverCreateLogMacros.hpp \
                      KIM_ModelRefresh.hpp KIM_ModelRefreshLogMacros.hpp \
                      KIM_Numbering.hpp \
                      KIM_SemVer.hpp \
                      KIM_SpeciesName.hpp \
                      KIM_SupportStatus.hpp \
                      KIM_TemperatureUnit.hpp \
                      KIM_TimeUnit.hpp \
                      KIM_UnitSystem.hpp \
                      KIM_Version.hpp \
                      KIM_func.hpp

install: dirs-install headers-install lib-install

dirs-install:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_includedir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_modelsdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_modeldriversdir)"
        #
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(dest_package_dir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(libdir)"

headers-install:
	@printf "Installing...($(dest_package_dir))................................. cpp/include.\n"
        # Install include files
	$(QUELL)for fl in $(install_include_cpp); do $(INSTALL_PROGRAM) -m 0644 "../include/$$fl" "$(install_includedir)/$$fl"; done

lib-install: lib$(KIM_LIB_BUILD).so
	@printf "Installing...($(dest_package_dir))................................. cpp/src.\n"
        # Install lib files
	$(QUELL)$(INSTALL_PROGRAM) -m 0755 "lib$(KIM_LIB_BUILD).so" "$(dest_package_dir)/lib$(KIM_LIB_BUILD).so"
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).so" "$(dest_package_dir)/lib$(KIM_LIB).so"
	$(QUELL)ln -fs "$(full_package_name)/lib$(KIM_LIB).so" "$(DESTDIR)$(libdir)/lib$(KIM_LIB).so"


#
# Main uninstall settings and rules
#
.PHONY: uninstall lib-uninstall headers-uninstall dirs-uninstall

uninstall: lib-uninstall headers-uninstall dirs-uninstall

lib-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. cpp/src.\n"
        # Uninstall lib files
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).so" && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(dest_package_dir)/lib$(KIM_LIB).so" && if test -L "$$fl"; then rm -f "$$fl"; fi
	$(QUELL)fl="$(dest_package_dir)/lib$(KIM_LIB_BUILD).so" && if test -f "$$fl"; then rm -f "$$fl"; fi

headers-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. cpp/include.\n"
	$(QUELL)for fl in $(install_include_cpp); do if test -f "$(install_includedir)/$$fl"; then rm -f "$(install_includedir)/$$fl"; fi; done

dirs-uninstall:
	$(QUELL)if test -d "$(install_modeldriversdir)"; then rmdir "$(install_modeldriversdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(install_modelsdir)"; then rmdir "$(install_modelsdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(install_includedir)"; then rmdir "$(install_includedir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(dest_package_dir)"; then rmdir "$(dest_package_dir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)$(libdir)"; then rmdir "$(DESTDIR)$(libdir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(DESTDIR)"; then rmdir "$(DESTDIR)" > /dev/null 2>&1 || true; fi
