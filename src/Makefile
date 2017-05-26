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

#
# Release: This file is part of the kim-api.git repository.
#

ifeq ($(wildcard Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include Makefile.KIM_Config

ifndef MODELS_LIST
  MODEL_DRIVERS_LIST := $(filter-out $(if $(wildcard $(modeldriversdir)/.kimignore),$(shell cat $(modeldriversdir)/.kimignore),),$(patsubst $(modeldriversdir)/%/,%,$(filter-out $(modeldriversdir)/,$(sort $(dir $(wildcard $(modeldriversdir)/*/))))))
  MODELS_LIST        := $(filter-out $(if $(wildcard $(modelsdir)/.kimignore),$(shell cat $(modelsdir)/.kimignore),),$(patsubst $(modelsdir)/%/,%,$(filter-out $(modelsdir)/,$(sort $(dir $(wildcard $(modelsdir)/*/))))))
endif


# Definition of c and fortran .o file list
OBJCPP = KIM_LanguageName.o \
         KIM_LogVerbosity.o \
         KIM_Log.o \
         KIM_SpeciesName.o \
         KIM_Numbering.o \
         KIM_DataType.o \
         KIM_LengthUnit.o \
         KIM_EnergyUnit.o \
         KIM_ChargeUnit.o \
         KIM_TemperatureUnit.o \
         KIM_TimeUnit.o \
         KIM_ArgumentName.o \
         KIM_CallBackName.o \
         KIM_Attribute.o \
         KIM_Model.o \
         KIM_ModelInitialization.o \
         KIM_ModelDriverInitialization.o \
         KIM_ModelReinitialization.o \
         KIM_ModelDestroy.o \
         KIM_ModelCompute.o \
         KIM_semver.o \
         \
         KIM_ModelImplementation.o \
         KIM_ModelLibrary.o \
         old_KIM_API_DIRS.o

OBJC = KIM_LanguageName_c.o \
       KIM_LogVerbosity_c.o \
       KIM_Log_c.o \
       KIM_SpeciesName_c.o \
       KIM_Numbering_c.o \
       KIM_DataType_c.o \
       KIM_LengthUnit_c.o \
       KIM_EnergyUnit_c.o \
       KIM_ChargeUnit_c.o \
       KIM_TemperatureUnit_c.o \
       KIM_TimeUnit_c.o \
       KIM_ArgumentName_c.o \
       KIM_CallBackName_c.o \
       KIM_Attribute_c.o \
       KIM_Model_c.o \
       KIM_ModelInitialization_c.o \
       KIM_ModelDriverInitialization_c.o \
       KIM_ModelReinitialization_c.o \
       KIM_ModelDestroy_c.o \
       KIM_ModelCompute_c.o \
       KIM_semver_c.o

OBJF03 = kim_language_name_module.o kim_language_name_f.o \
         kim_log_f.o \
         kim_log_verbosity_module.o kim_log_verbosity_f.o \
         kim_species_name_module.o kim_species_name_f.o \
         kim_numbering_module.o kim_numbering_f.o \
         kim_data_type_module.o kim_data_type_f.o \
         kim_length_unit_module.o \
         kim_energy_unit_module.o \
         kim_charge_unit_module.o \
         kim_temperature_unit_module.o \
         kim_time_unit_module.o \
         kim_argument_name_module.o kim_argument_name_f.o \
         kim_call_back_name_module.o kim_call_back_name_f.o \
         kim_attribute_module.o kim_attribute_f.o \
         kim_model_f.o \
         kim_model_initialization_f.o \
         kim_model_driver_initialization_f.o \
         kim_model_reinitialization_f.o \
         kim_model_destroy_f.o \
         kim_model_compute_f.o

#fortran on/of
ifdef KIM_NO_FORTRAN
   ALLOBJ = $(OBJCPP) $(OBJC)
else
   ALLOBJ = $(OBJCPP) $(OBJC) $(OBJF03)
endif

# Set defines
KIMDEFINES = $(KIM_DEFINE_VALUES)

# load lists
MODELS_TARGETS := $(addprefix ., $(addsuffix .make-temp, $(MODELS_LIST)))

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

all: objects libs

objects: $(ALLOBJ)
libs: lib$(KIM_LIB).so

lib$(KIM_LIB).%: lib$(KIM_LIB_BUILD).%
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).$*" "lib$(KIM_LIB).$*"

lib$(KIM_LIB_BUILD).a: $(ALLOBJ)
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(ALLOBJ)
	$(QUELL)ar rcs .$@ $(ALLOBJ)
lib$(KIM_LIB_BUILD).so: lib$(KIM_LIB_BUILD).a .lib$(KIM_LIB_BUILD).a
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)$(LD) $(ARCHFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(srcdir)/lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) $@ \
                      $(LDWHOLEARCHIVESTARTFLAG) $< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(LDLIBS)
	$(QUELL)$(LD) $(ARCHFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(LINKSOPATH)lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) .$@ \
                      $(LDWHOLEARCHIVESTARTFLAG) .$< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(LDLIBS)

# old_KIM_API_Version.h: old_KIM_API_Version.h.tpl $(KIM_MAKE_FILES)
# 	@printf "Creating... old_KIM_API_Version.h.\n"
# 	$(QUELL)cat old_KIM_API_Version.h.tpl | sed -e 's/versionMajor/$(VERSION_MAJOR)/g' \
#                                                 -e 's/versionMinor/$(VERSION_MINOR)/g' \
#                                                 -e 's/versionPatch/$(VERSION_PATCH)/g' \
#                                                 -e 's/versionPrerelease/"$(VERSION_PRERELEASE)"/g' \
#                                                 -e 's/versionBuildMetadata/"$(VERSION_BUILD_METADATA)"/g' \
#                                                 -e 's/versionString/"$(VERSION_FULL)"/g' \
#                 > $@

# # improve this --- shouldn't need to touch files like this
# %.h:
# 	@touch $@

KIM_ModelImplementation.o: KIM_ModelImplementation.hpp KIM_ModelImplementation.cpp KIM_ModelLibrary.hpp $(KIM_MAKE_FILES)
KIM_ModelLibrary.o: KIM_DEFINE_VALUES +=  -D MODELLIBFILE=\"lib$(MODEL_LIB)\" -D MODELDRIVERLIBFILE=\"lib$(MODEL_DRIVER_LIB)\"
KIM_ModelLibrary.o: KIM_ModelLibrary.hpp KIM_ModelLibrary.cpp old_KIM_API_DIRS.h $(KIM_MAKE_FILES)

KIM_LanguageName.o: KIM_LanguageName.hpp KIM_LanguageName.cpp $(KIM_MAKE_FILES)
KIM_LogVerbosity.o: KIM_LogVerbosity.hpp KIM_LogVerbosity.cpp $(KIM_MAKE_FILES)
KIM_Log.o: KIM_Log.hpp KIM_Log.cpp $(KIM_MAKE_FILES)
KIM_SpeciesName.o: KIM_SpeciesName.hpp KIM_SpeciesName.cpp $(KIM_MAKE_FILES)
KIM_DataType.o: KIM_DataType.hpp KIM_DataType.cpp $(KIM_MAKE_FILES)
KIM_Numbering.o: KIM_Numbering.hpp KIM_Numbering.cpp $(KIM_MAKE_FILES)
KIM_LengthUnit.o: KIM_LengthUnit.hpp KIM_LengthUnit.cpp $(KIM_MAKE_FILES)
KIM_EnergyUnit.o: KIM_EnergyUnit.hpp KIM_EnergyUnit.cpp $(KIM_MAKE_FILES)
KIM_ChargeUnit.o: KIM_ChargeUnit.hpp KIM_ChargeUnit.cpp $(KIM_MAKE_FILES)
KIM_TemperatureUnit.o: KIM_TemperatureUnit.hpp KIM_TemperatureUnit.cpp $(KIM_MAKE_FILES)
KIM_TimeUnit.o: KIM_TimeUnit.hpp KIM_TimeUnit.cpp $(KIM_MAKE_FILES)
KIM_ArgumentName.o: KIM_ArgumentName.hpp KIM_ArgumentName.cpp $(KIM_MAKE_FILES)
KIM_CallBackName.o: KIM_CallBackName.hpp KIM_CallBackName.cpp $(KIM_MAKE_FILES)
KIM_Attribute.o: KIM_Attribute.hpp KIM_Attribute.cpp $(KIM_MAKE_FILES)
KIM_Model.o: KIM_Model.hpp KIM_Model.cpp $(KIM_MAKE_FILES)
KIM_ModelInitialization.o: KIM_ModelInitialization.hpp KIM_ModelInitialization.cpp $(KIM_MAKE_FILES)
KIM_ModelDriverInitialization.o: KIM_ModelDriverInitialization.hpp KIM_ModelDriverInitialization.cpp $(KIM_MAKE_FILES)
KIM_ModelReinitialization.o: KIM_ModelReinitialization.hpp KIM_ModelReinitialization.cpp $(KIM_MAKE_FILES)
KIM_ModelDestroy.o: KIM_ModelDestroy.hpp KIM_ModelDestroy.cpp $(KIM_MAKE_FILES)
KIM_ModelCompute.o: KIM_ModelCompute.hpp KIM_ModelCompute.cpp $(KIM_MAKE_FILES)
KIM_semver.o: KIM_semver.hpp KIM_semver.cpp $(KIM_MAKE_FILES)

KIM_LanguageName_c.o: KIM_LanguageName.h KIM_LanguageName_c.cpp $(KIM_MAKE_FILES)
KIM_LogVerbosity_c.o: KIM_LogVerbosity.h KIM_LogVerbosity_c.cpp $(KIM_MAKE_FILES)
KIM_Log_c.o: KIM_Log.h KIM_Log_c.cpp $(KIM_MAKE_FILES)
KIM_SpeciesName_c.o: KIM_SpeciesName.h KIM_SpeciesName_c.cpp $(KIM_MAKE_FILES)
KIM_DataType_c.o: KIM_DataType.h KIM_DataType_c.cpp $(KIM_MAKE_FILES)
KIM_Numbering_c.o: KIM_Numbering.h KIM_Numbering_c.cpp $(KIM_MAKE_FILES)
KIM_LengthUnit_c.o: KIM_LengthUnit.hpp KIM_LengthUnit_c.cpp $(KIM_MAKE_FILES)
KIM_EnergyUnit_c.o: KIM_EnergyUnit.hpp KIM_EnergyUnit_c.cpp $(KIM_MAKE_FILES)
KIM_ChargeUnit_c.o: KIM_ChargeUnit.hpp KIM_ChargeUnit_c.cpp $(KIM_MAKE_FILES)
KIM_TemperatureUnit_c.o: KIM_TemperatureUnit.hpp KIM_TemperatureUnit_c.cpp $(KIM_MAKE_FILES)
KIM_TimeUnit_c.o: KIM_TimeUnit.hpp KIM_TimeUnit_c.cpp $(KIM_MAKE_FILES)
KIM_ArgumentName_c.o: KIM_ArgumentName.h KIM_ArgumentName_c.cpp $(KIM_MAKE_FILES)
KIM_CallBackName_c.o: KIM_CallBackName.h KIM_CallBackName_c.cpp $(KIM_MAKE_FILES)
KIM_Attribute_c.o: KIM_Attribute.h KIM_Attribute_c.cpp $(KIM_MAKE_FILES)
KIM_Model_c.o: KIM_Model.h KIM_Model_c.cpp $(KIM_MAKE_FILES)
KIM_ModelInitialization_c.o: KIM_ModelInitialization.h KIM_ModelInitialization_c.cpp $(KIM_MAKE_FILES)
KIM_ModelDriverInitialization_c.o: KIM_ModelDriverInitialization.h KIM_ModelDriverInitialization_c.cpp $(KIM_MAKE_FILES)
KIM_ModelReinitialization_c.o: KIM_ModelReinitialization.h KIM_ModelReinitialization_c.cpp $(KIM_MAKE_FILES)
KIM_ModelDestroy_c.o: KIM_ModelDestroy.h KIM_ModelDestroy_c.cpp $(KIM_MAKE_FILES)
KIM_ModelCompute_c.o: KIM_ModelCompute.h KIM_ModelCompute_c.cpp $(KIM_MAKE_FILES)
KIM_semver_c.o: KIM_semver.h KIM_semver_c.cpp $(KIM_MAKE_FILES)

kim_language_name_id_module.mod: kim_language_name_id_module.f90 $(KIM_MAKE_FILES)
kim_language_name_module.mod kim_language_name_module.o: kim_language_name_id_module.mod kim_language_name_module.f90 $(KIM_MAKE_FILES)
kim_language_name_f.o: kim_language_name_module.mod kim_language_name_f.f90 $(KIM_MAKE_FILES)
kim_log_verbosity_id_module.mod: kim_log_verbosity_id_module.f90 $(KIM_MAKE_FILES)
kim_log_verbosity_module.mod kim_log_verbosity_module.o: kim_log_verbosity_id_module.mod kim_log_verbosity_module.f90 $(KIM_MAKE_FILES)
kim_log_verbosity_f.o: kim_log_verbosity_module.mod kim_log_verbosity_f.f90 $(KIM_MAKE_FILES)
kim_log_module.mod: kim_log_verbosity_module.mod kim_log_module.f90 $(KIM_MAKE_FILES)
kim_log_f.mod kim_log_f.o: kim_log_module.mod kim_log_f.f90 $(KIM_MAKE_FILES)
kim_species_name_id_module.mod: kim_species_name_id_module.f90 $(KIM_MAKE_FILES)
kim_species_name_module.mod kim_species_name_module.o: kim_species_name_id_module.mod kim_species_name_module.f90 $(KIM_MAKE_FILES)
kim_species_name_f.o: kim_species_name_module.mod kim_species_name_f.f90 $(KIM_MAKE_FILES)
kim_numbering_id_module.mod: kim_numbering_id_module.f90 $(KIM_MAKE_FILES)
kim_numbering_module.mod kim_numbering_module.o: kim_numbering_id_module.mod kim_numbering_module.f90 $(KIM_MAKE_FILES)
kim_numbering_f.o: kim_numbering_module.mod kim_numbering_f.f90 $(KIM_MAKE_FILES)
kim_data_type_id_module.mod: kim_data_type_id_module.f90 $(KIM_MAKE_FILES)
kim_data_type_module.mod kim_data_type_module.o: kim_data_type_id_module.mod kim_data_type_module.f90 $(KIM_MAKE_FILES)
kim_data_type_f.o: kim_data_type_module.mod kim_data_type_f.f90 $(KIM_MAKE_FILES)
kim_argument_name_id_module.mod: kim_argument_name_id_module.f90 $(KIM_MAKE_FILES)
kim_argument_name_module.mod kim_argument_name_module.o: kim_argument_name_id_module.mod kim_argument_name_module.f90 $(KIM_MAKE_FILES)
kim_argument_name_f.o: kim_argument_name_module.mod kim_argument_name_f.f90 $(KIM_MAKE_FILES)
kim_call_back_name_id_module.mod: kim_call_back_name_id_module.f90 $(KIM_MAKE_FILES)
kim_call_back_name_module.mod kim_call_back_name_module.o: kim_call_back_name_id_module.mod kim_call_back_name_module.f90 $(KIM_MAKE_FILES)
kim_call_back_name_f.o: kim_call_back_name_module.mod kim_call_back_name_f.f90 $(KIM_MAKE_FILES)
kim_attribute_id_module.mod: kim_attribute_id_module.f90 $(KIM_MAKE_FILES)
kim_attribute_module.mod kim_attribute_module.o: kim_attribute_id_module.mod kim_attribute_module.f90 $(KIM_MAKE_FILES)
kim_attribute_f.o: kim_attribute_module.mod kim_attribute_f.f90 $(KIM_MAKE_FILES)
kim_length_unit_id_module.mod: kim_length_unit_id_module.f90 $(KIM_MAKE_FILES)
kim_length_unit_module.mod kim_length_unit_module.o: kim_length_unit_id_module.mod kim_length_unit_module.f90 $(KIM_MAKE_FILES)
kim_length_unit_f.o: kim_length_unit_module.mod kim_length_unit_f.f90 $(KIM_MAKE_FILES)
kim_energy_unit_id_module.mod: kim_energy_unit_id_module.f90 $(KIM_MAKE_FILES)
kim_energy_unit_module.mod kim_energy_unit_module.o: kim_energy_unit_id_module.mod kim_energy_unit_module.f90 $(KIM_MAKE_FILES)
kim_energy_unit_f.o: kim_energy_unit_module.mod kim_energy_unit_f.f90 $(KIM_MAKE_FILES)
kim_charge_unit_id_module.mod: kim_charge_unit_id_module.f90 $(KIM_MAKE_FILES)
kim_charge_unit_module.mod kim_charge_unit_module.o: kim_charge_unit_id_module.mod kim_charge_unit_module.f90 $(KIM_MAKE_FILES)
kim_charge_unit_f.o: kim_charge_unit_module.mod kim_charge_unit_f.f90 $(KIM_MAKE_FILES)
kim_temperature_unit_id_module.mod: kim_temperature_unit_id_module.f90 $(KIM_MAKE_FILES)
kim_temperature_unit_module.mod kim_temperature_unit_module.o: kim_temperature_unit_id_module.mod kim_temperature_unit_module.f90 $(KIM_MAKE_FILES)
kim_temperature_unit_f.o: kim_temperature_unit_module.mod kim_temperature_unit_f.f90 $(KIM_MAKE_FILES)
kim_time_unit_id_module.mod: kim_time_unit_id_module.f90 $(KIM_MAKE_FILES)
kim_time_unit_module.mod kim_time_unit_module.o: kim_time_unit_id_module.mod kim_time_unit_module.f90 $(KIM_MAKE_FILES)
kim_time_unit_f.o: kim_time_unit_module.mod kim_time_unit_f.f90 $(KIM_MAKE_FILES)
kim_unit_system_module.mod: kim_length_unit_module.mod kim_energy_unit_module.mod kim_charge_unit_module.mod kim_temperature_unit_module.mod kim_time_unit_module.mod kim_unit_system_module.f90 $(KIM_MAKE_FILES)
kim_model_module.mod: kim_species_name_module.mod kim_data_type_module.mod kim_unit_system_module.mod kim_model_module.f90 $(KIM_MAKE_FILES)
kim_model_f.o: kim_model_module.mod kim_model_f.f90 $(KIM_MAKE_FILES)
kim_model_initialization_module.mod: kim_model_initialization_module.f90 $(KIM_MAKE_FILES)
kim_model_initialization_f.o: kim_model_initialization_module.mod kim_model_initialization_f.f90 $(KIM_MAKE_FILES)
kim_model_driver_initialization_f.o: kim_model_driver_initialization_module.mod kim_model_driver_initialization_f.f90 $(KIM_MAKE_FILES)
kim_model_reinitialization_module.mod: kim_model_reinitialization_module.f90 $(KIM_MAKE_FILES)
kim_model_reinitialization_f.o: kim_model_reinitialization_module.mod kim_model_reinitialization_f.f90 $(KIM_MAKE_FILES)
kim_model_destroy_module.mod: kim_model_destroy_module.f90 $(KIM_MAKE_FILES)
kim_model_destroy_f.o: kim_model_destroy_module.mod kim_model_destroy_f.f90 $(KIM_MAKE_FILES)
kim_model_compute_module.mod: kim_model_compute_module.f90 $(KIM_MAKE_FILES)
kim_model_compute_f.o: kim_model_compute_module.mod kim_model_compute_f.f90 $(KIM_MAKE_FILES)
kim_semver_module.mod kim_semver_module.o: kim_semver_module.f90 $(KIM_MAKE_FILES)

 old_KIM_API_DIRS.o: old_KIM_API_DIRS.h $(KIM_MAKE_FILES)

.INTERMEDIATE: .old_KIM_API_DIRS.cpp
.old_KIM_API_DIRS.cpp: old_KIM_API_DIRS.cpp $(KIM_MAKE_FILES)
	@printf "Creating... Intermediate file..... $@.\n"
	$(QUELL)cp $< $@

%IM_API_DIRS.o: KIM_DEFINE_VALUES +=  -D INPLACE=$(if $(filter-out .old_KIM_API_DIRS.o,$@),1,0) \
                                      -D PACKAGEDIR=\"$(if $(filter-out .old_KIM_API_DIRS.o,$@),$(srcdir),$(package_dir))\" \
                                      -D KIMDIR=\"$(KIM_DIR)\" \
                                      -D MODELDRIVERSDIR=\"$(modeldriversdir)\" \
                                      -D MODELSDIR=\"$(modelsdir)\" \
                                      -D PACKAGENAME=\"$(package_name)\" \
                                      -D USERROOT=$(if $(filter-out $(HOME),$(user_config_file_root_name)),1,0) \
                                      -D USERCONFIGFILEROOTNAME=\"$(user_config_file_root_name)\" \
                                      -D USERCONFIGFILEDIRNAME=\"$(user_config_file_dir_name)\" \
                                      -D VERSION_MAJOR=\"$(VERSION_MAJOR)\" \
                                      -D MODELLIBFILE=\"lib$(MODEL_LIB)\" \
                                      -D MODELDRIVERLIBFILE=\"lib$(MODEL_DRIVER_LIB)\"

.PHONY: all objects libs clean install objects-install libs-install installdirs uninstall objects-uninstall install-check libs-uninstall

clean:
	@printf "Cleaning... src.\n"
	$(QUELL)rm -f *.o .*.o *.a .*.a *.so .*.so *.mod

install_modelsdir = $(dest_package_dir)/$(modelsdir)
install_modeldriversdir = $(dest_package_dir)/$(modeldriversdir)
install_includedir = $(dest_package_dir)/include
install_nodest_includedir = $(package_dir)/include
install_include = # @@@@ needs updating

install-check:
        # should we check that the installed stuff is actually dynamic-load and the right settings (32bit, etc.)?
	$(QUELL)if test -d "$(dest_package_dir)"; then \
                  rm -rf "$(install_includedir)"; \
                  rm -f  "$(DESTDIR)$(includedir)/$(full_package_name)"; \
                  rm -f  "$(dest_package_dir)/lib*.so"; \
                  rm -f  "$(DESTDIR)$(libdir)/lib$(full_package_name).so"; \
                fi

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(includedir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_includedir)"
ifneq (static-link,$(KIM_LINK))
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_modelsdir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_modeldriversdir)"
endif

install: objects-install libs-install

objects-install: install-check objects
	@true

libs-install: libs installdirs
	@printf "Installing...($(dest_package_dir))................................. src"
        # Install include files
	$(QUELL)for fl in $(install_include); do $(INSTALL_PROGRAM) -m 0644 "$$fl" "$(install_includedir)/$$fl"; done
	$(QUELL)ln -fs "$(install_nodest_includedir)" "$(DESTDIR)$(includedir)/$(full_package_name)"
        # Install lib files
	$(QUELL)$(INSTALL_PROGRAM) -m 0644 "lib$(KIM_LIB_BUILD).a" "$(dest_package_dir)/lib$(KIM_LIB_BUILD).a"
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).a" "$(dest_package_dir)/lib$(KIM_LIB).a"
	$(QUELL)ln -fs "$(full_package_name)/lib$(KIM_LIB).a" "$(DESTDIR)$(libdir)/lib$(KIM_LIB).a"
	@printf ".\n"

uninstall: objects-uninstall libs-uninstall

objects-uninstall:
	@true

libs-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. src.\n"
	$(QUELL)if test -d "$(dest_package_dir)"; then rm -rf "$(dest_package_dir)"; fi
        # Uninstall include files
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(full_package_name)" && if test -L "$$fl"; then rm -f "$$fl"; fi
        # Uninstall lib files
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).so" && if test -L "$$fl"; then rm -f "$$fl"; fi
