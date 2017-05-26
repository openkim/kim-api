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
         KIM_Logger.o \
         KIM_SpeciesName.o \
         KIM_ParticleNumbering.o \
         KIM_DataType.o \
         KIM_UnitSystem.o \
         KIM_COMPUTE_ArgumentName.o \
         KIM_COMPUTE_ArgumentAttribute.o \
         KIM_COMPUTE_ModelComputeArguments.o \
         KIM_Model.o \
         KIM_COMPUTE_SimulatorComputeArguments.o \
         KIM_Simulator.o \
         KIM_semver.o \
         old_KIM_API.o \
         $(KIM_STR_NAME).o \
         old_Unit_Handling.o

OBJC = KIM_LanguageName_c.o \
       KIM_Logger_c.o \
       KIM_SpeciesName_c.o \
       KIM_ParticleNumbering_c.o \
       KIM_DataType_c.o \
       KIM_UnitSystem_c.o \
       KIM_COMPUTE_ArgumentName_c.o \
       KIM_COMPUTE_ArgumentAttribute_c.o \
       KIM_COMPUTE_ModelComputeArguments_c.o \
       KIM_Model_c.o \
       KIM_COMPUTE_SimulatorComputeArguments_c.o \
       KIM_Simulator_c.o \
       KIM_semver_c.o

OBJF03 = kim_language_name_f.o \
         kim_logger_f.o \
         kim_species_name_f.o \
         kim_particle_numbering_f.o \
         kim_data_type_f.o \
         kim_unit_system_f.o \
         kim_compute_argument_name_f.o \
         kim_compute_argument_attribute_f.o \
         kim_compute_model_compute_arguments_f.o \
         kim_model_f.o \
         kim_compute_simulator_compute_arguments_f.o \
         kim_simulator_f.o \

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
# create list of models that have been deleted from the $(modelsdir)/ since the last time make was executed
DELETED_MODELS_TARGETS := $(filter-out $(MODELS_TARGETS), $(shell find . -name ".*.make-temp" -exec basename {} \;))
# If there are deleted models, remove files and touch .models-deleted to force a reconfigure
ifneq ($(DELETED_MODELS_TARGETS),)
   DUMMY:=$(shell rm -f .*.make-temp model_init_include.h model_init_include.cpp && touch .models-deleted)
   $(info Deleted Models: $(patsubst .%.make-temp,%,$(DELETED_MODELS_TARGETS)))
   $(info Must reconfigure all models...)
   $(info )
endif

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += Makefile

all: objects libs

ifneq ($(KIM_LINK),static-link)
   objects: $(ALLOBJ) old_KIM_API_DIRS.o .old_KIM_API_DIRS.o
   libs: lib$(KIM_LIB).so
else
   objects: models_check $(ALLOBJ) old_KIM_API_DIRS.o
   libs: models_check lib$(KIM_LIB).a

   models_check:
	@if test \( $(words $(MODELS_LIST)) -ne 1 \) -o \( $(words $(MODEL_DRIVERS_LIST)) -gt 1 \); then \
        printf "*******************************************************************************\n"; \
        printf "*******          Can't compile the API for static linking with          *******\n"; \
        printf "*******                %3i Models and %3i Model Drivers                 *******\n" $(words $(MODELS_LIST)) $(words $(MODEL_DRIVERS_LIST)); \
        printf "*******          Static linking requires exactly ONE Model and          *******\n"; \
        printf "*******                    ZERO OR ONE Model Drivers                    *******\n"; \
        printf "*******************************************************************************\n"; \
        false; else true; fi
endif

lib$(KIM_LIB).%: lib$(KIM_LIB_BUILD).%
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).$*" "lib$(KIM_LIB).$*"

ifneq ($(KIM_LINK),static-link)
   lib$(KIM_LIB_BUILD).a: $(ALLOBJ) old_KIM_API_DIRS.o .old_KIM_API_DIRS.o
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(ALLOBJ) old_KIM_API_DIRS.o
	$(QUELL)ar rcs .$@ $(ALLOBJ) .old_KIM_API_DIRS.o
else
   lib$(KIM_LIB_BUILD).a: MD_DEPS:= $(addprefix $(srcdir)/$(modeldriversdir)/,$(addsuffix /lib$(MODEL_DRIVER_LIB_BUILD).a,$(MODEL_DRIVERS_LIST)))
   lib$(KIM_LIB_BUILD).a: MO_DEPS:= $(addprefix $(srcdir)/$(modelsdir)/,$(addsuffix /lib$(MODEL_LIB_BUILD).a,$(MODELS_LIST)))
   lib$(KIM_LIB_BUILD).a: $(ALLOBJ) $(MD_DEPS) $(MO_DEPS)
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)mkdir temp_archive_dir && cd temp_archive_dir && \
        $(foreach arch, $(MD_DEPS) $(MO_DEPS),ar x $(arch);) \
        ar rcs $(srcdir)/$@ *.o $(addprefix $(srcdir)/,$(ALLOBJ)) && \
        cd $(srcdir) && rm -rf temp_archive_dir
endif
lib$(KIM_LIB_BUILD).so: MD_DEPS :=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdldrv, $(MODEL_DRIVERS_LIST), $(srcdir)/$(modeldriversdir)/$(mdldrv)/lib$(MODEL_DRIVER_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: MO_DEPS :=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdl, $(MODELS_LIST), $(srcdir)/$(modelsdir)/$(mdl)/lib$(MODEL_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: DMD_DEPS:=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdldrv, $(MODEL_DRIVERS_LIST), $(srcdir)/$(modeldriversdir)/$(mdldrv)/.lib$(MODEL_DRIVER_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: DMO_DEPS:=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdl, $(MODELS_LIST), $(srcdir)/$(modelsdir)/$(mdl)/.lib$(MODEL_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: lib$(KIM_LIB_BUILD).a .lib$(KIM_LIB_BUILD).a $(MD_DEPS) $(MO_DEPS)
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)$(LD) $(ARCHFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(srcdir)/lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) $@ \
                      $(LDWHOLEARCHIVESTARTFLAG) $< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(MD_DEPS) $(MO_DEPS) \
                      $(LDLIBS)
	$(QUELL)$(LD) $(ARCHFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(LINKSOPATH)lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) .$@ \
                      $(LDWHOLEARCHIVESTARTFLAG) .$< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(DMD_DEPS) $(DMO_DEPS) \
                      $(LDLIBS)

old_KIM_API_Version.h: old_KIM_API_Version.h.tpl $(KIM_MAKE_FILES)
	@printf "Creating... old_KIM_API_Version.h.\n"
	$(QUELL)cat old_KIM_API_Version.h.tpl | sed -e 's/versionMajor/$(VERSION_MAJOR)/g' \
                                                -e 's/versionMinor/$(VERSION_MINOR)/g' \
                                                -e 's/versionPatch/$(VERSION_PATCH)/g' \
                                                -e 's/versionPrerelease/"$(VERSION_PRERELEASE)"/g' \
                                                -e 's/versionBuildMetadata/"$(VERSION_BUILD_METADATA)"/g' \
                                                -e 's/versionString/"$(VERSION_FULL)"/g' \
                > $@

# improve this --- shouldn't need to touch files like this
%.h:
	@touch $@

KIM_LanguageName.o: KIM_LanguageName.hpp KIM_LanguageName.cpp $(KIM_MAKE_FILES)
KIM_Logger.o: KIM_Logger.hpp KIM_Logger.cpp old_KIM_API.o $(KIM_MAKE_FILES)
KIM_SpeciesName.o: KIM_SpeciesName.hpp KIM_SpeciesName.cpp $(KIM_MAKE_FILES)
KIM_DataType.o: KIM_DataType.hpp KIM_DataType.cpp $(KIM_MAKE_FILES)
KIM_ParticleNumbering.o: KIM_ParticleNumbering.hpp KIM_ParticleNumbering.cpp $(KIM_MAKE_FILES)
KIM_UnitSystem.o: KIM_UnitSystem.hpp KIM_UnitSystem.cpp old_KIM_API.o $(KIM_MAKE_FILES)
KIM_COMPUTE_ArgumentName.o: KIM_COMPUTE_ArgumentName.hpp KIM_COMPUTE_ArgumentName.cpp $(KIM_MAKE_FILES)
KIM_COMPUTE_ArgumentAttribute.o: KIM_COMPUTE_ArgumentAttribute.hpp KIM_COMPUTE_ArgumentAttribute.cpp $(KIM_MAKE_FILES)
KIM_Model.o: KIM_Model.hpp KIM_Model.cpp old_KIM_API.o $(KIM_MAKE_FILES)
KIM_COMPUTE_ModelComputeArguments.o: KIM_COMPUTE_ModelComputeArguments.hpp KIM_COMPUTE_ModelComputeArguments.cpp old_KIM_API.o $(KIM_MAKE_FILES)
KIM_Simulator.o: KIM_Simulator.hpp KIM_Simulator.cpp old_KIM_API.o $(KIM_MAKE_FILES)
KIM_COMPUTE_SimulatorComputeArguments.o: KIM_COMPUTE_SimulatorComputeArguments.hpp KIM_COMPUTE_SimulatorComputeArguments.cpp old_KIM_API.o $(KIM_MAKE_FILES)
KIM_semver.o: KIM_semver.hpp KIM_semver.cpp old_KIM_API.o $(KIM_MAKE_FILES)

KIM_LanguageName_c.o: KIM_LanguageName.h KIM_LanguageName_c.cpp $(KIM_MAKE_FILES)
KIM_Logger_c.o: KIM_Logger.h KIM_Logger_c.cpp $(KIM_MAKE_FILES)
KIM_SpeciesName_c.o: KIM_SpeciesName.h KIM_SpeciesName_c.cpp $(KIM_MAKE_FILES)
KIM_DataType_c.o: KIM_DataType.h KIM_DataType_c.cpp $(KIM_MAKE_FILES)
KIM_ParticleNumbering_c.o: KIM_ParticleNumbering.h KIM_ParticleNumbering_c.cpp $(KIM_MAKE_FILES)
KIM_UnitSystem_c.o: KIM_UnitSystem.h KIM_UnitSystem_c.cpp $(KIM_MAKE_FILES)
KIM_COMPUTE_ArgumentName_c.o: KIM_COMPUTE_ArgumentName.h KIM_COMPUTE_ArgumentName_c.cpp $(KIM_MAKE_FILES)
KIM_COMPUTE_ArgumentAttribute_c.o: KIM_COMPUTE_ArgumentAttribute.h KIM_COMPUTE_ArgumentAttribute_c.cpp $(KIM_MAKE_FILES)
KIM_Model_c.o: KIM_Model.h KIM_Model_c.cpp $(KIM_MAKE_FILES)
KIM_COMPUTE_ModelComputeArguments_c.o: KIM_COMPUTE_ModelComputeArguments.h KIM_COMPUTE_ModelComputeArguments_c.cpp $(KIM_MAKE_FILES)
KIM_Simulator_c.o: KIM_Simulator.h KIM_Simulator_c.cpp $(KIM_MAKE_FILES)
KIM_COMPUTE_SimulatorComputeArguments_c.o: KIM_COMPUTE_SimulatorComputeArguments.h KIM_COMPUTE_SimulatorComputeArguments_c.cpp $(KIM_MAKE_FILES)
KIM_semver_c.o: KIM_semver.h KIM_semver_c.cpp $(KIM_MAKE_FILES)


kim_language_name_id_module.mod: kim_language_name_id_module.f90
kim_language_name_module.mod: kim_language_name_id_module.mod kim_language_name_module.f90
kim_language_name_f.o: kim_language_name_module.mod kim_language_name_f.f90
kim_logger_module.mod: kim_logger_module.f90
kim_logger_f.o: kim_logger_module.mod kim_logger_f.f90
kim_species_name_id_module.mod: kim_species_name_id_module.f90
kim_species_name_module.mod: kim_species_name_id_module.mod kim_species_name_module.f90
kim_species_name_f.o: kim_species_name_module.mod kim_species_name_f.f90
kim_particle_numbering_id_module.mod: kim_particle_numbering_id_module.f90
kim_particle_numbering_module.mod: kim_particle_numbering_id_module.mod kim_particle_numbering_module.f90
kim_particle_numbering_f.o: kim_particle_numbering_module.mod kim_particle_numbering_f.f90
kim_data_type_id_module.mod: kim_data_type_id_module.f90
kim_data_type_module.mod: kim_data_type_id_module.mod kim_data_type_module.f90
kim_data_type_f.o: kim_data_type_module.mod kim_data_type_f.f90
kim_compute_argument_name_id_module.mod: kim_compute_argument_name_id_module.f90
kim_compute_argument_name_module.mod: kim_compute_argument_name_id_module.mod kim_compute_argument_name_module.f90
kim_compute_argument_name_f.o: kim_compute_argument_name_module.mod kim_compute_argument_name_f.f90
kim_compute_argument_attribute_id_module.mod: kim_compute_argument_attribute_id_module.f90
kim_compute_argument_attribute_module.mod: kim_compute_argument_attribute_id_module.mod kim_compute_argument_attribute_module.f90
kim_compute_argument_attribute_f.o: kim_compute_argument_attribute_module.mod kim_compute_argument_attribute_f.f90
kim_compute_model_compute_arguments_module.mod:  kim_language_name_module.mod kim_compute_model_compute_arguments_module.f90
kim_compute_model_compute_arguments_f.o: kim_compute_model_compute_arguments_module.mod kim_compute_model_compute_arguments_f.f90
kim_model_module.mod: kim_language_name_module.mod kim_species_name_module.mod kim_data_type_module.mod kim_compute_model_compute_arguments_module.mod kim_model_module.f90
kim_model_f.o: kim_model_module.mod kim_model_f.f90
kim_compute_simulator_compute_arguments_module.mod: kim_language_name_module.mod kim_compute_argument_name_module.mod kim_compute_simulator_compute_arguments_module.f90
kim_compute_simulator_compute_arguments_f.o: kim_compute_simulator_compute_arguments_module.mod kim_compute_simulator_compute_arguments_f.f90

kim_unit_system_id_module.mod: kim_unit_system_id_module.f90
kim_unit_system_module.mod: kim_unit_system_id_module.mod kim_unit_system_module.f90
kim_unit_system_f.o: kim_unit_system_module.mod kim_unit_system_f.f90


kim_simulator_module.mod: kim_language_name_module.mod kim_species_name_module.mod kim_unit_system_module.mod kim_simulator_module.f90
kim_simulator_f.o: kim_simulator_module.mod kim_simulator_f.f90


#kim_parameter_id_module.mod: kim_parameter_id_module.f90
#kim_parameter_module.mod: kim_parameter_id_module.mod kim_parameter_module.f90
#kim_semver_module.mod: kim_semver_module.f90

old_KIM_API.h: old_KIM_API_Version.h $(KIM_MAKE_FILES)

old_KIM_API_status.h: old_KIM_API_Version.h $(KIM_MAKE_FILES)

old_KIM_API.o: KIM_DEFINE_VALUES += -D STANDARD_KIM_STR_NAME=$(MODEL_NAME)_$(KIM_STR_NAME)               \
                                -D STANDARD_KIM_STR_LEN_NAME=$(MODEL_NAME)_$(KIM_STR_NAME)_len       \
                                -D KIM_STR_NAME=\"$(KIM_STR_NAME)\"                                  \
                                -D MODELLIBFILE=\"lib$(MODEL_LIB)\"
old_KIM_API.o: $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include.h model_init_include.cpp model_kim_str_include.cpp,) \
           old_KIM_API.h old_KIM_API_status.h old_KIM_API_DIRS.h old_KIM_API.cpp $(KIM_MAKE_FILES)

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

old_Unit_Handling.o: old_Unit_Handling.h old_KIM_API.h old_KIM_API_status.h $(KIM_MAKE_FILES)

# targets for the models: for any new model, adds the appropriate lines to the model_init_include.h and model_inclue.cpp files
$(MODELS_TARGETS): .models-deleted
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'\* %_init_pointer',$@) model_init_include.h; then \
                   printf 'extern "C" int (* %s_init_pointer)(void **);\n' $(patsubst .%.make-temp,%,$@)                >> model_init_include.h;\
                fi;
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'return \*%_init_pointer',$@) model_init_include.cpp; then \
                   printf 'if (strcmp(modelname, "%s") == 0)\n{\n   return (*%s_init_pointer)((void **) pkim);\n}\n' \
                   $(patsubst .%.make-temp,%,$@) $(patsubst .%.make-temp,%,$@)                                          >> model_init_include.cpp;\
                fi;
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'char\*\* %_kim_str',$@) model_init_include_stubs.cpp; then \
                   printf 'int (* %s_init_pointer)(void **) = 0;\n' $(patsubst .%.make-temp,%,$@)                       >> model_init_include_stubs.cpp;\
                   printf 'unsigned char* %s_kim_str = 0;\n' $(patsubst .%.make-temp,%,$@)                              >> model_init_include_stubs.cpp;\
                   printf 'unsigned int %s_kim_str_len = 0;\n' $(patsubst .%.make-temp,%,$@)                            >> model_init_include_stubs.cpp;\
                fi;
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'char\* const %_kim_str',$@) model_kim_str_include.h; then \
                   printf 'extern const unsigned int %s_kim_str_len;\n' $(patsubst .%.make-temp,%,$@)                   >> model_kim_str_include.h;\
                   printf 'extern const unsigned char %s_kim_str[];\n' $(patsubst .%.make-temp,%,$@)                    >> model_kim_str_include.h;\
                fi;
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'= %_kim_str_len',$@) model_kim_str_include.cpp; then \
                   printf 'if (strcmp(modelname, "%s") == 0)\n' $(patsubst .%.make-temp,%,$@)                           >> model_kim_str_include.cpp;\
                   printf '{\n'                                                                                         >> model_kim_str_include.cpp;\
                   printf '   in_mdlstr_len = %s_kim_str_len;\n' $(patsubst .%.make-temp,%,$@)                          >> model_kim_str_include.cpp;\
                   printf '   in_mdlstr = %s_kim_str;\n' $(patsubst .%.make-temp,%,$@)                                  >> model_kim_str_include.cpp;\
                   printf '}\n'                                                                                         >> model_kim_str_include.cpp;\
                fi;
	$(QUELL)touch $@
	@echo "Configuring Model: $(patsubst .%.make-temp,%,$@)..."

# If .models-deleted doesn't exist or Makefile is newer, delete files and then create .models-deleted.
.models-deleted : $(KIM_MAKE_FILES)
	$(QUELL)rm -f .*.make-temp $(addprefix model_init_include.,h cpp) $(addprefix model_kim_str_include.,h cpp)
	$(QUELL)touch .models-deleted

MODEL_NAME = standard
MODEL_KIM_FILE = standard.kim
$(KIM_STR_NAME).c: $(MODEL_NAME).kim
# dependencies for autogenerated files
model_init_include.h model_init_include.cpp: $(KIM_MAKE_FILES) .models-deleted $(MODELS_TARGETS)

.PHONY: all objects libs clean models_check install objects-install libs-install installdirs uninstall objects-uninstall install-check libs-uninstall

clean:
	@printf "Cleaning... src.\n"
	$(QUELL)rm -f *.o .*.o *.a .*.a *.so .*.so *.mod  \
                      model_init_include.h model_init_include.cpp \
                      model_init_include_stubs.cpp model_kim_str_include.h \
                      model_kim_str_include.cpp \
                      .*.make-temp .models-deleted \
                      old_KIM_API_Version.h \
                      $(KIM_STR_NAME).c .old_KIM_API_DIRS.cpp \
                      && \
                rm -rf "$(srcdir)/temp_archive_dir"


install_modelsdir = $(dest_package_dir)/$(modelsdir)
install_modeldriversdir = $(dest_package_dir)/$(modeldriversdir)
install_includedir = $(dest_package_dir)/include
install_nodest_includedir = $(package_dir)/include
install_include = old_KIM_API.h old_KIM_API_status.h old_KIM_API_Version.h old_KIM_API_DIRS.h old_Unit_Handling.h

install-check:
ifneq (dynamic-load,$(KIM_LINK))
	@if test -d "$(dest_package_dir)"; then \
        printf "*******************************************************************************\n"; \
        printf "*******               This package is already installed.                *******\n"; \
        printf "*******                 Please 'make uninstall' first.                  *******\n"; \
        printf "*******************************************************************************\n"; \
        false; else true; fi
else
        # should we check that the installed stuff is actually dynamic-load and the right settings (32bit, etc.)?
	$(QUELL)if test -d "$(dest_package_dir)"; then \
                  rm -rf "$(install_includedir)"; \
                  rm -f  "$(DESTDIR)$(includedir)/$(full_package_name)"; \
                  rm -f  "$(dest_package_dir)/lib*.so"; \
                  rm -f  "$(DESTDIR)$(libdir)/lib$(full_package_name).so"; \
                fi
endif

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
ifneq (static-link,$(KIM_LINK))
	$(QUELL)$(INSTALL_PROGRAM) -m 0755 ".lib$(KIM_LIB_BUILD).so" "$(dest_package_dir)/lib$(KIM_LIB_BUILD).so"
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).so" "$(dest_package_dir)/lib$(KIM_LIB).so"
	$(QUELL)ln -fs "$(full_package_name)/lib$(KIM_LIB).so" "$(DESTDIR)$(libdir)/lib$(KIM_LIB).so"
	@printf ".\n"
else
	$(QUELL)$(INSTALL_PROGRAM) -m 0644 "lib$(KIM_LIB_BUILD).a" "$(dest_package_dir)/lib$(KIM_LIB_BUILD).a"
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).a" "$(dest_package_dir)/lib$(KIM_LIB).a"
	$(QUELL)ln -fs "$(full_package_name)/lib$(KIM_LIB).a" "$(DESTDIR)$(libdir)/lib$(KIM_LIB).a"
	@printf ".\n"
endif

uninstall: objects-uninstall libs-uninstall

objects-uninstall:
	@true

libs-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. src.\n"
	$(QUELL)if test -d "$(dest_package_dir)"; then rm -rf "$(dest_package_dir)"; fi
        # Uninstall include files
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(full_package_name)" && if test -L "$$fl"; then rm -f "$$fl"; fi
        # Uninstall lib files
ifneq (static-link,$(KIM_LINK))
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).so" && if test -L "$$fl"; then rm -f "$$fl"; fi
else
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).a" && if test -L "$$fl"; then rm -f "$$fl"; fi
endif
