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
# Copyright (c) 2013--2014, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Ryan S. Elliott
#    Ellad B. Tadmor
#    Valeriu Smirichinski
#

#
# Release: This file is part of the openkim-api.git repository.
#

ifeq ($(wildcard Makefile.KIM_Config),)
  $(error Makefile.KIM_Config does not exist.  Something is wrong with your KIM API package setup)
endif
include Makefile.KIM_Config

ifndef MODELS_LIST
  MODEL_DRIVERS_LIST := $(filter-out $(if $(wildcard $(KIM_MODEL_DRIVERS_DIR)/.kimignore),$(shell cat $(KIM_MODEL_DRIVERS_DIR)/.kimignore),),$(patsubst $(KIM_MODEL_DRIVERS_DIR)/%/,%,$(filter-out $(KIM_MODEL_DRIVERS_DIR)/,$(sort $(dir $(wildcard $(KIM_MODEL_DRIVERS_DIR)/*/))))))
  MODELS_LIST        := $(filter-out $(if $(wildcard $(KIM_MODELS_DIR)/.kimignore),$(shell cat $(KIM_MODELS_DIR)/.kimignore),),$(patsubst $(KIM_MODELS_DIR)/%/,%,$(filter-out $(KIM_MODELS_DIR)/,$(sort $(dir $(wildcard $(KIM_MODELS_DIR)/*/))))))
  TESTS_LIST         := $(filter-out $(if $(wildcard $(KIM_TESTS_DIR)/.kimignore),$(shell cat $(KIM_TESTS_DIR)/.kimignore),),$(patsubst $(KIM_TESTS_DIR)/%/,%,$(filter-out $(KIM_TESTS_DIR)/,$(sort $(dir $(wildcard $(KIM_TESTS_DIR)/*/))))))
endif


# Definition of c and fortran .o file list
OBJC = KIM_API.o KIM_API_C.o $(KIM_STR_NAME).o Unit_Handling.o KIM_AUX.o
OBJF90 = KIM_API_F.o
OBJF03 = KIM_API_F03.o KIM_API_F03_Helper.o

#fortran on/of
ifdef KIM_NO_FORTRAN
   ALLOBJ = $(OBJC)
else
   ALLOBJ = $(OBJC) $(OBJF90) $(OBJF03)
endif

# Set defines
KIMDEFINES = $(KIM_DEFINE_VALUES)

# load lists
MODELS_TARGETS := $(addprefix ., $(addsuffix .make-temp, $(MODELS_LIST)))
# create list of models that have been deleted from the $(KIM_MODELS_DIR)/ since the last time make was executed
DELETED_MODELS_TARGETS := $(filter-out $(MODELS_TARGETS), $(shell find . -name ".*.make-temp" -exec basename {} \;))
# If there are deleted models, remove files and touch .models-deleted to force a reconfigure
ifneq ($(DELETED_MODELS_TARGETS),)
   DUMMY:=$(shell rm -f .*.make-temp model_init_include.h model_init_include.cpp && touch .models-deleted)
   $(info Deleted Models: $(patsubst .%.make-temp,%,$(DELETED_MODELS_TARGETS)))
   $(info Must reconfigure all models...)
   $(info )
endif

all: objects libs

ifneq ($(KIM_LINK),static-link)
   objects: $(if $(filter dynamic-link,$(KIM_LINK)),models_check,) $(ALLOBJ) KIM_API_DIRS.o .KIM_API_DIRS.o
   libs: $(if $(filter dynamic-link,$(KIM_LINK)),models_check,) lib$(KIM_LIB).so $(full_package_name)-descriptor-file-match

   models_check:
	@if test \( $(words $(MODELS_LIST)) -eq 0 \); then \
        printf "*******************************************************************************\n"; \
        printf "*******          Can't compile the API for dynamic-link with            *******\n"; \
        printf "*******                %3i Models and %3i Model Drivers                 *******\n" $(words $(MODELS_LIST)) $(words $(MODEL_DRIVERS_LIST)); \
        printf "*******          Dyanamic linking requires ONE OR MORE Models           *******\n"; \
        printf "*******************************************************************************\n"; \
        false; else true; fi
else
   objects: models_check $(ALLOBJ) KIM_API_DIRS.o
   libs: models_check lib$(KIM_LIB).a $(full_package_name)-descriptor-file-match

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

Makefile: $(KIM_MAKE_FILES)
	$(QUELL)touch Makefile

lib$(KIM_LIB).%: lib$(KIM_LIB_BUILD).%
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ln -fs "lib$(KIM_LIB_BUILD).$*" "lib$(KIM_LIB).$*"

ifneq ($(KIM_LINK),static-link)
   lib$(KIM_LIB_BUILD).a: $(ALLOBJ) KIM_API_DIRS.o .KIM_API_DIRS.o
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(ALLOBJ) KIM_API_DIRS.o
	$(QUELL)ar rcs .$@ $(ALLOBJ) .KIM_API_DIRS.o
else
   lib$(KIM_LIB_BUILD).a: MD_DEPS:= $(addprefix $(KIM_DIR)/$(modeldriversdir)/,$(addsuffix /lib$(MODEL_DRIVER_LIB_BUILD).a,$(MODEL_DRIVERS_LIST)))
   lib$(KIM_LIB_BUILD).a: MO_DEPS:= $(addprefix $(KIM_DIR)/$(modelsdir)/,$(addsuffix /lib$(MODEL_LIB_BUILD).a,$(MODELS_LIST)))
   lib$(KIM_LIB_BUILD).a: $(ALLOBJ) $(MD_DEPS) $(MO_DEPS)
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)mkdir $(KIM_DIR)/KIM_API/temp_archive_dir && cd $(KIM_DIR)/KIM_API/temp_archive_dir && \
        $(foreach arch, $(MD_DEPS) $(MO_DEPS),ar x $(arch);) \
        ar rcs $(KIM_DIR)/KIM_API/$@ *.o $(addprefix $(KIM_DIR)/KIM_API/,$(ALLOBJ)) && \
        cd $(KIM_DIR)/KIM_API/ && rm -rf $(KIM_DIR)/KIM_API/temp_archive_dir
endif
lib$(KIM_LIB_BUILD).so: MD_DEPS :=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdldrv, $(MODEL_DRIVERS_LIST), $(KIM_MODEL_DRIVERS_DIR)/$(mdldrv)/lib$(MODEL_DRIVER_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: MO_DEPS :=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdl, $(MODELS_LIST), $(KIM_MODELS_DIR)/$(mdl)/lib$(MODEL_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: DMD_DEPS:=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdldrv, $(MODEL_DRIVERS_LIST), $(KIM_MODEL_DRIVERS_DIR)/$(mdldrv)/.lib$(MODEL_DRIVER_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: DMO_DEPS:=$(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdl, $(MODELS_LIST), $(KIM_MODELS_DIR)/$(mdl)/.lib$(MODEL_LIB_BUILD).so),)
lib$(KIM_LIB_BUILD).so: lib$(KIM_LIB_BUILD).a .lib$(KIM_LIB_BUILD).a $(MD_DEPS) $(MO_DEPS)
	@printf "Creating... KIM API library....... $@.\n";
	$(QUELL)$(LD) $(ARCHFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(KIM_DIR)/KIM_API/lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) $@ \
                      $(LDWHOLEARCHIVESTARTFLAG) $< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(MD_DEPS) $(MO_DEPS) \
                      $(LDLIBS)
	$(QUELL)$(LD) $(ARCHFLAG) $(LDSHAREDFLAG) $(LINKSONAME)$(package_dir)/lib$(KIM_LIB).so $(LDFLAGS) $(OUTPUTINFLAG) .$@ \
                      $(LDWHOLEARCHIVESTARTFLAG) .$< $(LDWHOLEARCHIVEENDFLAG) \
                      $(LDDYNAMICLIB) $(XLANGLDLIBS) \
                      $(DMD_DEPS) $(DMO_DEPS) \
                      $(LDLIBS)

KIM_API.o: KIM_DEFINE_VALUES += -D STANDARD_KIM_STR_NAME=$(MODEL_NAME)_$(KIM_STR_NAME)               \
                                -D STANDARD_KIM_STR_CHUNKS_NAME=$(MODEL_NAME)_$(KIM_STR_NAME)_chunks \
                                -D KIM_STR_NAME=\"$(KIM_STR_NAME)\"                                  \
                                -D MODELLIBFILE=\"lib$(MODEL_LIB)\"                                  \
                                -D KIM_TESTS_DIR_VAL=\"$(KIM_TESTS_DIR)\"
KIM_API.o: $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include.h model_init_include.cpp model_kim_str_include.cpp,) KIM_API.h KIM_API.cpp KIM_AUX.h KIM_API_DIRS.h Makefile

KIM_API_C.o: KIM_API.h KIM_API_C.h Makefile

KIM_API_F.o: Makefile

KIM_API_F03.o: KIM_API_F03_Multiples.incl KIM_API_F03_Helper.o Makefile

KIM_API_F03_Multiples.incl: CreateGetSetMultipleRoutines.sh Makefile
	@printf "Creating... KIM_API_F03_Multiples.incl.\n"
	$(QUELL)./CreateGetSetMultipleRoutines.sh > KIM_API_F03_Multiples.incl

KIM_API_F03_Helper.o: Makefile

KIM_AUX.o: KIM_AUX.h Makefile

KIM_API_DIRS.o: Makefile
KIM_API_DIRS.o: KIM_DEFINE_VALUES += -D KIMDIR=\"$(KIM_DIR)\"                       \
                                     -D MODELDIR=\"$(KIM_MODELS_DIR)\"              \
                                     -D MODELDRIVERDIR=\"$(KIM_MODEL_DRIVERS_DIR)\"
.KIM_API_DIRS.o: Makefile
.KIM_API_DIRS.o: KIM_DEFINE_VALUES += -D PACKAGEDIR=\"$(package_dir)\"            \
                                      -D MODELDRIVERSDIR=\"$(modeldriversdir)\"   \
                                      -D MODELSDIR=\"$(modelsdir)\"               \
                                      -D PACKAGENAME=\"$(package_name)\" \
                                      -D VERSION_MAJOR=\"$(VERSION_MAJOR)\"

descriptor-file-match.o: descriptor-file-match.cpp KIM_API.h KIM_API_status.h Makefile

model_init_include_stubs.o: model_init_include_stubs.cpp

$(full_package_name)-descriptor-file-match: descriptor-file-match.o KIM_API.o $(if $(filter-out static-link,$(KIM_LINK)),KIM_API_DIRS.o,) KIM_AUX.o Unit_Handling.o $(KIM_STR_NAME).o \
        $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include_stubs.o,)
	@printf "Creating... $@.\n"
	$(QUELL)$(LD) $(ARCHFLAG) $(LDFLAGS) -o $@ descriptor-file-match.o KIM_API.o $(if $(filter-out static-link,$(KIM_LINK)),KIM_API_DIRS.o,) KIM_AUX.o Unit_Handling.o $(KIM_STR_NAME).o $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include_stubs.o,) $(XLANGLDLIBS) $(LDDYNAMICLIB) $(LDLIBS)

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
                   printf 'char** %s_kim_str = 0;\n' $(patsubst .%.make-temp,%,$@)                                      >> model_init_include_stubs.cpp;\
                   printf 'int %s_kim_str_chunks = 0;\n' $(patsubst .%.make-temp,%,$@)                                  >> model_init_include_stubs.cpp;\
                fi;
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'char\* const %_kim_str',$@) model_kim_str_include.h; then \
                   printf 'extern const int %s_kim_str_chunks;\n' $(patsubst .%.make-temp,%,$@)                         >> model_kim_str_include.h;\
                   printf 'extern const char* const %s_kim_str[];\n' $(patsubst .%.make-temp,%,$@)                      >> model_kim_str_include.h;\
                fi;
	$(QUELL)if ! grep -i -q -s $(patsubst .%.make-temp,'= %_kim_str_chunks',$@) model_kim_str_include.cpp; then \
                   printf 'if (strcmp(modelname, "%s") == 0)\n' $(patsubst .%.make-temp,%,$@)                           >> model_kim_str_include.cpp;\
                   printf '{\n'                                                                                         >> model_kim_str_include.cpp;\
                   printf '   in_mdlstr_chunks = %s_kim_str_chunks;\n' $(patsubst .%.make-temp,%,$@)                    >> model_kim_str_include.cpp;\
                   printf '   in_mdlstr = (const char**) %s_kim_str;\n' $(patsubst .%.make-temp,%,$@)                   >> model_kim_str_include.cpp;\
                   printf '}\n'                                                                                         >> model_kim_str_include.cpp;\
                fi;
	$(QUELL)touch $@
	@echo "Configuring Model: $(patsubst .%.make-temp,%,$@)..."

# If .models-deleted doesn't exist or Makefile is newer, delete files and then create .models-deleted.
.models-deleted : Makefile
	$(QUELL)rm -f .*.make-temp $(addprefix model_init_include.,h cpp) $(addprefix model_kim_str_include.,h cpp)
	$(QUELL)touch .models-deleted

MODEL_NAME = standard
$(KIM_STR_NAME).cpp: $(MODEL_NAME).kim
# dependencies for autogenerated files
model_init_include.h model_init_include.cpp: Makefile .models-deleted $(MODELS_TARGETS)

.PHONY: all objects libs clean models_check install objects-install libs-install installdirs uninstall objects-uninstall install-check libs-uninstall

clean:
	@printf "Cleaning... KIM_API.\n"
	$(QUELL)rm -f *.o .*.o *.a .*.a *.so .*.so *.mod  model_init_include.h model_init_include.cpp model_init_include_stubs.cpp model_kim_str_include.h model_kim_str_include.cpp \
                      .*.make-temp .models-deleted KIM_API_F03_Multiples.incl &&\
                rm -f $(full_package_name)-descriptor-file-match &&\
                rm -rf "$(KIM_DIR)/KIM_API/temp_archive_dir"


install_modelsdir = $(dest_package_dir)/$(modelsdir)
install_modeldriversdir = $(dest_package_dir)/$(modeldriversdir)
install_includedir = $(dest_package_dir)/include
install_nodest_includedir = $(package_dir)/include
install_include = KIM_API.h KIM_API_C.h KIM_API_status.h KIM_API_DIRS.h Unit_Handling.h KIM_AUX.h kim_kinds.mod kim_api.mod kim_api_f03.mod
install_bindir = $(dest_package_dir)/bin
install_nodest_bindir = $(package_dir)/bin
install_bin = $(full_package_name)-descriptor-file-match

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
                  rm -rf "$(install_bindir)"; \
                  for fl in $(install_bin); do rm -f "$(DESTDIR)$(bindir)/$$fl"; done; \
                  rm -rf "$(install_includedir)"; \
                  rm -f  "$(DESTDIR)$(includedir)/$(full_package_name)"; \
                  rm -f  "$(dest_package_dir)/lib*.so"; \
                  rm -f  "$(DESTDIR)$(libdir)/lib$(full_package_name).so"; \
                fi
endif

installdirs:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(DESTDIR)$(bindir)"
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_bindir)"
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
	@printf "Installing...($(dest_package_dir))................................. KIM_API"
        # Install bin files
	$(QUELL)for fl in $(install_bin); do $(INSTALL_PROGRAM) -m 0755 "$$fl" "$(install_bindir)/$$fl"; done
	$(QUELL)for fl in $(install_bin); do ln -fs "$(install_nodest_bindir)/$$fl" "$(DESTDIR)$(bindir)/$$fl"; done
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
	@printf "Uninstalling...($(dest_package_dir))................................. KIM_API.\n"
	$(QUELL)if test -d "$(dest_package_dir)"; then rm -rf "$(dest_package_dir)"; fi
        # Uninstall bin files
	$(QUELL)for fl in $(install_bin); do if test -L "$(DESTDIR)$(bindir)/$$fl"; then rm -f "$(DESTDIR)$(bindir)/$$fl"; fi; done
        # Uninstall include files
	$(QUELL)fl="$(DESTDIR)$(includedir)/$(full_package_name)" && if test -L "$$fl"; then rm -f "$$fl"; fi
        # Uninstall lib files
ifneq (static-link,$(KIM_LINK))
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).so" && if test -L "$$fl"; then rm -f "$$fl"; fi
else
	$(QUELL)fl="$(DESTDIR)$(libdir)/lib$(KIM_LIB).a" && if test -L "$$fl"; then rm -f "$$fl"; fi
endif
