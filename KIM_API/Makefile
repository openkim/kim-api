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
# Copyright (c) 2013, Regents of the University of Minnesota.
# All rights reserved.
#
# Contributors:
#    Valeriu Smirichinski
#    Ryan S. Elliott
#    Ellad B. Tadmor
#

#
# Release: This file is part of the openkim-api.git repository.
#


include Makefile.KIMConfig

# Definition of c and fortran .o file list
OBJC = KIM_API.o KIM_API_C.o standard_kim_str.o Unit_Handling.o KIM_AUX.o
OBJF90 = KIM_API_F.o

#fortran on/of
ifdef KIM_NO_FORTRAN
   ALLOBJ = $(OBJC)
else
   ALLOBJ = $(OBJC) $(OBJF90)
endif

# create list of models
MODELS_TARGETS := $(addprefix ., $(addsuffix .make-temp, $(filter-out $(shell if test -e "$(KIM_MODELS_DIR)/.kimignore"; then cat "$(KIM_MODELS_DIR)/.kimignore";fi;),$(filter-out .%,$(shell find "$(KIM_MODELS_DIR)/" -maxdepth 1 -mindepth 1 -type d -exec basename {} \;)))))
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

objects: $(ALLOBJ)

ifneq ($(KIM_LINK),static-link)
   libs: libkim.so openkim-api-descriptor-file-match
else
   libs: libkim.a openkim-api-descriptor-file-match
endif

Makefile: $(KIM_MAKE_FILES)
	@touch Makefile

ifneq ($(KIM_LINK),static-link)
   libkim.a: $(ALLOBJ)
	@printf "Creating... openkim-api library... $@.\n";
	@ar rcs $@ $(ALLOBJ)
else
   libkim.a: $(ALLOBJ) # should also depend on MODEL and MODEL DRIVER archives
	@printf "Creating... openkim-api library... $@.\n";
	@mkdir $(KIM_DIR)/KIM_API/temp_archive_dir && cd $(KIM_DIR)/KIM_API/temp_archive_dir && \
        $(foreach arch, $(shell find $(KIM_MODEL_DRIVERS_DIR) $(KIM_MODELS_DIR) -name "*.a"),ar x $(arch);) \
        ar rcs $(KIM_DIR)/KIM_API/$@ *.o $(addprefix $(KIM_DIR)/KIM_API/,$(ALLOBJ)) && \
        cd $(KIM_DIR)/KIM_API/ && rm -rf $(KIM_DIR)/KIM_API/temp_archive_dir
endif
libkim.so: libkim.a $(if $(filter-out dynamic-load,$(KIM_LINK)),$(foreach mdl, $(patsubst .%.make-temp, %, $(MODELS_TARGETS)), $(KIM_MODELS_DIR)/$(mdl)/$(mdl).so),)
	@printf "Creating... openkim-api library... $@.\n";
	@$(LD) $(LDSHAREDFLAG) $(LINKSONAME)$(KIM_DIR)/KIM_API/$@ $(LDFLAGS) $(OUTPUTINFLAG) $@ \
               $(LDWHOLEARCHIVESTARTFLAG) $< $(LDWHOLEARCHIVEENDFLAG) \
               $(LDDYNAMICLIB) $(XLANGLDLIBS) \
               $(if $(filter-out dynamic-load,$(KIM_LINK)),\
                  $(foreach mdl, $(patsubst .%.make-temp, %, $(MODELS_TARGETS)),$(KIM_MODELS_DIR)/$(mdl)/$(mdl).so),)

KIM_API.o: $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include.h model_init_include.cpp model_kim_str_include.cpp,) KIM_API.h KIM_API.cpp KIM_AUX.h Makefile

KIM_API_C.o: KIM_API.h KIM_API_C.h Makefile

KIM_API_F.o: Makefile

KIM_AUX.o: KIM_AUX.h Makefile

openkim-api-descriptor-file-match.o: openkim-api-descriptor-file-match.cpp KIM_API.h KIM_API_status.h Makefile

model_init_include_stubs.o: model_init_include_stubs.c

openkim-api-descriptor-file-match: openkim-api-descriptor-file-match.o KIM_API.o KIM_AUX.o Unit_Handling.o standard_kim_str.o \
        $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include_stubs.o,)
	@printf "Creating... $@.\n"
	@$(LD) -o $@ $@.o KIM_API.o KIM_AUX.o Unit_Handling.o standard_kim_str.o $(if $(filter-out dynamic-load,$(KIM_LINK)),model_init_include_stubs.o,) $(LDDYNAMICLIB)

# targets for the models: for any new model, adds the appropriate lines to the model_init_include.h and model_inclue.cpp files
$(MODELS_TARGETS): .models-deleted
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,'int %_init',$@) model_init_include.h); then echo int \
	`echo $(patsubst .%.make-temp,%,$@)`_init\(void \*\*\)\; >> model_init_include.h;  fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,'return %_init',$@) model_init_include.cpp); then \
	echo \"\" if\(strcmp\(modelname,\\\"$(patsubst .%.make-temp,%,$@)\\\"\)==0\){ >> model_init_include.cpp;\
	echo \"       \" return `echo $(patsubst .%.make-temp,%,$@)`_init\(\(void \*\*\) pkim\)\; >> model_init_include.cpp;\
	echo \" }\" >> model_init_include.cpp;\
	fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,'int %_init',$@) model_init_include_stubs.c); then echo int \
	`echo $(patsubst .%.make-temp,%,$@)`_init\(void \*\* kimmdl\) {return 1\;} >> model_init_include_stubs.c; \
	echo char* `echo $(patsubst .%.make-temp,%,$@)`_kim_str = \\\"\\\"\; >> model_init_include_stubs.c;  fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,'char* %_kim_str',$@) model_kim_str_include.h); then echo \
	`echo char\* $(patsubst .%.make-temp,%,$@)`_kim_str'()'\; >> model_kim_str_include.h;  fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,'= %_kim_str',$@) model_kim_str_include.cpp); then \
	echo \"\" if\(strcmp\(modelname,\\\"$(patsubst .%.make-temp,%,$@)\\\"\)==0\){ >> model_kim_str_include.cpp;\
	echo \"       in_mdlstr = \" `echo $(patsubst .%.make-temp,%,$@)`_kim_str'()'\; >> model_kim_str_include.cpp;\
	echo \" }\" >> model_kim_str_include.cpp;\
	fi"
	@touch $@
	@echo "Configuring Model: $(patsubst .%.make-temp,%,$@)..."

# If .models-deleted doesn't exist or Makefile is newer, delete files and then create .models-deleted.
.models-deleted : Makefile
	@rm -f .*.make-temp $(addprefix model_init_include.,h cpp) $(addprefix model_kim_str_include.,h cpp)
	@touch .models-deleted

MODEL_NAME_KIM_STR_H = char* standard_kim_str();
MODEL_NAME_KIM_STR_CPP = char* standard_kim_str(){
MODEL_NAME = standard
# dependencies for autogenerated files
model_init_include.h model_init_include.cpp: Makefile .models-deleted $(MODELS_TARGETS)

.PHONY: all objects libs clean

clean:
	@printf "Cleaning... KIM_API.\n"
	@rm -f *.o *.a *.so *.mod  model_init_include.h model_init_include.cpp model_init_include_stubs.c model_kim_str_include.h model_kim_str_include.cpp \
        .*.make-temp .models-deleted standard_kim_str.cpp &&\
        rm -f openkim-api-descriptor-file-match &&\
        rm -rf $(KIM_DIR)/KIM_API/temp_archive_dir
