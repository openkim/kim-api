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
#    Valeriu Smirichinski
#    Ryan S. Elliott
#    Ellad B. Tadmor
#

#
# Release: This file is part of the openkim-api.git repository.
#


# load in variable definitions and pattern rules
include Include.mk


# create list of models
MODELS_TARGETS := $(addprefix ., $(addsuffix .make-temp, $(filter-out .%,$(shell find $(KIM_MODELS_DIR) -maxdepth 1 -mindepth 1 -type d -exec basename {} \;))))
# create list of models that have been deleted from the $(KIM_MODELS_DIR) since the last time make was executed
DELETED_MODELS_TARGETS := $(filter-out $(MODELS_TARGETS), $(shell find . -name ".*.make-temp" -exec basename {} \;))
# If there are deleted models, remove files and touch .models-deleted to force a reconfigure
ifneq ($(DELETED_MODELS_TARGETS),)
   DUMMY:=$(shell rm -f .*.make-temp model_init_include.{h,cpp} && touch .models-deleted)
   $(info Deleted Models: $(patsubst .%.make-temp,%,$(DELETED_MODELS_TARGETS)))
   $(info Must reconfigure all models...)
   $(info )
endif


all: $(ALLOBJ)

# default target
ifdef KIM_DYNAMIC
   lib: libkim.so
else
   lib: libkim.a
endif

Makefile: Include.mk
	@touch Makefile

ifneq ($(OSTYPE),darwin11)
libkim.a: $(ALLOBJ) $(MODELOBJ)
	mkdir $(KIM_API_DIR)temp_archive_dir && cd $(KIM_API_DIR)temp_archive_dir && \
        $(foreach arch, $(MODELOBJ),ar x $(arch);) \
        ar rcs $(KIM_API_DIR)$@ *.o $(addprefix $(KIM_API_DIR),$(ALLOBJ)) && \
        cd $(KIM_API_DIR) && rm -rf $(KIM_API_DIR)temp_archive_dir
libkim.so: $(ALLOBJ)
	$(LINKCOMPILER) $(SHARED_LIB_FLAG) $(CPPLIBFLAG) $(LINKSONAME)$(KIM_API_DIR)$@ -o $@ *.o

else
ifdef KIM_DYNAMIC
libkim.a: $(ALLOBJ)
	ar rcs $(KIM_API_DIR)$@ $(ALLOBJ)
else
libkim.a: $(ALLOBJ) $(MODELOBJ)
	mkdir $(KIM_API_DIR)temp_archive_dir && cd $(KIM_API_DIR)temp_archive_dir && \
        $(foreach arch, $(MODELOBJ),ar x $(arch);) \
        ar rcs $(KIM_API_DIR)$@ *.o $(addprefix $(KIM_API_DIR),$(ALLOBJ)) && \
        cd $(KIM_API_DIR) && rm -rf $(KIM_API_DIR)temp_archive_dir
endif

libkim.so:  libkim.a
	$(LINKCOMPILER) $(SHARED_LIB_FLAG) $(CPPLIBFLAG) $(LINKSONAME)$(KIM_API_DIR)$@ -o $@ *.o
endif


KIM_API.o: model_init_include.h model_init_include.cpp model_kim_str_include.cpp KIM_API.h KIM_API.cpp KIM_AUX.h Makefile

KIM_API_C.o: KIM_API.h KIM_API_C.h Makefile

KIM_API_F.o: Makefile

KIM_AUX.o: KIM_AUX.h Makefile

# targets for the models: for any new model adds the appropriate lines to the model_init_include.{h,cpp} files
$(MODELS_TARGETS): .models-deleted
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,%_init,$@) model_init_include.h); then echo void \* \
	`echo $(patsubst .%.make-temp,%,$@) | tr A-Z a-z`_init_\(void \*\*\)\; >> model_init_include.h;  fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,%_init,$@) model_init_include.cpp); then \
	echo \"\" if\(strcmp\(modelname,\\\"$(patsubst .%.make-temp,%,$@)\\\"\)==0\){ >> model_init_include.cpp;\
	echo \"       \" `echo $(patsubst .%.make-temp,%,$@) | tr A-Z a-z`_init_\(\(void \*\*\) pkim\)\; >> model_init_include.cpp;\
	echo \"       \" return true\; >> model_init_include.cpp;\
	echo \" }\" >> model_init_include.cpp;\
	fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,%_kim_str,$@) model_kim_str_include.h); then echo \
	`echo char\* $(patsubst .%.make-temp,%,$@)`_kim_str'()'\; >> model_kim_str_include.h;  fi"
	@sh -c "if !(grep -i -q -s $(patsubst .%.make-temp,%_kim_str,$@) model_kim_str_include.cpp); then \
	echo \"\" if\(strcmp\(modelname,\\\"$(patsubst .%.make-temp,%,$@)\\\"\)==0\){ >> model_kim_str_include.cpp;\
	echo \"       in_mdlstr = \" `echo $(patsubst .%.make-temp,%,$@)`_kim_str'()'\; >> model_kim_str_include.cpp;\
	echo \" }\" >> model_kim_str_include.cpp;\
	fi"
	@touch $@
	@echo "Configuring Model: $(patsubst .%.make-temp,%,$@)..."

# If .models-deleted doesn't exist or Makefile is newer, delete files and then create .models-deleted.
.models-deleted : Makefile
	@rm -f .*.make-temp model_init_include.{h,cpp} model_kim_str_include.{h,cpp}
	@touch .models-deleted

MODEL_NAME_KIM_STR_H = char* standard_kim_str'('')'';'
MODEL_NAME_KIM_STR_CPP= char* standard_kim_str'('')''{'
MODEL_NAME = standard
# dependencies for autogenerated files
model_init_include.h model_init_include.cpp: Makefile .models-deleted $(MODELS_TARGETS)

.PHONY: all tests clean

clean:
	rm -f *.o *.a *.so *.mod  model_init_include.{h,cpp} model_kim_str_include.{h,cpp} .*.make-temp .models-deleted standard_kim_str.cpp &&\
        rm -rf $(KIM_API_DIR)temp_archive_dir
