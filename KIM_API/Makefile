
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                                                     
# Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
#

include Include.mk



all: $(ALLOBJ) libkim.a

tests:aKIMservice_test KIMserviceTest

.PHONY:configmodels clean configtests


libkim.a:$(ALLOBJ)
	@ar rcs  $@ $(ALLOBJ)

configmodels: clean tolowercase modelsconfig
	@echo $(notdir $(patsubst %/,%,$(filter-out $(F_FILTER_OUT),$(wildcard $(KIM_MODELS_DIR)*/)))) > listmodelname.txt
	@echo $(notdir $(patsubst %/,%_init_,$(filter-out $(F_FILTER_OUT),$(wildcard $(KIM_MODELS_DIR)*/)))) > listmodelinit2.txt
	./tolowercase < listmodelinit2.txt | cat  > listmodelinit.txt ; rm -f listmodelinit2.txt
	./modelsconfig; rm -f listmodelinit.txt

configtests:  testsconfig
	@echo $(notdir $(patsubst %/,%,$(filter-out $(F_FILTER_OUT),$(wildcard $(KIM_TESTS_DIR)*/)))) > listtestname.txt
	./testsconfig

tolowercase: tolowercase.cpp
	$(CPPCOMPILER) $<  $(OUTPUTIN) $@

modelsconfig: modelsconfig.cpp
	$(CPPCOMPILER) -D KIM_DIR_MODELS=\"$(KIM_MODELS_DIR)\"  $<  $(OUTPUTIN) $@

testsconfig: testsconfig.cpp
	$(CPPCOMPILER) -D KIM_DIR_MODELS=\"$(KIM_MODELS_DIR)\"  $<  $(OUTPUTIN) $@

aKIMservice_test:   aKIMservice_test.o  $(ALLOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG)  $(KIM_LIB) $(ALLOBJ) $(MODELOBJ) $< $(OUTPUTIN) $@
	cp $@ RUN/$@

KIMserviceTest: KIMserviceTest.o $(ALLOBJ)
	$(LINKCOMPILER) $(CPPFLAG) $(CPPLIBFLAG) $(ALLOBJ) $(MODELOBJ) $< $(OUTPUTIN) $@
	cp $@ RUN/$@



#local dependencies
aKIMservice_test.o: $(ALLOBJ)
KIMserviceTest.o: $(ALLOBJ)
KIMserviceC.o: KIMservice.o
KIMserviceF.o: KIMserviceC.o

clean:
	rm -f *.o *.a *.mod  KIMserviceTest  aKIMservice_test  tolowercase modelsconfig testsconfig RUN/* 

cleanall:
	rm -f listmodelname.txt listtestname.txt model_obj_lib.mk testsmake.mk testsmake.mk model_init_include.h model_init_include.cpp


