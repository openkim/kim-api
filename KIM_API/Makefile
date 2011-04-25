
#                                                                      
# Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
# All rights reserved.                                                 
#                                                                     
# Author: Valeriu Smirichinski                                         
#

include Include.mk



all: aKIMservice_test KIMserviceTest $(ALLOBJ)

ifdef NO_FORTRAN
aKIMservice_test:   $(ALLOBJ) 
	echo Skip fortran
else
aKIMservice_test:   aKIMservice_test.o  $(ALLOBJ)
	$(LINKCOMPILER)  $(CPPLIBFLAG)    $(ALLOBJ) aKIMservice_test.o $(OUTPUTIN) $@
	cp $@ RUN/$@
endif

KIMserviceTest: KIMserviceTest.o $(ALLOBJ)
	$(CPPCOMPILER) $(CPPFLAG) KIMserviceTest.o KIMservice.o  $(OUTPUTIN) $@
	cp $@ RUN/$@


#local dependencies
aKIMservice_test.o: $(ALLOBJ)
KIMserviceTest.o: $(ALLOBJ)
KIMserviceC.o: KIMservice.o
KIMserviceF.o: KIMserviceC.o

clean:
	rm -f *.o *.mod  KIMserviceTest  aKIMservice_test RUN/*

