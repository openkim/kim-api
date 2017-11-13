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

VPATH = ../include
KIMINCLUDEFLAGS = -I../include

# Definition of fortran object files
OBJF = kim_argument_name_f.o \
       kim_callback_name_f.o \
       kim_charge_unit_f.o \
       kim_data_type_f.o \
       kim_energy_unit_f.o \
       kim_language_name_f.o \
       kim_length_unit_f.o \
       kim_log_f.o \
       kim_log_verbosity_module.o \
       kim_log_verbosity_f.o \
       kim_model_compute_f.o \
       kim_model_create_f.o \
       kim_model_destroy_f.o \
       kim_model_driver_create_f.o \
       kim_model_f.o \
       kim_model_null_handles.o \
       kim_model_refresh_f.o \
       kim_numbering_f.o \
       kim_sem_ver_f.o \
       kim_species_name_f.o \
       kim_support_status_f.o \
       kim_temperature_unit_f.o \
       kim_time_unit_f.o

##fortran on/of
#ifdef KIM_NO_FORTRAN
#   ALLOBJ = $(OBJCPP) $(OBJC)
#else
#   ALLOBJ = $(OBJCPP) $(OBJC) $(OBJF03)
#endif

#
# Main build settings and rules
#
.PHONY: all objects lib

all: objects lib

# add local Makefile to KIM_MAKE_FILES
KIM_MAKE_FILES += $(KIM_DIR)/fortran/include/Makefile.dependencies $(KIM_DIR)/fortran/src/Makefile

all: objects lib

objects: $(OBJF)
lib: lib-fortran-$(KIM_LIB_BUILD).a

lib-fortran-$(KIM_LIB_BUILD).a: $(OBJF)
	@printf "Creating... Fortran KIM API library....... $@.\n";
	$(QUELL)ar rcs $@ $(OBJF)

# Include file dependency lists
include ../include/Makefile.dependencies

# Source file dependencies
kim_argument_name_f.o: kim_argument_name_f.f90 kim_argument_name_module.mod kim_data_type_module.mod
kim_callback_name_f.o: kim_callback_name_f.f90 kim_callback_name_module.mod
kim_charge_unit_f.o: kim_charge_unit_f.f90 kim_charge_unit_module.mod
kim_data_type_f.o: kim_data_type_f.f90 kim_data_type_module.mod
kim_energy_unit_f.o: kim_energy_unit_f.f90 kim_energy_unit_module.mod
kim_language_name_f.o: kim_language_name_f.f90 kim_language_name_module.mod
kim_length_unit_f.o: kim_length_unit_f.f90 kim_length_unit_module.mod
kim_log_f.o: kim_log_f.f90 kim_log_verbosity_module.mod
kim_log_verbosity_f.o: kim_log_verbosity_f.f90 kim_log_verbosity_module.mod
kim_model_compute_f.o: kim_model_compute_f.f90 kim_model_compute_module.mod kim_argument_name_module.mod kim_callback_name_module.mod kim_log_verbosity_module.mod
kim_model_create_f.o: kim_model_create_f.f90 kim_model_create_module.mod kim_numbering_module.mod kim_language_name_module.mod kim_species_name_module.mod kim_argument_name_module.mod kim_support_status_module.mod kim_callback_name_module.mod kim_unit_system_module.mod kim_log_verbosity_module.mod
kim_model_destroy_f.o: kim_model_destroy_f.f90 kim_model_destroy_module.mod kim_log_verbosity_module.mod
kim_model_driver_create_f.o: kim_model_driver_create_f.f90 kim_model_driver_create_module.mod kim_numbering_module.mod kim_language_name_module.mod kim_species_name_module.mod kim_argument_name_module.mod kim_support_status_module.mod kim_callback_name_module.mod kim_unit_system_module.mod kim_log_verbosity_module.mod
kim_model_f.o: kim_model_f.f90 kim_model_module.mod kim_numbering_module.mod kim_unit_system_module.mod kim_argument_name_module.mod kim_support_status_module.mod kim_callback_name_module.mod kim_language_name_module.mod kim_species_name_module.mod kim_data_type_module.mod kim_log_verbosity_module.mod
kim_model_null_handles.o: kim_model_null_handles.cpp
kim_model_refresh_f.o: kim_model_refresh_f.f90 kim_model_refresh_module.mod kim_log_verbosity_module.mod
kim_numbering_f.o: kim_numbering_f.f90 kim_numbering_module.mod
kim_sem_ver_f.o: kim_sem_ver_f.f90 kim_sem_ver_module.mod
kim_species_name_f.o: kim_species_name_f.f90 kim_species_name_module.mod
kim_support_status_f.o: kim_support_status_f.f90 kim_support_status_module.mod
kim_temperature_unit_f.o: kim_temperature_unit_f.f90 kim_temperature_unit_module.mod
kim_time_unit_f.o: kim_time_unit_f.f90 kim_time_unit_module.mod


#
# Main clean settings and rules
#
.PHONY: clean

clean:
	@printf "Cleaning... fortran/src.\n"
	$(QUELL)rm -f *.o *.mod *.a

#
# Main install settings and rules
#
.PHONY: install dirs-install headers-install

install_includedir = $(dest_package_dir)/include
install_include_fortran = kim_argument_name_module.mod \
                          kim_callback_name_module.mod \
                          kim_charge_unit_module.mod \
                          kim_data_type_module.mod \
                          kim_energy_unit_module.mod \
                          kim_language_name_module.mod \
                          kim_length_unit_module.mod \
                          kim_log_module.mod \
                          kim_log_verbosity_module.mod \
                          kim_model_compute_module.mod \
                          kim_model_create_module.mod \
                          kim_model_destroy_module.mod \
                          kim_model_driver_create_module.mod \
                          kim_model_module.mod \
                          kim_model_refresh_module.mod \
                          kim_numbering_module.mod \
                          kim_sem_ver_module.mod \
                          kim_species_name_module.mod \
                          kim_support_status_module.mod \
                          kim_temperature_unit_module.mod \
                          kim_time_unit_module.mod \
                          kim_unit_system_module.mod
install_include_fd =  kim_model_compute_log_macros.fd \
                      kim_model_create_log_macros.fd \
                      kim_model_destroy_log_macros.fd \
                      kim_model_driver_create_log_macros.fd \
                      kim_model_refresh_log_macros.fd \
                      kim_version.fd

install: dirs-install headers-install

dirs-install:
	$(QUELL)$(INSTALL_PROGRAM) -d -m 0755 "$(install_includedir)"

headers-install:
	@printf "Installing...($(dest_package_dir))................................. fortran/include.\n"
        # Install include files
	$(QUELL)for fl in $(install_include_fortran); do $(INSTALL_PROGRAM) -m 0644 "$$fl" "$(install_includedir)/$$fl"; done
	$(QUELL)for fl in $(install_include_fd); do $(INSTALL_PROGRAM) -m 0644 "../include/$$fl" "$(install_includedir)/$$fl"; done

#
# Main uninstall settings and rules
#
.PHONY: uninstall headers-uninstall dirs-unistall

uninstall: headers-uninstall dirs-uninstall

headers-uninstall:
	@printf "Uninstalling...($(dest_package_dir))................................. fortran/include.\n"
        # Uninstall include files
	$(QUELL)for fl in $(install_include_fortran); do rm -f "$(install_includedir)/$$fl"; done
	$(QUELL)for fl in $(install_include_fd); do rm -f "$(install_includedir)/$$fl"; done

dirs-uninstall:
	$(QUELL)if test -d "$(install_includedir)"; then rmdir "$(install_includedir)" > /dev/null 2>&1 || true; fi
	$(QUELL)if test -d "$(dest_package_dir)"; then rmdir "$(dest_package_dir)" > /dev/null 2>&1 || true; fi
