/*
*
* CDDL HEADER START
*
* The contents of this file are subject to the terms of the Common Development
* and Distribution License Version 1.0 (the "License").
*
* You can obtain a copy of the license at
* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
* specific language governing permissions and limitations under the License.
*
* When distributing Covered Code, include this CDDL HEADER in each file and
* include the License file in a prominent location with the name LICENSE.CDDL.
* If applicable, add the following below this CDDL HEADER, with the fields
* enclosed by brackets "[]" replaced with your own identifying information:
*
* Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
*
* CDDL HEADER END
*

*
* Copyright (c) 2013--2017, Regents of the University of Minnesota.
* All rights reserved.
*
* Contributors:
*    Valeriu Smirichinski
*    Ryan S. Elliott
*    Ellad B. Tadmor
*
*/

/*
* Release: This file is part of the kim-api.git repository.
*/


#ifndef KIMHDR_OLD_KIM_API_C_H
#define KIMHDR_OLD_KIM_API_C_H

#include <stdint.h>
#include "old_KIM_API_Version.h"

#ifdef  __cplusplus
extern "C" {
#endif

typedef void (* func_ptr)();

/* global methods */
  int odl_KIM_API_get_version(const char** const version);
  int odl_KIM_API_get_version_major(int* const major);
  int odl_KIM_API_get_version_minor(int* const minor);
  int odl_KIM_API_get_version_patch(int* const patch);
  int odl_KIM_API_get_version_prerelease(const char** const prerelease);
  int odl_KIM_API_get_version_build_metadata(const char** const build_metadata);
  int odl_KIM_API_version_newer(const char* const versionA,
                                const char* const versionB,
                                int* const result);

  int old_KIM_API_get_version_model_major(void* kimmdl, int* const major);
  int old_KIM_API_get_version_model_minor(void* kimmdl, int* const minor);
  int old_KIM_API_get_version_simulator_major(void* kimmdl, int* const major);
  int old_KIM_API_get_version_simulator_minor(void* kimmdl, int* const minor);

int old_KIM_API_string_init(void * kimmdl, const char *siminputstring, const char * modelname);
void old_KIM_API_free(void *kimmdl,int * error);
void old_KIM_API_print(void *kimmdl,int *error);
int old_KIM_API_model_compute(void *kimmdl);
int old_KIM_API_model_init(void * kimmdl);
int old_KIM_API_get_model_kim_str_len(const char *modelname, int* const kimStringLen);
int old_KIM_API_get_model_kim_str(const char *modelname, const char** const kimString);
int old_KIM_API_model_destroy(void * kimmdl);
int old_KIM_API_model_reinit(void * kimmdl);


  int old_KIM_API_get_num_model_species(void* kimmdl, int* numberSpecies,
                                        int* maxStringLength);
  int old_KIM_API_get_model_species(void* kimmdl, const int index,
                                    const char** const speciesString);
  int old_KIM_API_get_num_sim_species(void* kimmdl, int* numberSpecies,
                                      int* maxStringLength);
  int old_KIM_API_get_sim_species(void* kimmdl, const int index,
                                  const char** const speciesString);

int old_KIM_API_get_species_code(void * kimmdl, const char* species, int * error);
void old_KIM_API_set_species_code(void * kimmdl, const char* species, int code, int * error);

  int old_KIM_API_get_num_params(void* kimmdl, int* numberParameters,
                                 int* maxStringLength);
  int old_KIM_API_get_parameter(void* kimmdl, const int index,
                                const char** const parameterString);

int old_KIM_API_get_neigh(void *kimmdl, int request, int *numnei, int **nei1part);

  int old_KIM_API_get_status_msg(const int status_code, const char** const status_msg);

int old_KIM_API_report_error(int ln,const char * fl,const char * usermsg,int error);

void old_KIM_API_set_model_buffer(void * kimmdl,void * ob, int * error);
void * old_KIM_API_get_model_buffer(void * kimmdl, int * error);
void old_KIM_API_set_sim_buffer(void * kimmdl,void * ob, int * error);
void * old_KIM_API_get_sim_buffer(void * kimmdl, int * error);

/* element access methods */
int  old_KIM_API_set_data(void *kimmdl,const char *nm,  intptr_t size, void *dt);
int  old_KIM_API_set_method(void *kimmdl,const char *nm,  intptr_t size, int const language, func_ptr dt);
void * old_KIM_API_get_data(void *kimmdl,const char *nm,int * error);
func_ptr old_KIM_API_get_method(void *kimmdl,const char *nm,int * const language, int * error);

intptr_t old_KIM_API_get_size(void *kimmdl,const char *nm, int * error);

void old_KIM_API_set_compute(void *kimmdl,const char *nm, int flag, int *error);
int old_KIM_API_get_compute(void *kimmdl,const char *nm,int *error);

int old_KIM_API_process_dEdr(void **kimmdl, double * dE, double * dr, double **dx,int *i, int *j);
int old_KIM_API_process_d2Edr2(void **kimmdl, double * dE, double ** dr, double **dx,int **i, int **j);
/* related to Unit_Handling */
double old_KIM_API_get_scale_conversion(const char *u_from,const char *u_to, int *error);
int    old_KIM_API_get_unit_handling(void *kimmdl,int *error);
char const * const old_KIM_API_get_unit_length(void *kimmdl, int *error);
char const * const old_KIM_API_get_unit_energy(void *kimmdl, int *error);
char const * const old_KIM_API_get_unit_charge(void *kimmdl, int *error);
char const * const old_KIM_API_get_unit_temperature(void *kimmdl, int *error);
char const * const old_KIM_API_get_unit_time(void *kimmdl, int *error);

double old_KIM_API_convert_to_act_unit(void * kimmdl,
                                       const char *length,
                                       const char *energy,
                                       const char * charge,
                                       const char *temperature,
                                       const char *time,
                                       double length_exponent,
                                       double energy_exponent,
                                       double charge_exponent,
                                       double temperature_exponent,
                                       double time_exponent,
                                       int *error);


/* multiple data set/get methods */

void old_KIM_API_setm_data(void *kimmdl, int *error, int numargs, ... );
void old_KIM_API_setm_method(void *kimmdl, int *error, int numargs, ... );
void old_KIM_API_getm_data(void *kimmdl, int *error,int numargs, ...);
void old_KIM_API_getm_method(void *kimmdl, int *error,int numargs, ...);
void old_KIM_API_setm_compute(void *kimmdl, int *error, int numargs, ...);
void old_KIM_API_getm_compute(void *kimmdl, int *error,int numargs, ...);

#ifdef  __cplusplus
}
#endif

#endif  /* KIMHDR_OLD_KIM_API_C_H */
