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
* Copyright (c) 2013--2014, Regents of the University of Minnesota.
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


#ifndef KIMHDR_KIM_API_C_H
#define KIMHDR_KIM_API_C_H

#include <stdint.h>

#define KIM_KEY_STRING_LENGTH 128

#ifdef  __cplusplus
extern "C" {
#endif

typedef void (* func_ptr)();

/* global methods */
int KIM_API_file_init(void * kimmdl, const char *simkimfile, const char *modelname);

int KIM_API_model_info(void * kimmdl, const char * mdlname);

int KIM_API_string_init(void * kimmdl, const char *siminputstring, const char * modelname);
void KIM_API_allocate(void *kimmdl, int natoms, int ntypes, int * error);
void KIM_API_free(void *kimmdl,int * error);
void KIM_API_print(void *kimmdl,int *error);
int KIM_API_model_compute(void *kimmdl);
int KIM_API_model_init(void * kimmdl);
char * KIM_API_get_model_kim_str(const char *modelname,int * error);
int KIM_API_model_destroy(void * kimmdl);
int KIM_API_model_reinit(void * kimmdl);



char * KIM_API_get_model_partcl_typs(void * kimmdl,int* nATypes, int * error);
char * KIM_API_get_sim_partcl_typs(void * kimmdl,int* nATypes, int * error);
int KIM_API_get_partcl_type_code(void * kimmdl, const char* atom, int * error);
void KIM_API_set_partcl_type_code(void * kimmdl, const char* atom, int code, int * error);

char * KIM_API_get_params(void * kimmdl,int* nVpar, int * error);
char * KIM_API_get_free_params(void * kimmdl,int* nVpar, int * error);
char * KIM_API_get_fixed_params(void * kimmdl,int* nVpar, int * error);

char * KIM_API_get_NBC_method(void *kimmdl,int * error);

int KIM_API_get_neigh(void *kimmdl,int mode,int request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int KIM_API_get_neigh_mode(void *kimmdl,int *error);

char * KIM_API_get_status_msg(int error);

int KIM_API_report_error(int ln,const char * fl,const char * usermsg,int error);

int KIM_API_get_model_index_shift(void *kimmdl);

void KIM_API_set_model_buffer(void * kimmdl,void * ob, int * error);
void * KIM_API_get_model_buffer(void * kimmdl, int * error);
void KIM_API_set_sim_buffer(void * kimmdl,void * ob, int * error);
void * KIM_API_get_sim_buffer(void * kimmdl, int * error);

int KIM_API_is_half_neighbors(void *kimmdl,int * error);

/* element access methods */
int  KIM_API_set_data(void *kimmdl,const char *nm,  intptr_t size, void *dt);
int  KIM_API_set_method(void *kimmdl,const char *nm,  intptr_t size, func_ptr dt);
void * KIM_API_get_data(void *kimmdl,const char *nm,int * error);
func_ptr KIM_API_get_method(void *kimmdl,const char *nm,int * error);

intptr_t KIM_API_get_size(void *kimmdl,const char *nm, int * error);
intptr_t KIM_API_get_rank(void *kimmdl,const char *nm, int * error);
intptr_t KIM_API_get_shape(void *kimmdl,const char *nm, int * shape, int *error);
void KIM_API_set_shape(void *kimmdl,const char *nm, int * shape, int rank,int *error);

void KIM_API_set_compute(void *kimmdl,const char *nm, int flag, int *error);
void KIM_API_set_compute_by_index(void *kimmdl, int I, int flag, int *error);
int KIM_API_get_compute(void *kimmdl,const char *nm,int *error);

int KIM_API_get_index(void *kimmdl,const char *nm, int * error);

int KIM_API_set_data_by_index(void *kimmdl,int I, intptr_t size, void *dt);
int KIM_API_set_method_by_index(void *kimmdl,int I, intptr_t size, func_ptr dt);
void * KIM_API_get_data_by_index(void *kimmdl,int I, int *error);
func_ptr KIM_API_get_method_by_index(void *kimmdl,int I, int *error);

intptr_t KIM_API_get_size_by_index(void *kimmdl,int I,int *error);
intptr_t KIM_API_get_rank_by_index(void *kimmdl,int I,int *error);
intptr_t KIM_API_get_shape_by_index(void *kimmdl,int I, int * shape,int *error);

int KIM_API_get_compute_by_index(void *kimmdl,int I,int * error);

int KIM_API_process_dEdr(void **kimmdl, double * dE, double * dr, double **dx,int *i, int *j);
int KIM_API_process_d2Edr2(void **kimmdl, double * dE, double ** dr, double **dx,int **i, int **j);
/* related to Unit_Handling */
double KIM_API_get_scale_conversion(const char *u_from,const char *u_to, int *error);
int    KIM_API_get_unit_handling(void *kimmdl,int *error);
char * KIM_API_get_unit_length(void *kimmdl, int *error);
char * KIM_API_get_unit_energy(void *kimmdl, int *error);
char * KIM_API_get_unit_charge(void *kimmdl, int *error);
char * KIM_API_get_unit_temperature(void *kimmdl, int *error);
char * KIM_API_get_unit_time(void *kimmdl, int *error);

double KIM_API_convert_to_act_unit(void * kimmdl,
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

void KIM_API_setm_data(void *kimmdl, int *error, int numargs, ... );
void KIM_API_setm_method(void *kimmdl, int *error, int numargs, ... );
void KIM_API_setm_data_by_index(void *kimmdl, int *error, int numargs, ... );
void KIM_API_setm_method_by_index(void *kimmdl, int *error, int numargs, ... );
void KIM_API_getm_data(void *kimmdl, int *error,int numargs, ...);
void KIM_API_getm_method(void *kimmdl, int *error,int numargs, ...);
void KIM_API_getm_data_by_index(void *kimmdl,int *error,int numargs, ...);
void KIM_API_getm_method_by_index(void *kimmdl,int *error,int numargs, ...);
void KIM_API_getm_index(void *kimmdl, int *error, int numargs, ...);
void KIM_API_setm_compute(void *kimmdl, int *error, int numargs, ...);
void KIM_API_setm_compute_by_index(void *kimmdl, int *error, int numargs, ...);
void KIM_API_getm_compute(void *kimmdl, int *error,int numargs, ...);
void KIM_API_getm_compute_by_index(void *kimmdl, int *error,int numargs, ...);

#ifdef  __cplusplus
}
#endif

#endif  /* KIMHDR_KIM_API_C_H */
