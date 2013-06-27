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
* Copyright (c) 2013, Regents of the University of Minnesota.
* All rights reserved.
*
* Contributors:
*    Valeriu Smirichinski
*    Ryan S. Elliott
*    Ellad B. Tadmor
*
*/

/*
* Release: This file is part of the openkim-api.git repository.
*/


#ifndef KIMHDR_KIM_API_C_H
#define KIMHDR_KIM_API_C_H

#include <stdint.h>

#define KIM_KEY_STRING_LENGTH 64

#ifdef  __cplusplus
extern "C" {
#endif

typedef void (* func_ptr)();

/* global methods */
int KIM_API_init(void * kimmdl, const char *testname, const char *modelname);

int KIM_API_model_info(void * kimmdl, const char * mdlname);

int KIM_API_string_init(void * kimmdl, const char *testinputstring, const char * modelname);
void KIM_API_allocate(void *kimmdl, int natoms, int ntypes, int * error);
void KIM_API_free(void *kimmdl,int * error);
void KIM_API_print(void *kimmdl,int *error);
int KIM_API_model_compute(void *kimmdl);
int KIM_API_model_init(void * kimmdl);
char * KIM_API_get_model_kim_str(const char *modelname,int * error);
int KIM_API_model_destroy(void * kimmdl);
int KIM_API_model_reinit(void * kimmdl);



char * KIM_API_get_model_partcl_typs(void * kimmdl,int* nATypes, int * error);
char * KIM_API_get_test_partcl_typs(void * kimmdl,int* nATypes, int * error);
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
void KIM_API_set_test_buffer(void * kimmdl,void * ob, int * error);
void * KIM_API_get_test_buffer(void * kimmdl, int * error);

int KIM_API_is_half_neighbors(void *kimmdl,int * error);

/* element access methods */
int  KIM_API_set_data(void *kimmdl,const char *nm,  intptr_t size, void *dt);
int  KIM_API_set_method_data(void *kimmdl,const char *nm,  intptr_t size, func_ptr dt);
void * KIM_API_get_data(void *kimmdl,const char *nm,int * error);
func_ptr KIM_API_get_method_data(void *kimmdl,const char *nm,int * error);

intptr_t KIM_API_get_size(void *kimmdl,const char *nm, int * error);
intptr_t KIM_API_get_shape(void *kimmdl,const char *nm, int * shape, int *error);
void KIM_API_set_shape(void *kimmdl,const char *nm, int * shape, int rank,int *error);

void KIM_API_set_compute(void *kimmdl,const char *nm, int flag, int *error);
void KIM_API_set_compute_by_index(void *kimmdl, int I, int flag, int *error);
int KIM_API_get_compute(void *kimmdl,const char *nm,int *error);

int KIM_API_get_index(void *kimmdl,const char *nm, int * error);

int KIM_API_set_data_by_index(void *kimmdl,int I, intptr_t size, void *dt);
int KIM_API_set_method_data_by_index(void *kimmdl,int I, intptr_t size, func_ptr dt);
void * KIM_API_get_data_by_index(void *kimmdl,int I, int *error);
func_ptr KIM_API_get_method_data_by_index(void *kimmdl,int I, int *error);

intptr_t KIM_API_get_size_by_index(void *kimmdl,int I,int *error);
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
void KIM_API_setm_method_data(void *kimmdl, int *error, int numargs, ... );
void KIM_API_setm_data_by_index(void *kimmdl, int *error, int numargs, ... );
void KIM_API_setm_method_data_by_index(void *kimmdl, int *error, int numargs, ... );
void KIM_API_getm_data(void *kimmdl, int *error,int numargs, ...);
void KIM_API_getm_method_data(void *kimmdl, int *error,int numargs, ...);
void KIM_API_getm_data_by_index(void *kimmdl,int *error,int numargs, ...);
void KIM_API_getm_method_data_by_index(void *kimmdl,int *error,int numargs, ...);
void KIM_API_getm_index(void *kimmdl, int *error, int numargs, ...);
void KIM_API_setm_compute(void *kimmdl, int *error, int numargs, ...);
void KIM_API_setm_compute_by_index(void *kimmdl, int *error, int numargs, ...);
void KIM_API_getm_compute(void *kimmdl, int *error,int numargs, ...);
void KIM_API_getm_compute_by_index(void *kimmdl, int *error,int numargs, ...);

/* total ?? service routines */

/* fortran interface */
int kim_api_init_(void * kimmdl,char ** testname, char **mdlname);
int kim_api_model_info_(void * kimmdl,char ** mdlname);
int kim_api_string_init_(void * kimmdl, char **testinputstring, char ** modelname);
void kim_api_allocate_(void *kimmdl, int *natoms, int *ntypes, int *error);
void kim_api_free_(void *kimmdl, int * error);
void kim_api_print_(void *kimmdl,int * error);
int kim_api_model_compute_f_(void*kimmdl);

int kim_api_model_reinit_f_(void * kimmdl);
int kim_api_model_destroy_f_(void * kimmdl);

int kim_api_model_init_f_(void * kimmdl);

void * kim_api_get_model_kim_str_(char **modelname,int *ln,int *kimerr);

int kim_api_get_neigh_mode_f_(void *kimmdl,int *error);

void * kim_api_get_model_partcl_typs_f_(void * kimmdl,int* nATypes, int* error);
void * kim_api_get_test_partcl_typs_f_(void * kimmdl,int* nATypes, int* error);
void * kim_api_get_params_f_(void * kimmdl,int* nVpar, int* error);
void * kim_api_get_free_params_f_(void * kimmdl,int* nVpar, int* error);
void * kim_api_get_fixed_params_f_(void * kimmdl,int* nVpar, int* error);

void * kim_api_get_nbc_method_f_(void * kimmdl,int* error);

int kim_api_get_partcl_type_code_(void * kimmdl, char** atom, int * error);
void kim_api_set_partcl_type_code_(void * kimmdl, char** atom, int* code, int * error);

int kim_api_get_neigh_f_(void *kimmdl,int *mode,int *request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int kim_api_get_model_index_shift_f_(void * kimmdl);

void kim_api_set_model_buffer_f_(void * kimmdl,void * ob, int * ier);
void * kim_api_get_model_buffer_f_(void * kimmdl, int * ier);
void kim_api_set_test_buffer_f_(void * kimmdl,void * ob, int * ier);
void * kim_api_get_test_buffer_f_(void * kimmdl, int * ier);

int kim_api_is_half_neighbors_f_(void * kimmdl,int *ier);

/* element access methods */

int  kim_api_set_data_(void *kimmdl,char **nm,  intptr_t *size, void *dt);
int  kim_api_set_method_data_(void *kimmdl,char **nm,  intptr_t *size, func_ptr *dt);
void * kim_api_get_data_(void *kimmdl,char **nm, int *error);
func_ptr kim_api_get_method_data_(void *kimmdl,char **nm, int *error);

intptr_t kim_api_get_size_(void *kimmdl,char **nm,int *error);
intptr_t kim_api_get_shape_(void *kimmdl,char **nm, int ** shape, int *error);
void kim_api_set_shape_(void *kimmdl,char **nm, int ** shape, int * rank,int *error);

void kim_api_set_compute_(void *kimmdl,char **nm, int *flag, int *error);
void kim_api_set_compute_by_index_f_(void *kimmdl, int *I, int *flag, int *error);
int kim_api_get_compute_(void *kimmdl,char **nm, int *error);

int kim_api_get_index_(void *kimmdl,char**nm, int *error);

int kim_api_set_data_by_index_(void *kimmdl,int * I, intptr_t * size, void *dt);
int kim_api_set_method_data_by_index_(void *kimmdl,int * I, intptr_t * size, func_ptr *dt);
void * kim_api_get_data_by_index_(void *kimmdl,int * I, int *error);
func_ptr kim_api_get_method_data_by_index_(void *kimmdl,int * I, int *error);

intptr_t kim_api_get_size_by_index_(void *kimmdl,int * I, int *error);
intptr_t kim_api_get_shape_by_index_(void *kimmdl,int * I, int ** shape, int *error);

int kim_api_get_compute_by_index_(void *kimmdl,int * I, int *error);

void * kim_api_get_status_msg_f_(int * error);

int kim_api_report_error_(int * ln,char ** fl, char ** usermsg, int * ier);

int kim_api_process_dedr_f_(void **ppkim, double * dE, double * dr, double **dx, int *i, int *j);
int kim_api_process_d2edr2_f_(void **ppkim, double * dE, double ** dr, double **dx, int **i,int **j);

/* related to Unit_Handling */
double kim_api_get_scale_conversion_(char **u_from,char **u_to, int *error);
int    kim_api_get_unit_handling_f_(void *kimmdl,int *error);
char * kim_api_get_unit_length_f_(void *kimmdl, int *error);
char * kim_api_get_unit_energy_f_(void *kimmdl, int *error);
char * kim_api_get_unit_charge_f_(void *kimmdl, int *error);
char * kim_api_get_unit_temperature_f_(void *kimmdl, int *error);
char * kim_api_get_unit_time_f_(void *kimmdl, int *error);
double kim_api_convert_to_act_unit_(void * kimmdl,
                                char **length,
                                char **energy,
                                char **charge,
                                char **temperature,
                                char **time,
                                double *length_exponent,
                                double *energy_exponent,
                                double *charge_exponent,
                                double *temperature_exponent,
                                double *time_exponent,
                                int* kimerror);
#ifdef  __cplusplus
}
#endif

#endif  /* KIMHDR_KIM_API_C_H */
