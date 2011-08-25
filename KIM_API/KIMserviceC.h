
//
// Release: This file is part of the openkim-api.git repository.
//
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
// All rights reserved.
//
// Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
//

#ifndef _KIMSERVICC_H
#define	_KIMSERVICC_H

#ifndef KIM_API_MAX_NEIGHBORS
#define KIM_API_MAX_NEIGHBORS 512
#endif

#include <stdint.h>
//#include "KIMservice.h"
//#define intptr_t long long  // for 64 bit machines
#ifdef	__cplusplus
extern "C" {
#endif
//global methods
int KIM_API_init(void * kimmdl, char *testname, char *modelname);
int KIM_API_init1(void * kimmdl, char * testinputf,char * testname, char * mdlinputf,char *mdlname);
int KIM_API_init_str_testname(void * kimmdl, char *testinputstring, char * modelname);
void KIM_API_allocate(void *kimmdl, int natoms, int ntypes, int * error);
void KIM_API_free(void *kimmdl,int * error);
void KIM_API_print(void *kimmdl,int *error);
void KIM_API_model_compute(void *kimmdl,int *error);
int KIM_API_model_init(void * kimmdl);
void KIM_API_model_destroy(void * kimmdl,int * error);
int KIM_API_model_reinit(void * kimmdld);


void KIM_API_get_Units(void *kimmdl,char * UnitsSystem, int *error);
void KIM_API_get_originalUnits(void *kimmdl,char * UnitsSystem, int*error);
int KIM_API_set_Units(void *kimmdl,char * UnitsSystem);
void KIM_API_transform_Units_to(void *kimmdl,char * UnitsSystem, int *error);
int KIM_API_isUnitS_fixed(void *kimmdl);

void * KIM_API_get_listAtomTypes(void * kimmdl,int* nATypes, int* error);
int KIM_API_get_aTypeCode(void * kimmdl, char* atom, int * error);

void * KIM_API_get_listParams(void * kimmdl,int* nVpar, int* error);
void * KIM_API_get_listFreeParams(void * kimmdl,int* nVpar, int* error);
void * KIM_API_get_listFixedParams(void * kimmdl,int* nVpar, int* error);

void * KIM_API_get_NBC_method(void *kimmdl,int * error);

int KIM_API_get_full_neigh(void *kimmdl,int mode,int request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int KIM_API_get_half_neigh(void *kimmdl,int mode,int request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int KIM_API_get_neigh_mode(void *,int *);

char * KIM_API_status_msg(void *kimmdl,int error);
//element access methods
int  KIM_API_set_data(void *kimmdl,char *nm,  intptr_t size, void *dt);
void * KIM_API_get_data(void *kimmdl,char *nm,int * error);

intptr_t KIM_API_get_size(void *kimmdl,char *nm, int *error);
intptr_t KIM_API_get_rank_shape(void *kimmdl,char *nm, int * shape, int *error);
void KIM_API_set_rank_shape(void *kimmdl,char *nm, int * shape, int rank,int *error);

void KIM_API_set2_compute(void *kimmdl,char *nm,int * error);
void KIM_API_set2_donotcompute(void *kimmdl,char *nm,int *error);
int KIM_API_isit_compute(void *kimmdl,char *nm,int *error);

int KIM_API_get_index(void *kimmdl,char*nm, int * error);
float KIM_API_get_unit_scalefactor(void * kim, char*nm, int * error);
void KIM_API_set_data_byI(void *kimmdl,int I, intptr_t size, void *dt, int * error);
void * KIM_API_get_data_byI(void *kimmdl,int I, int *error);

intptr_t KIM_API_get_size_byI(void *kimmdl,int I,int *error);
intptr_t KIM_API_get_rank_shape_byI(void *kimmdl,int I, int * shape,int *error);

void KIM_API_set2_compute_byI(void *kimmdl,int I,int * error);
void KIM_API_set2_donotcompute_byI(void *kimmdl,int I, int * error);
int KIM_API_isit_compute_byI(void *kimmdl,int I,int * error);

//total 30 methods

//fortran interface
int kim_api_init_(void * kimmdl,char ** testname, char **mdlname);
int kim_api_init1_(void * kimmdl, char ** testinputf,char ** testname, char ** mdlinputf,char **mdlname);
int kim_api_init_str_testname_(void * kimmdl, char **testinputstring, char ** modelname);
void kim_api_allocate_(void *kimmdl, int *natoms, int *ntypes, int *error);
void kim_api_free_(void *kimmdl, int * error);
void kim_api_print_(void *kimmdl,int * error);
void kim_api_model_compute_f_(void*kimmdl, int *error);

int kim_api_model_reinit_f_(void * kimmdl);
void kim_api_model_destroy_f_(void * kimmdl, int * error);

int kim_api_model_init_f_(void * kimmdl);

void kim_api_get_units_(void *kimmdl,char ** UnitsSystem, int *error);
void kim_api_get_originalunits_(void *kimmdl,char ** UnitsSystem,int *error);
int kim_api_set_units_(void *kimmdl,char ** UnitsSystem);
void kim_api_transform_units_to_(void *kimmdl,char ** UnitsSystem, int *error);
int kim_api_isunits_fixed_(void *kimmdl);

int kim_api_get_neigh_mode_f_(void *kimmdl,int *error);

void * kim_api_get_listatomtypes_f_(void * kimmdl,int* nATypes, int* error);
void * kim_api_get_listparams_f_(void * kimmdl,int* nVpar, int* error);
void * kim_api_get_listfreeparams_f_(void * kimmdl,int* nVpar, int* error);
void * kim_api_get_listfixedparams_f_(void * kimmdl,int* nVpar, int* error);

void * kim_api_get_nbc_method_f_(void * kimmdl,int* error);

int kim_api_get_atypecode_(void * kimmdl, char** atom, int * error);
int kim_api_get_full_neigh_f_(void *kimmdl,int *mode,int *request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int kim_api_get_half_neigh_f_(void *kimmdl,int *mode,int *request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

//element access methods
int  kim_api_set_data_(void *kimmdl,char **nm,  intptr_t *size, void *dt);
void * kim_api_get_data_(void *kimmdl,char **nm, int *error);
void * kim_api_get_data_cptr_(void *kimmdl,char **nm, int *error);

intptr_t kim_api_get_size_(void *kimmdl,char **nm,int *error);
intptr_t kim_api_get_rank_shape_(void *kimmdl,char **nm, int ** shape, int *error);
void kim_api_set_rank_shape_(void *kimmdl,char **nm, int ** shape, int * rank,int *error);

float kim_api_get_unit_scalefactor_(void * kim, char**nm, int *error);
void kim_api_set2_compute_(void *kimmdl,char **nm,int *error);
void kim_api_set2_donotcompute_(void *kimmdl,char **nm, int *error);
int kim_api_isit_compute_(void *kimmdl,char **nm, int *error);

int kim_api_get_index_(void *kimmdl,char**nm, int *error);

void kim_api_set_data_byi_(void *kimmdl,int * I, intptr_t * size, void *dt, int *error);
void * kim_api_get_data_byi_(void *kimmdl,int * I, int *error);

intptr_t kim_api_get_size_byi_(void *kimmdl,int * I, int *error);
intptr_t kim_api_get_rank_shape_byi_(void *kimmdl,int * I, int ** shape, int *error);

void kim_api_set2_compute_byi_(void *kimmdl,int * I, int *error);
void kim_api_set2_donotcompute_byi_(void *kimmdl,int * I, int *error);
int kim_api_isit_compute_byi_(void *kimmdl,int * I, int *error);

void * kim_api_status_msg_f_(void*kimmdl,int * error);



#ifdef	__cplusplus
}
#endif

#endif	/* _KIMSERVICC_H */
