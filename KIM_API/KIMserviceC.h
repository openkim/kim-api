//                                                                      
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
// All rights reserved.                                                 
//                                                                     
// Author: Valeriu Smirichinski                                         
// 

#ifndef _KIMSERVICC_H
#define	_KIMSERVICC_H

//#include "KIMservice.h"
#include <stdint.h>
//#define intptr_t long long  // for 64 bit machines
#ifdef	__cplusplus
extern "C" {
#endif
//global methods
int KIM_API_init(void * kimmdl, char *testname, char *modelname);
int KIM_API_init1(void * kimmdl, char * testinputf,char * testname, char * mdlinputf,char *mdlname);
void KIM_API_allocate(void *kimmdl, intptr_t natoms, int ntypes, int * error);
void KIM_API_free(void *kimmdl,int * error);
void KIM_API_print(void *kimmdl,int *error);
void KIM_API_model_compute(void *kimmdl,int *error);
int KIM_API_model_init(void * kimmdl);
void KIM_API_model_destroy(void * kimmdl,int * error);


void KIM_API_get_Units(void *kimmdl,char * UnitsSystem, int *error);
void KIM_API_get_originalUnits(void *kimmdl,char * UnitsSystem, int*error);
int KIM_API_set_Units(void *kimmdl,char * UnitsSystem);
void KIM_API_transform_Units_to(void *kimmdl,char * UnitsSystem, int *error);
int KIM_API_isUnitS_fixed(void *kimmdl);

void * KIM_API_get_listAtomsTypes(void * kimmdl,int* nATypes, int* error);
int KIM_API_get_aTypeCode(void * kimmdl, char* atom, int * error);

void * KIM_API_get_listParams(void * kimmdl,int* nVpar, int* error);
void * KIM_API_get_listFreeParams(void * kimmdl,int* nVpar, int* error);
void * KIM_API_get_listFixedParams(void * kimmdl,int* nVpar, int* error);

void * KIM_API_get_NBC_method(void *kimmdl,int * error);

int KIM_API_get_full_neigh(void *kimmdl,int mode,int request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int KIM_API_get_half_neigh(void *kimmdl,int mode,int request,
        int *atom, int *numnei, int **nei1atom, double **Rij);



//element access methods
int  KIM_API_set_data(void *kimmdl,char *nm,  intptr_t size, void *dt);
void * KIM_API_get_data(void *kimmdl,char *nm,int * error);

intptr_t KIM_API_get_size(void *kimmdl,char *nm, int *error);
intptr_t KIM_API_get_rank_shape(void *kimmdl,char *nm, int * shape, int *error);

void KIM_API_set2_compute(void *kimmdl,char *nm,int * error);
void KIM_API_set2_uncompute(void *kimmdl,char *nm,int *error);
int KIM_API_isit_compute(void *kimmdl,char *nm,int *error);

int KIM_API_get_index(void *kimmdl,char*nm, int * error);
float KIM_API_get_unit_scalefactor(void * kim, char*nm, int * error);
void KIM_API_set_data_byI(void *kimmdl,int I, intptr_t size, void *dt, int * error);
void * KIM_API_get_data_byI(void *kimmdl,int I, int *error);

intptr_t KIM_API_get_size_byI(void *kimmdl,int I,int *error);
intptr_t KIM_API_get_rank_shape_byI(void *kimmdl,int I, int * shape,int *error);

void KIM_API_set2_compute_byI(void *kimmdl,int I,int * error);
void KIM_API_set2_uncompute_byI(void *kimmdl,int I, int * error);
int KIM_API_isit_compute_byI(void *kimmdl,int I,int * error);

//total 30 methods

//fortran interface
int kim_api_init_(void * kimmdl,char ** testname, char **mdlname);
int kim_api_init1_(void * kimmdl, char ** testinputf,char ** testname, char ** mdlinputf,char **mdlname);
void kim_api_allocate_(void *kimmdl, intptr_t *natoms, int *ntypes, int *error);
void kim_api_free_(void *kimmdl, int * error);
void kim_api_print_(void *kimmdl,int * error);
void kim_api_model_compute_(void*kimmdl, int *error);

void kim_api_model_destroy_(void * kimmdl, int * error);

int kim_api_model_init_(void * kimmdl);

void kim_api_get_units_(void *kimmdl,char ** UnitsSystem, int *error);
void kim_api_get_originalunits_(void *kimmdl,char ** UnitsSystem,int *error);
int kim_api_set_units_(void *kimmdl,char ** UnitsSystem);
void kim_api_transform_units_to_(void *kimmdl,char ** UnitsSystem, int *error);
int kim_api_isunits_fixed_(void *kimmdl);

void * kim_api_get_listatomstypes_(void * kimmdl,int* nATypes, int* error);
void * kim_api_get_listparams_(void * kimmdl,int* nVpar, int* error);
void * kim_api_get_listfreeparams_(void * kimmdl,int* nVpar, int* error);
void * kim_api_get_listfixedparams_(void * kimmdl,int* nVpar, int* error);

void * kim_api_get_nbc_method_(void * kimmdl,int* error);

int kim_api_get_atypecode_(void * kimmdl, char** atom, int * error);
int kim_api_get_full_neigh_(void *kimmdl,int *mode,int *request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

int kim_api_get_half_neigh_(void *kimmdl,int *mode,int *request,
        int *atom, int *numnei, int **nei1atom, double **Rij);

//element access methods
int  kim_api_set_data_(void *kimmdl,char **nm,  intptr_t *size, void *dt);
void * kim_api_get_data_(void *kimmdl,char **nm, int *error);
void * kim_api_get_data_cptr_(void *kimmdl,char **nm, int *error);

intptr_t kim_api_get_size_(void *kimmdl,char **nm,int *error);
intptr_t kim_api_get_rank_shape_(void *kimmdl,char **nm, int ** shape, int *error);
float kim_api_get_unit_scalefactor_(void * kim, char**nm, int *error);
void kim_api_set2_compute_(void *kimmdl,char **nm,int *error);
void kim_api_set2_uncompute_(void *kimmdl,char **nm, int *error);
int kim_api_isit_compute_(void *kimmdl,char **nm, int *error);

int kim_api_get_index_(void *kimmdl,char**nm, int *error);

void kim_api_set_data_byi_(void *kimmdl,int * I, intptr_t * size, void *dt, int *error);
void * kim_api_get_data_byi_(void *kimmdl,int * I, int *error);

intptr_t kim_api_get_size_byi_(void *kimmdl,int * I, int *error);
intptr_t kim_api_get_rank_shape_byi_(void *kimmdl,int * I, int ** shape, int *error);

void kim_api_set2_compute_byi_(void *kimmdl,int * I, int *error);
void kim_api_set2_uncompute_byi_(void *kimmdl,int * I, int *error);
int kim_api_isit_compute_byi_(void *kimmdl,int * I, int *error);



#ifdef	__cplusplus
}
#endif

#endif	/* _KIMSERVICC_H */
