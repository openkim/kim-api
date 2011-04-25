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
void KIM_API_allocate(void *kimmdl, intptr_t natoms, int ntypes);
void KIM_API_free(void *kimmdl);
void KIM_API_print(void *kimmdl);
void KIM_API_model_compute(void *kimmdl);


void KIM_API_get_Units(void *kimmdl,char * UnitsSystem);
void KIM_API_get_originalUnits(void *kimmdl,char * UnitsSystem);
int KIM_API_set_Units(void *kimmdl,char * UnitsSystem);
void KIM_API_transform_Units_to(void *kimmdl,char * UnitsSystem);
int KIM_API_isUnitS_fixed(void *kimmdl);


//element access methods
int  KIM_API_set_data(void *kimmdl,char *nm,  intptr_t size, void *dt);
void * KIM_API_get_data(void *kimmdl,char *nm);

intptr_t KIM_API_get_size(void *kimmdl,char *nm);
intptr_t KIM_API_get_rank_shape(void *kimmdl,char *nm, int * shape);

void KIM_API_set2_compute(void *kimmdl,char *nm);
void KIM_API_set2_uncompute(void *kimmdl,char *nm);
int KIM_API_isit_compute(void *kimmdl,char *nm);

int KIM_API_get_index(void *kimmdl,char*nm);

void KIM_API_set_data_byI(void *kimmdl,int I, intptr_t size, void *dt);
void * KIM_API_get_data_byI(void *kimmdl,int I);

intptr_t KIM_API_get_size_byI(void *kimmdl,int I);
intptr_t KIM_API_get_rank_shape_byI(void *kimmdl,int I, int * shape);

void KIM_API_set2_compute_byI(void *kimmdl,int I);
void KIM_API_set2_uncompute_byI(void *kimmdl,int I);
int KIM_API_isit_compute_byI(void *kimmdl,int I);

//total 25 methods

//fortran interface
int kim_api_init_(void * kimmdl,char ** testname, char **mdlname);
int kim_api_init1_(void * kimmdl, char ** testinputf,char ** testname, char ** mdlinputf,char **mdlname);
void kim_api_allocate_(void *kimmdl, intptr_t *natoms, int *ntypes);
void kim_api_free_(void *kimmdl);
void kim_api_print_(void *kimmdl);
void kim_api_model_compute_(void*kimmdl);

void kim_api_get_units_(void *kimmdl,char ** UnitsSystem);
void kim_api_get_originalunits_(void *kimmdl,char ** UnitsSystem);
int kim_api_set_units_(void *kimmdl,char ** UnitsSystem);
void kim_api_transform_units_to_(void *kimmdl,char ** UnitsSystem);
int kim_api_isunits_fixed_(void *kimmdl);


//element access methods
int  kim_api_set_data_(void *kimmdl,char **nm,  intptr_t *size, void *dt);
void * kim_api_get_data_(void *kimmdl,char **nm);
void * kim_api_get_data_cptr_(void *kimmdl,char **nm);

intptr_t kim_api_get_size_(void *kimmdl,char **nm);
intptr_t kim_api_get_rank_shape_(void *kimmdl,char **nm, int ** shape);

void kim_api_set2_compute_(void *kimmdl,char **nm);
void kim_api_set2_uncompute_(void *kimmdl,char **nm);
int kim_api_isit_compute_(void *kimmdl,char **nm);

int kim_api_get_index_(void *kimmdl,char**nm);

void kim_api_set_data_byi_(void *kimmdl,int * I, intptr_t * size, void *dt);
void * kim_api_get_data_byi_(void *kimmdl,int * I);

intptr_t kim_api_get_size_byi_(void *kimmdl,int * I);
intptr_t kim_api_get_rank_shape_byi_(void *kimmdl,int * I, int ** shape);

void kim_api_set2_compute_byi_(void *kimmdl,int * I);
void kim_api_set2_uncompute_byi_(void *kimmdl,int * I);
int kim_api_isit_compute_byi_(void *kimmdl,int * I);



#ifdef	__cplusplus
}
#endif

#endif	/* _KIMSERVICC_H */
