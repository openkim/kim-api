//                                                                      
// Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna 
// All rights reserved.                                                 
//                                                                     
// Author: Valeriu Smirichinski                                         
// 

#include <stdlib.h>
#include <iostream>
#include <fstream>
//#include <cctype>


#include <string.h>


using namespace std;
#include "KIMservice.h"
#include "KIMserviceC.h"

//global methods
int KIM_API_init(void * kimmdl, char *testname, char *mdlname){
    KIM_API_model * mdl;
    mdl = new KIM_API_model[1];
    if(mdl->init(testname,mdlname)) {
        *(KIM_API_model **)kimmdl = mdl;
        return 1;
    }
    *(KIM_API_model **)kimmdl=NULL;
    return 0;
}
int KIM_API_init1(void * kimmdl, char * testinputf,char * testname, char * mdlinputf,char *mdlname){
    KIM_API_model * mdl;
    mdl = new KIM_API_model[1];
    if(mdl->init(testinputf,testname,mdlinputf,mdlname)) {
        *(KIM_API_model **)kimmdl = mdl;
        return 1;
    }
    *(KIM_API_model **)kimmdl=NULL;
    return 0;
}
 
 void KIM_API_allocate(void *kimmdl, intptr_t natoms, int ntypes){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->allocateinitialized(mdl,natoms,ntypes);
 }
void KIM_API_free(void *kimmdl){
    KIM_API_model * mdl=*(KIM_API_model **) kimmdl;
    if (mdl==NULL) return;
    mdl->free();
    delete [] mdl;
    *(KIM_API_model **) kimmdl=NULL;
}
void KIM_API_print(void *kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if (mdl==NULL) return;
    cout<<(*mdl);
}
void KIM_API_model_compute(void *kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->model_compute();
}
void KIM_API_get_Units(void *kimmdl,char * UnitsSystem){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->get_units(UnitsSystem);
}
void KIM_API_get_originalUnits(void *kimmdl,char * UnitsSystem){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->get_originalUnits(UnitsSystem);
}
int KIM_API_set_Units(void *kimmdl,char * UnitsSystem){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
cout<<"UnitsSystem"<<":"<<endl;
    if(mdl->set_units(UnitsSystem)) return 1;
    return 0;
}
void KIM_API_transform_Units_to(void *kimmdl,char * UnitsSystem){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->transform_units_to(UnitsSystem);
}
int KIM_API_isUnitS_fixed(void *kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->is_unitsfixed()) return 1;
    return 0;
}

//element access methods by name
int  KIM_API_set_data(void *kimmdl,char *nm, intptr_t size, void *dt){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->set_data(nm,size,dt)) return 1;
    return 0;
}
void * KIM_API_get_data(void *kimmdl,char *nm){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_data(nm);
}

intptr_t KIM_API_get_size(void *kimmdl,char *nm){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return (*mdl)[nm].size;
}
intptr_t KIM_API_get_rank_shape(void *kimmdl,char *nm, int * shape){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_rank_shape(nm,shape);
}
void KIM_API_set2_compute(void *kimmdl,char *nm){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set2_compute(nm);
}
void KIM_API_set2_uncompute(void *kimmdl,char *nm){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set2_uncompute(nm);
}
int KIM_API_isit_compute(void *kimmdl,char *nm){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return (*mdl)[nm].flag->calculate;
}

// element access by Index (fast way)
int KIM_API_get_index(void *kimmdl,char *nm){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_index(nm);
}
void  KIM_API_set_data_byI(void *kimmdl,int I, intptr_t size, void *dt){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
      int c=1;
       (*mdl)[I].data = dt;
       (*mdl)[I].size = size;
        if ((*mdl)[I].rank > 1) {
            for (int i=1;i<(*mdl)[I].rank;i++) {
                c=c * (*mdl)[I].shape[i];
            }
            (*mdl)[I].shape[0] = size/c;
        }
        (*mdl)[I].flag->freeable = 1;
}
void * KIM_API_get_data_byI(void *kimmdl,int I){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return (*mdl)[I].data;
}

intptr_t KIM_API_get_size_byI(void *kimmdl,int I){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return (*mdl)[I].size;
}
intptr_t KIM_API_get_rank_shape_byI(void *kimmdl,int I, int * shape){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     if((*mdl)[I].rank == 0){
            return 0;
        }else if((*mdl)[I].rank ==1){
            shape[0] = (int)(*mdl)[I].size;
            return 1;
        }else if((*mdl)[I].rank>1){
            for (int i=0; i< (*mdl)[I].rank; i++) shape[i] =(*mdl)[I].shape[i];
            return (*mdl)[I].rank;
        }else{
            return -1;
        }
}

void KIM_API_set2_compute_byI(void *kimmdl,int I){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    (*mdl)[I].flag->calculate =1;
}
void KIM_API_set2_uncompute_byI(void *kimmdl,int I){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    (*mdl)[I].flag->calculate = 0;
}
int KIM_API_isit_compute_byI(void *kimmdl,int I){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return (*mdl)[I].flag->calculate;
}

//Fortran interface
//global methots
int kim_api_init_(void * kimmdl,char ** testname, char **mdlname){
    return KIM_API_init(kimmdl,*testname,*mdlname);
}
int kim_api_init1_(void * kimmdl, char ** testinputf,char ** testname, char ** mdlinputf,char **mdlname){
    return KIM_API_init1(kimmdl,*testinputf,*testname,*mdlinputf,*mdlname);
}
void kim_api_allocate_(void *kimmdl, intptr_t *natoms, int *ntypes){
    KIM_API_allocate(*(KIM_API_model **)kimmdl,*natoms,*ntypes);
}
void kim_api_free_(void *kimmdl){
    KIM_API_free(kimmdl);
}
void kim_api_print_(void *kimmdl){
    KIM_API_print(*(KIM_API_model **)kimmdl);
}
void kim_api_model_compute_(void*kimmdl){
    KIM_API_model_compute(*(KIM_API_model **)kimmdl);
}


void kim_api_get_units_(void *kimmdl,char ** UnitsSystem){
    KIM_API_get_Units(*(KIM_API_model **)kimmdl,*UnitsSystem);
    for(int i=strlen(*UnitsSystem)+1;i<KEY_CHAR_LENGTH;i++){
        (*UnitsSystem)[i]='\0';
    }
}
void kim_api_get_originalunits_(void *kimmdl,char ** UnitsSystem){
    KIM_API_get_originalUnits(*(KIM_API_model **)kimmdl,*UnitsSystem);
    for(int i=strlen(*UnitsSystem)+1;i<KEY_CHAR_LENGTH;i++){
        (*UnitsSystem)[i]='\0';
    }
}
int kim_api_set_units_(void *kimmdl,char ** UnitsSystem){

    return KIM_API_set_Units(*(KIM_API_model **)kimmdl,*UnitsSystem);
}
void kim_api_transform_units_to_(void *kimmdl,char ** UnitsSystem){

    KIM_API_transform_Units_to(*(KIM_API_model **)kimmdl,*UnitsSystem);

}
int kim_api_isunits_fixed_(void *kimmdl){
    return KIM_API_isUnitS_fixed(*(KIM_API_model **)kimmdl);
}


//element access methods
int  kim_api_set_data_(void *kimmdl,char **nm,  intptr_t *size, void *dt){
    return KIM_API_set_data(*(KIM_API_model **)kimmdl,*nm,*size,*(char**)dt);
}


void * kim_api_get_data_(void *kimmdl,char **nm){
    return KIM_API_get_data(*(KIM_API_model **)kimmdl,*nm);
}
void * kim_api_get_data_cptr_(void *kimmdl,char **nm){
    return KIM_API_get_data(*(KIM_API_model **)kimmdl,*nm);
}

intptr_t kim_api_get_size_(void *kimmdl,char **nm){
    return KIM_API_get_size(*(KIM_API_model **)kimmdl,*nm);
}
intptr_t kim_api_get_rank_shape_(void *kimmdl,char **nm, int ** shape){
    return KIM_API_get_rank_shape(*(KIM_API_model **)kimmdl,*nm,*shape);
}

void kim_api_set2_compute_(void *kimmdl,char **nm){
    KIM_API_set2_compute(*(KIM_API_model **)kimmdl,*nm);
}
void kim_api_set2_uncompute_(void *kimmdl,char **nm){
    KIM_API_set2_uncompute(*(KIM_API_model **)kimmdl,*nm);
}
int kim_api_isit_compute_(void *kimmdl,char **nm){
    return KIM_API_isit_compute(*(KIM_API_model **)kimmdl,*nm);
}

int kim_api_get_index_(void *kimmdl,char **nm){
    return KIM_API_get_index(*(KIM_API_model **)kimmdl,*nm);
}

void kim_api_set_data_byi_(void *kimmdl,int * I, intptr_t * size, void *dt){
    KIM_API_set_data_byI(*(KIM_API_model **)kimmdl,*I,*size,*(char**)dt);
}
void * kim_api_get_data_byi_(void *kimmdl,int * I){
    return KIM_API_get_data_byI(*(KIM_API_model **)kimmdl,*I);
}

intptr_t kim_api_get_size_byi_(void *kimmdl,int * I){
    return KIM_API_get_size_byI(*(KIM_API_model **)kimmdl,*I);
}
intptr_t kim_api_get_rank_shape_byi_(void *kimmdl,int * I, int ** shape){
    return KIM_API_get_rank_shape_byI(*(KIM_API_model **)kimmdl,*I,*shape);
}

void kim_api_set2_compute_byi_(void *kimmdl,int * I){
    KIM_API_set2_compute_byI(*(KIM_API_model **)kimmdl,*I);
}
void kim_api_set2_uncompute_byi_(void *kimmdl,int * I){
    KIM_API_set2_uncompute_byI(*(KIM_API_model **)kimmdl,*I);
}
int kim_api_isit_compute_byi_(void *kimmdl,int * I){
    return KIM_API_isit_compute_byI(*(KIM_API_model **)kimmdl,*I);
}
