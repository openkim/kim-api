//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2013, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//    Ryan S. Elliott
//    Ellad B. Tadmor
//

//
// Release: This file is part of the openkim-api.git repository.
//


#include <iostream>
#include <cstring>
#include <stdarg.h>
#include <stdint.h>

#include "KIM_API_C.h"
#include "KIM_API.h"
#include "KIM_API_status.h"

//global methods
int KIM_API_init(void * kimmdl, const char *testname, const char *mdlname){
    KIM_API_model * mdl;
    mdl = new KIM_API_model[1];
    if(mdl->init(testname,mdlname)) {
        *(KIM_API_model **)kimmdl = mdl;
        return KIM_STATUS_OK;
    }
    *(KIM_API_model **)kimmdl=NULL;
    return KIM_STATUS_FAIL;
}

int KIM_API_model_info(void * kimmdl, const char * mdlname){
    KIM_API_model * mdl;
    mdl = new KIM_API_model[1];
    if(mdl->model_info(mdlname)) {
        *(KIM_API_model **)kimmdl = mdl;
        return KIM_STATUS_OK;
    }
    *(KIM_API_model **)kimmdl=NULL;
    return KIM_STATUS_FAIL;
}

 int KIM_API_string_init(void * kimmdl, const char *testinputstring, const char * mdlname){
     KIM_API_model * mdl;
    mdl = new KIM_API_model[1];
    if(mdl->string_init(testinputstring,mdlname)) {
        *(KIM_API_model **)kimmdl = mdl;
        return KIM_STATUS_OK;
    }
    *(KIM_API_model **)kimmdl=NULL;
    return KIM_STATUS_FAIL;
 }

 void KIM_API_allocate(void *kimmdl, int natoms, int ntypes,int * error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->allocate(natoms,ntypes,error);
 }
void KIM_API_free(void *kimmdl,int * error){
    KIM_API_model * mdl=*(KIM_API_model **) kimmdl;
    *error=KIM_STATUS_OK;
    if (mdl==NULL) return;
    mdl->free(error);
    delete [] mdl;
    *(KIM_API_model **) kimmdl=NULL;

}
void KIM_API_print(void *kimmdl,int * error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error =KIM_STATUS_FAIL;
    if (mdl==NULL) return;
    std::cout<<(*mdl);
    *error=KIM_STATUS_OK;
}

int KIM_API_model_init(void * kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->model_init()) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}

char * KIM_API_get_model_kim_str(const char * modelname,int * kimerr){
    return KIM_API_model::get_model_kim_str(modelname,kimerr);
}

int KIM_API_model_compute(void *kimmdl){

    KIM_API_model * mdl=(KIM_API_model *) kimmdl;

    return mdl->model_compute();

}
int KIM_API_model_reinit(void * kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if (mdl->model_reinit()) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}
int KIM_API_model_destroy(void * kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->model_destroy();
}

char * KIM_API_get_model_partcl_typs(void * kimmdl,int* nATypes, int* error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_model_partcl_typs(nATypes,error);
}
char * KIM_API_get_test_partcl_typs(void * kimmdl,int* nATypes, int* error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_test_partcl_typs(nATypes,error);
}

char * KIM_API_get_params(void * kimmdl,int* nVpar, int* error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->get_params(nVpar,error);
}
char * KIM_API_get_free_params(void * kimmdl,int* nVpar, int* error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->get_free_params(nVpar,error);
}
char * KIM_API_get_fixed_params(void * kimmdl,int* nVpar, int* error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->get_fixed_params(nVpar,error);
}
char * KIM_API_get_NBC_method(void *kimmdl,int * error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->get_NBC_method(error);
}
int KIM_API_get_partcl_type_code(void * kimmdl, const char* atom, int * error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->get_partcl_type_code(atom,error);
}
void KIM_API_set_partcl_type_code(void * kimmdl, const char* atom, int code, int * error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->set_partcl_type_code(atom, code, error);
}



int KIM_API_get_neigh(void *kimmdl,int mode,int request,
        int *atom, int *numnei, int **nei1atom, double **Rij){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_neigh(mode,request,atom,numnei,nei1atom,Rij);
}

int KIM_API_process_dEdr(void **ppkim, double * dE, double * dr, double **dx, int *i, int *j){
   return KIM_API_model::process_dEdr((KIM_API_model **)ppkim,dE,dr,dx,i,j);
}
int KIM_API_process_d2Edr2(void **ppkim, double * dE, double ** dr, double **dx,int **i, int **j){
   return KIM_API_model::process_d2Edr2((KIM_API_model **)ppkim,dE,dr,dx,i,j);
 }
char * KIM_API_get_status_msg(int error){
    return KIM_API_model::get_status_msg(error);
}

int KIM_API_report_error(int ln, const char *fl,const char *usermsg,int ier){
    return KIM_API_model::report_error(ln,fl,usermsg,ier);
}

int KIM_API_get_model_index_shift(void *  kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_model_index_shift();
}

void KIM_API_set_model_buffer(void* kimmdl,void *ob,int * ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_model_buffer(ob,ier);
}
void * KIM_API_get_model_buffer(void* kimmdl, int* ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_model_buffer(ier);
}

void KIM_API_set_test_buffer(void* kimmdl,void *ob,int * ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_test_buffer(ob,ier);
}
void * KIM_API_get_test_buffer(void* kimmdl, int* ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_test_buffer(ier);
}

int KIM_API_is_half_neighbors(void *kimmdl,int *ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    int ans=1;
    *ier = KIM_STATUS_FAIL;
    if (!mdl->is_half_neighbors(ier)) ans = 0;
    *ier = KIM_STATUS_OK;
    return ans;
}

//element access methods by name
int  KIM_API_set_data(void *kimmdl,const char *nm, intptr_t size, void *dt){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->set_data(nm,size,dt)) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}
int  KIM_API_set_method_data(void *kimmdl,const char *nm, intptr_t size, func_ptr dt){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->set_method_data(nm,size,dt)) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}
void * KIM_API_get_data(void *kimmdl,const char *nm,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_data(nm,error);
}
func_ptr KIM_API_get_method_data(void *kimmdl,const char *nm,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_method_data(nm,error);
}

intptr_t KIM_API_get_size(void *kimmdl,const char *nm,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_size(nm,error);

}
intptr_t KIM_API_get_shape(void *kimmdl,const char *nm, int * shape, int *error){

    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_shape(nm,shape,error);

}

void KIM_API_set_shape(void *kimmdl,const char *nm, int * shape, int rank,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_shape(nm,shape,rank,error);
}

void KIM_API_set_compute(void *kimmdl,const char *nm, int flag, int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_compute(nm, flag, error);
}

void KIM_API_set_compute_by_index(void *kimmdl,int ind, int flag, int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_compute_by_index(ind, flag, error);
}

int KIM_API_get_compute(void *kimmdl,const char *nm, int * error){

    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     *error = KIM_STATUS_FAIL;
     int ind =mdl->get_index(nm, error);
     if (ind<0) return KIM_COMPUTE_FALSE;
    *error =KIM_STATUS_OK;
    return (*mdl)[ind].flag->calculate;
}

int KIM_API_get_neigh_mode(void * kimmdl,int * error){

     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     *error =  KIM_STATUS_FAIL;
     return mdl->get_neigh_mode(error);
}

// element access by Index (fast way)
int KIM_API_get_index(void *kimmdl,const char *nm, int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_index(nm,error);
}
int KIM_API_set_data_by_index(void *kimmdl,int I, intptr_t size, void *dt){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     int error = KIM_STATUS_FAIL;
     if (mdl == NULL) return error;

      int c=1;
       (*mdl)[I].data.p = dt;
       (*mdl)[I].size = size;
        if ((*mdl)[I].rank > 1) {
            for (int i=1;i<(*mdl)[I].rank;i++) {
                c=c * (*mdl)[I].shape[i];
            }
            (*mdl)[I].shape[0] = size/c;
        }
        (*mdl)[I].flag->freeable = 1;
        error=KIM_STATUS_OK;

        return error;
}
int KIM_API_set_method_data_by_index(void *kimmdl,int I, intptr_t size, func_ptr dt){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     int error = KIM_STATUS_FAIL;
     if (mdl == NULL) return error;

      int c=1;
       (*mdl)[I].data.fp = dt;
       (*mdl)[I].size = size;
        if ((*mdl)[I].rank > 1) {
            for (int i=1;i<(*mdl)[I].rank;i++) {
                c=c * (*mdl)[I].shape[i];
            }
            (*mdl)[I].shape[0] = size/c;
        }
        (*mdl)[I].flag->freeable = 1;
        error=KIM_STATUS_OK;

        return error;
}
void * KIM_API_get_data_by_index(void *kimmdl,int I,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error = KIM_STATUS_FAIL;
    if (mdl == NULL) return NULL;
    *error =KIM_STATUS_OK;
    return (*mdl)[I].data.p;
}
func_ptr KIM_API_get_method_data_by_index(void *kimmdl,int I,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error = KIM_STATUS_FAIL;
    if (mdl == NULL) return NULL;
    *error =KIM_STATUS_OK;
    return (*mdl)[I].data.fp;
}

intptr_t KIM_API_get_size_by_index(void *kimmdl,int I, int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error =KIM_STATUS_FAIL;
    if (mdl == NULL) return 0;
    *error =KIM_STATUS_OK;
    return (*mdl)[I].size;

}
intptr_t KIM_API_get_shape_by_index(void *kimmdl,int I, int * shape,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error =KIM_STATUS_OK;
    if (mdl == NULL) return -2;
    *error =1;
     if((*mdl)[I].rank == 0){
            return 0;
        }else if((*mdl)[I].rank ==1){
            shape[0] = (int)(*mdl)[I].size;
            return 1;
        }else if((*mdl)[I].rank>1){
            for (int i=0; i< (*mdl)[I].rank; i++) shape[i] =(*mdl)[I].shape[i];
            return (*mdl)[I].rank;
        }else{
            *error =KIM_STATUS_FAIL;
            return -1;
        }
}

int KIM_API_get_compute_by_index(void *kimmdl,int I,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_compute_by_index(I,error);
}



//multiple data set/get methods
//
void KIM_API_setm_data(void *kimmdl, int *err, int numargs, ... ){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"setm_data: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        void *dt      = va_arg(listPointer, void *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_data: WARNING: for "<<nm<<" data is NULL\n";
        if(!pkim->set_data(nm,size,dt)){
            std::cout<<"setm_data: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_setm_method_data(void *kimmdl, int *err, int numargs, ... ){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"setm_method_data: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        func_ptr dt      = va_arg(listPointer, func_ptr);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_method_data: WARNING: for "<<nm<<" data is NULL\n";
        if(!pkim->set_method_data(nm,size,dt)){
            std::cout<<"setm_method_data: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void KIM_API_setm_data_by_index(void *kimmdl, int *err, int numargs, ... ){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"setm_data_by_index: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        int ind      = va_arg(listPointer, int);
        intptr_t size = va_arg(listPointer, intptr_t);
        void *dt      = va_arg(listPointer, void *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_data_by_index: WARNING: for argument group "<<i<<" data is NULL\n";

        if(!pkim->set_data_by_index(ind,size,dt)){
            std::cout<<"setm_data_by_index: set data for argument group"<<i<<" failed\n";
            va_end(listPointer);
            return;
        }
    }
    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_setm_method_data_by_index(void *kimmdl, int *err, int numargs, ... ){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"setm_method_data_by_index: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        int ind      = va_arg(listPointer, int);
        intptr_t size = va_arg(listPointer, intptr_t);
        func_ptr dt      = va_arg(listPointer, func_ptr);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_method_data_by_index: WARNING: for argument group "<<i<<" data is NULL\n";

        if(!pkim->set_method_data_by_index(ind,size,dt)){
            std::cout<<"setm_method_data_by_index: set data for argument group"<<i<<" failed\n";
            va_end(listPointer);
            return;
        }
    }
    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_getm_data(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_data: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        void **dt      = va_arg(listPointer, void **);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = pkim->get_data(nm,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_data: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_getm_method_data(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_method_data: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        func_ptr *dt      = va_arg(listPointer, func_ptr *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = pkim->get_method_data(nm,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_method_data: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_getm_data_by_index(void *kimmdl,int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_data_by_index: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int ind      = va_arg(listPointer, int);
        void **dt      = va_arg(listPointer, void **);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = pkim->get_data_by_index(ind,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_data_by_index: get data for argument group "<<i<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_getm_method_data_by_index(void *kimmdl,int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_method_data_by_index: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int ind      = va_arg(listPointer, int);
        func_ptr *dt      = va_arg(listPointer, func_ptr *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = pkim->get_method_data_by_index(ind,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_method_data_by_index: get data for argument group "<<i<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_getm_index(void *kimmdl, int *err, int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
     *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);

    if(numargs % 3 != 0) {
        std::cout<<"getm_index: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *ind      = va_arg(listPointer, int *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *ind = pkim->get_index(nm,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_index: get index for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);

}
void KIM_API_setm_compute(void *kimmdl, int *err, int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"setm_compute: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int compute_flag = va_arg(listPointer, int);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        int index = pkim->get_index(nm,err);
        if (*err != KIM_STATUS_OK){
           std::cout<<"setm_compute:  name "<<nm<<" not in KIM\n";
           va_end(listPointer);
           return;
        }
        if (compute_flag ==1){
            (*pkim)[index].flag->calculate = 1;
        }else if (compute_flag ==0){
            (*pkim)[index].flag->calculate = 0;
        }else{
            std::cout<<"setm_compute:  for "<<nm<<" failed: compute_flag must be 0 or 1\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_setm_compute_by_index(void *kimmdl, int *err, int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
     *err=KIM_STATUS_OK;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"setm_compute_by_index: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int index      = va_arg(listPointer, int);
        int compute_flag = va_arg(listPointer, int);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if (index < 0 || index >= pkim->model.size) *err=KIM_STATUS_FAIL;
        if (*err != KIM_STATUS_OK){
           std::cout<<"setm_compute_by_index:  for argument group "<<i<<" failed\n";
           va_end(listPointer);
           return;
        }
        if (compute_flag ==1){
            (*pkim)[index].flag->calculate = 1;
        }else if (compute_flag ==0){
            (*pkim)[index].flag->calculate = 0;
        }else{
            std::cout<<"setm_compute_by_index:  for argument group "<<i<<" failed: compute_flag must be 0 or 1\n";
            *err=KIM_STATUS_FAIL;
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void KIM_API_getm_compute(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_compute: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *compute_flag = va_arg(listPointer, int*);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        int index = pkim->get_index(nm,err);
        if (*err != KIM_STATUS_OK){
           std::cout<<"getm_compute:  name "<<nm<<" not in KIM\n";
           va_end(listPointer);
           return;
        }
        *compute_flag =(*pkim)[index].flag->calculate;
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);

}
void KIM_API_getm_compute_by_index(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
     *err=KIM_STATUS_OK;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_compute_by_index: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        int index      = va_arg(listPointer, int);
        int *compute_flag = va_arg(listPointer, int*);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if (index < 0 || index >= pkim->model.size) *err=KIM_STATUS_FAIL;
        if (*err != KIM_STATUS_OK){
           std::cout<<"getm_compute_by_index:  for argument group "<<i<<" failed\n";
           va_end(listPointer);
           return;
        }
        *compute_flag = (*pkim)[index].flag->calculate;

    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}


//related to Unit_Handling
double KIM_API_get_scale_conversion(const char *u_from,const char *u_to, int *error){
    return KIM_API_model::get_scale_conversion(u_from,u_to,error);
}
int    KIM_API_get_unit_handling(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_handling(error);
}
char * KIM_API_get_unit_length(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_length(error);
}
char * KIM_API_get_unit_energy(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_energy(error);
}
char * KIM_API_get_unit_charge(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_charge(error);
}
char * KIM_API_get_unit_temperature(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_temperature(error);
}
char * KIM_API_get_unit_time(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_time(error);
}
double KIM_API_convert_to_act_unit(void * kimmdl,
                                const char *length,
                                const char *energy,
                                const char *charge,
                                const char *temperature,
                                const char *time,
                                double length_exponent,
                                double energy_exponent,
                                double charge_exponent,
                                double temperature_exponent,
                                double time_exponent,
                                int *kimerror){
    return ((KIM_API_model *)kimmdl)->convert_to_act_unit(length,energy,charge,temperature,time,
            length_exponent,energy_exponent,charge_exponent,temperature_exponent,time_exponent, kimerror);
}


//Fortran interface
//global methots
int kim_api_init_(void * kimmdl,char ** testname, char **mdlname){
    return KIM_API_init(kimmdl,*testname,*mdlname);
}
int kim_api_model_info_(void * kimmdl,char ** mdlname){
    return KIM_API_model_info(kimmdl,*mdlname);
}
int kim_api_string_init_(void * kimmdl, char **testinputstring, char ** mdlname){
    return KIM_API_string_init(kimmdl,*testinputstring,*mdlname);
}
void kim_api_allocate_(void *kimmdl, int *natoms, int *ntypes,int * error){
    KIM_API_allocate(*(KIM_API_model **)kimmdl,*natoms,*ntypes,error);
}

void kim_api_free_(void *kimmdl,int *error){
    KIM_API_free(kimmdl,error);
}
void kim_api_print_(void *kimmdl,int *error){
    KIM_API_print(*(KIM_API_model **)kimmdl, error);
}
int kim_api_model_compute_f_(void*kimmdl){
   return KIM_API_model_compute(*(KIM_API_model **)kimmdl);
}
int kim_api_get_neigh_mode_f_(void *kimmdl,int *error ){
    return KIM_API_get_neigh_mode(*(KIM_API_model **)kimmdl,error);
}

int kim_api_model_reinit_f_(void * kimmdl){
    return KIM_API_model_reinit(*(KIM_API_model **)kimmdl);
}
int kim_api_model_destroy_f_(void * kimmdl){
    return KIM_API_model_destroy(*(KIM_API_model **)kimmdl);
}
int kim_api_model_init_f_(void * kimmdl){
    return KIM_API_model_init(*(KIM_API_model **)kimmdl);
}

void * kim_api_get_model_kim_str_(char ** modelname, int *ln,int *kimerr){
    char * tmp =(char *)KIM_API_model::get_model_kim_str(*modelname,kimerr);
    *ln = (int)strlen(tmp);
    return (void *)tmp;
}






void * kim_api_get_model_partcl_typs_f_(void * kimmdl,int* nATypes, int* error){
 return KIM_API_get_model_partcl_typs(*(KIM_API_model **)kimmdl,nATypes,error);
}
void * kim_api_get_test_partcl_typs_f_(void * kimmdl,int* nATypes, int* error){
 return KIM_API_get_test_partcl_typs(*(KIM_API_model **)kimmdl,nATypes,error);
}

void * kim_api_get_params_f_(void * kimmdl,int* nVpar, int* error){
 return KIM_API_get_params(*(KIM_API_model **)kimmdl,nVpar,error);
}
void * kim_api_get_free_params_f_(void * kimmdl,int* nVpar, int* error){
 return KIM_API_get_free_params(*(KIM_API_model **)kimmdl,nVpar,error);
}
void * kim_api_get_fixed_params_f_(void * kimmdl,int* nVpar, int* error){
 return KIM_API_get_fixed_params(*(KIM_API_model **)kimmdl,nVpar,error);
}
void * kim_api_get_nbc_method_f_(void * kimmdl,int* error){
    return KIM_API_get_NBC_method(*(KIM_API_model **)kimmdl,error);
}
int kim_api_get_partcl_type_code_(void * kimmdl, char **atom, int * error){
 return KIM_API_get_partcl_type_code(*(KIM_API_model **)kimmdl,*atom,error);
}
void kim_api_set_partcl_type_code_(void * kimmdl, char **atom, int* code, int * error){
 return KIM_API_set_partcl_type_code(*(KIM_API_model **)kimmdl,*atom,*code,error);
}

int kim_api_get_neigh_f_(void *kimmdl,int *mode,int *request,
        int *atom, int *numnei, int **nei1atom, double **Rij){
    return KIM_API_get_neigh(*(KIM_API_model **)kimmdl, *mode, *request,
            atom,numnei,nei1atom,Rij);
}

int kim_api_get_model_index_shift_f_(void *kimmdl){
    return KIM_API_get_model_index_shift(*(KIM_API_model **)kimmdl);
}

void kim_api_set_model_buffer_f_(void * kimmdl,void * ob, int * ier){
    KIM_API_set_model_buffer(*(KIM_API_model **)kimmdl, *(void **)ob, ier);
}

void * kim_api_get_model_buffer_f_(void * kimmdl, int * ier){
    return KIM_API_get_model_buffer(*(KIM_API_model **)kimmdl, ier);
}

void kim_api_set_test_buffer_f_(void * kimmdl,void * ob, int * ier){
    KIM_API_set_test_buffer(*(KIM_API_model **)kimmdl, *(void **)ob, ier);
}

void * kim_api_get_test_buffer_f_(void * kimmdl, int * ier){
    return KIM_API_get_test_buffer(*(KIM_API_model **)kimmdl, ier);
}

int kim_api_is_half_neighbors_f_(void * kimmdl,int *ier){
    return KIM_API_is_half_neighbors(*(KIM_API_model **)kimmdl, ier);
}
//element access methods
int  kim_api_set_data_(void *kimmdl,char **nm,  intptr_t *size, void *dt){
    return KIM_API_set_data(*(KIM_API_model **)kimmdl,*nm,*size,*(char**)dt);
}
int  kim_api_set_method_data_(void *kimmdl,char **nm,  intptr_t *size, func_ptr *dt){
    return KIM_API_set_method_data(*(KIM_API_model **)kimmdl,*nm,*size,*dt);
}

void * kim_api_get_data_(void *kimmdl,char **nm,int *error){
    return KIM_API_get_data(*(KIM_API_model **)kimmdl,*nm,error);
}
func_ptr kim_api_get_method_data_(void *kimmdl,char **nm,int *error){
    return KIM_API_get_method_data(*(KIM_API_model **)kimmdl,*nm,error);
}

intptr_t kim_api_get_size_(void *kimmdl,char **nm,int *error){
    return KIM_API_get_size(*(KIM_API_model **)kimmdl,*nm,error);
}

intptr_t kim_api_get_shape_(void *kimmdl,char **nm, int ** shape,int *error){
    return KIM_API_get_shape(*(KIM_API_model **)kimmdl,*nm,*shape,error);
}

void kim_api_set_shape_(void *kimmdl,char **nm, int ** shape, int * rank,int *error){
    KIM_API_set_shape(*(KIM_API_model **)kimmdl,*nm,*shape,*rank,error);
}
void kim_api_set_compute_(void *kimmdl,char **nm,int *flag,int *error){
   KIM_API_set_compute(*(KIM_API_model **)kimmdl,*nm,*flag,error);
}
void kim_api_set_compute_by_index_f_(void *kimmdl,int *ind,int *flag,int *error){
   KIM_API_set_compute_by_index(*(KIM_API_model **)kimmdl,*ind,*flag,error);
}
int kim_api_get_compute_(void *kimmdl,char **nm,int *error){
    return KIM_API_get_compute(*(KIM_API_model **)kimmdl,*nm,error);
}

int kim_api_get_index_(void *kimmdl,char **nm,int *error){
    return KIM_API_get_index(*(KIM_API_model **)kimmdl,*nm,error);
}

int kim_api_set_data_by_index_(void *kimmdl,int * I, intptr_t * size, void *dt){
   return KIM_API_set_data_by_index(*(KIM_API_model **)kimmdl,*I,*size,*(char**)dt);
}
int kim_api_set_method_data_by_index_(void *kimmdl,int * I, intptr_t * size, func_ptr *dt){
   return KIM_API_set_method_data_by_index(*(KIM_API_model **)kimmdl,*I,*size,*dt);
}
void * kim_api_get_data_by_index_(void *kimmdl,int * I,int *error){
    return KIM_API_get_data_by_index(*(KIM_API_model **)kimmdl,*I,error);
}
func_ptr kim_api_get_method_data_by_index_(void *kimmdl,int * I,int *error){
    return KIM_API_get_method_data_by_index(*(KIM_API_model **)kimmdl,*I,error);
}

intptr_t kim_api_get_size_by_index_(void *kimmdl,int * I,int *error){
    return KIM_API_get_size_by_index(*(KIM_API_model **)kimmdl,*I,error);
}
intptr_t kim_api_get_shape_by_index_(void *kimmdl,int * I, int ** shape,int *error){
    return KIM_API_get_shape_by_index(*(KIM_API_model **)kimmdl,*I,*shape,error);
}

int kim_api_get_compute_by_index_(void *kimmdl,int * I,int *error){
    return KIM_API_get_compute_by_index(*(KIM_API_model **)kimmdl,*I,error);
}

void * kim_api_get_status_msg_f_(int * error){
    return (void *) KIM_API_get_status_msg(*error);
}

int kim_api_report_error_(int * ln,char ** fl, char ** usermsg, int * ier){
    return KIM_API_report_error(*ln,*fl,*usermsg,*ier);
}

int kim_api_process_dedr_f_(void **ppkim, double * dE, double * dr, double **dx, int *i, int *j){
   return KIM_API_model::process_dEdr((KIM_API_model **)ppkim,dE,dr,dx,i,j);
}

int kim_api_process_d2edr2_f_(void **ppkim, double * dE, double ** dr, double **dx, int **i, int **j){
   return KIM_API_model::process_d2Edr2((KIM_API_model **)ppkim,dE,dr,dx,i,j);
}
double kim_api_get_scale_conversion_(char **u_from,char **u_to, int *error){
    return KIM_API_model::get_scale_conversion(*u_from,*u_to,error);
}
int    kim_api_get_unit_handling_f_(void *kimmdl, int *error){
    return (*((KIM_API_model **)kimmdl))->get_unit_handling(error);
}
char * kim_api_get_unit_length_f_(void *kimmdl, int *error){
    return (*((KIM_API_model **)kimmdl))->get_unit_length(error);
}
char * kim_api_get_unit_energy_f_(void *kimmdl, int *error){
    return (*((KIM_API_model **)kimmdl))->get_unit_energy(error);
}
char * kim_api_get_unit_charge_f_(void *kimmdl, int *error){
    return (*((KIM_API_model **)kimmdl))->get_unit_charge(error);
}
char * kim_api_get_unit_temperature_f_(void *kimmdl, int *error){
    return (*((KIM_API_model **)kimmdl))->get_unit_temperature(error);
}
char * kim_api_get_unit_time_f_(void *kimmdl, int *error){
    return (*((KIM_API_model **)kimmdl))->get_unit_time(error);
}
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
                                int* kimerror){
     return (*(KIM_API_model **)kimmdl)->convert_to_act_unit(*length,*energy,*charge,*temperature,*time,
            *length_exponent,*energy_exponent,*charge_exponent,*temperature_exponent,*time_exponent, kimerror);
}
