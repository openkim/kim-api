/******************************************************************************************************
 *
 *  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
 *  All rights reserved.
 * 
 *  Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
 * 
 *****************************************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "KIMserviceC.h"

struct data {
    int cur_n;
    int n;
    double *x;
    double cutoff;
};

void neighobj_both_allocate_(intptr_t **ptr) {

    *ptr = (intptr_t *)malloc(sizeof(struct data));
    return;
}

void neighobj_both_deallocate_(intptr_t **ptr) {
    free(*ptr);
    *ptr = NULL;
    return;
}

void neighborscalculate_both_(intptr_t **ptr,double **x,int *n,double *cutofeps) {
    struct data *d = (struct data *)*ptr;
    d->n = *n;
    d->x = *x;
    d->cutoff = *cutofeps;
    d->cur_n = 0;
}



int nei_iteratorlocator(intptr_t **ptr,int *mode,int *request,int *atom,int *n_items,int **nei1atom,double **Rij) {
    struct data *d; //(struct data *)*ptr;
    int i,j,kimerror,cnt,n;
    static int numrun=0, neighObj_index;
    static int x_list[KIM_API_MAX_NEIGHBORS];
    double xi[3],xj[3],dx[3],r2,cut2;
    double *x;
    if(numrun==0){
	neighObj_index = KIM_API_get_index(*ptr,"neighObject",&kimerror);
	numrun++;
	if(kimerror!=1) return -3;
    }
    d=(struct data *) KIM_API_get_data_byI(*ptr,neighObj_index,&kimerror);
    if(kimerror!=1) return -3;
     x = d->x;
    if (*mode==0 && *request==0) {
        d->cur_n = 0;
        return 2;
    }
    n = d->n;
    cut2 = d->cutoff * d->cutoff;

 
    if(*mode==0 && *request==1){
    	if (d->cur_n>n-1) {
    	    return 0;
    	}
    	cnt = 0; 
	i = d->cur_n;
    	*atom = i+1;
    	xi[0] = x[(i)*3 +0]; xi[1] = x[(i)*3 +1]; xi[2] = x[(i)*3 +2];

    	for(j=i+1; j<n; j++) {
    	    xj[0] = x[(j)*3 +0]; xj[1] = x[(j)*3 +1]; xj[2] = x[(j)*3 +2];
    	    dx[0]=xi[0]-xj[0]; dx[1]=xi[1]-xj[1]; dx[2]=xi[2]-xj[2]; 

    	    r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
    	    if (r2 <= cut2) {
		if(cnt>KIM_API_MAX_NEIGHBORS) return -4;
    	        x_list[cnt++] = j+1;

    	    }
    	}

	if(cnt>KIM_API_MAX_NEIGHBORS) return -4;
    	*n_items = cnt;
    	d->cur_n++;
	*nei1atom = &x_list[0];
    	return 1;
    }else if(*mode==1){
	if (*request<1 || *request > d->n) return -1;
	i=*request - 1;
        *atom = *request;
	cnt=0;

	xi[0] = x[(i)*3 +0]; xi[1] = x[(i)*3 +1]; xi[2] = x[(i)*3 +2];
    	for(j=i+1; j<n; j++) {
    	    xj[0] = x[(j)*3 +0]; xj[1] = x[(j)*3 +1]; xj[2] = x[(j)*3 +2];
    	    dx[0]=xi[0]-xj[0]; dx[1]=xi[1]-xj[1]; dx[2]=xi[2]-xj[2]; 

    	    r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
    	    if (r2 <= cut2) {
		if(cnt>KIM_API_MAX_NEIGHBORS) return -4;
    	        x_list[cnt++] = j+1;
    	    }
    	}

	if(cnt>KIM_API_MAX_NEIGHBORS) return -4;
	*n_items = cnt;
	*nei1atom = &x_list[0];

	return 1;
    }else{
	return -2;
    }
}

void * get_neigh_half_both_() {
    return (void *)&nei_iteratorlocator;
}

void  set_kim_neighobj_index_(int * ind){
	return;//dummy program for compatibility with neighborlistmod.F90 interface
}

