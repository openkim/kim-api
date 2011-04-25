#include <stdlib.h>
#include <stdio.h>
#include "KIMserviceC.h"

struct data {
    int cur_n;
    int n;
    double *x;
    double cutoff;
};

void neighobj_allocate_(intptr_t **ptr) {
    *ptr = (intptr_t *)malloc(sizeof(struct data));
    return;
}

void neighobj_deallocate_(intptr_t **ptr) {
    free(*ptr);
    *ptr = NULL;
    return;
}

void neighborscalculate_(intptr_t **ptr,double **x,int *n,double *cutofeps) {
    struct data *d = (struct data *)*ptr;
    d->n = *n;
    d->x = *x;
    d->cutoff = *cutofeps;
    d->cur_n = 0;
}

void neighborsiterator(intptr_t **ptr,int **x_list,int *n_items,int *restart) {
    struct data *d = (struct data *)*ptr;
    int n = d->n;
    int j;
	double xi[3],xj[3],dx[3],r2;
    if (*restart==0) {
        d->cur_n = 0;
        return;
    }
    double *x = d->x;
    int i = d->cur_n;
    double cut2 = d->cutoff * d->cutoff;

    int cnt = 2; 
    (*x_list)[1] = i+1;
    xi[0] = x[(i)*3 +0]; xi[1] = x[(i)*3 +1]; xi[2] = x[(i)*3 +2];
    for(j=i+1; j<n; j++) {
        xj[0] = x[(j)*3 +0]; xj[1] = x[(j)*3 +1]; xj[2] = x[(j)*3 +2];
        dx[0]=xi[0]-xj[0]; dx[1]=xi[1]-xj[1]; dx[2]=xi[2]-xj[2]; 

        r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
        if (r2 <= cut2) {
            (*x_list)[cnt++] = j+1;
        }
    }
    (*x_list)[0] = cnt;

    d->cur_n++;
    *n_items = 1;
    if (d->cur_n>n-1) {
        *n_items = -1;
    }
    return;
}

intptr_t * get_neigh_iterator_() {
    return (void *)neighborsiterator;
}

