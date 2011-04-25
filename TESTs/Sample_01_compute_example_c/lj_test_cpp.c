/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */
/* Author: Valeriu Smirichinski                                         */
/*                                                                      */


#include <stdlib.h>
#include <stdio.h>
#include "KIMserviceC.h"

/* Define prototypes for neighbor list handeling */
void neighobj_allocate_(intptr_t **);
void neighobj_deallocate_(intptr_t **);
void neighborscalculate_(intptr_t **,double**,int*,double*);
intptr_t * get_neigh_iterator_();

/* Define prototypes for model initiation routines */
void sample_01_lj_cutoff_cpp_init_(intptr_t **);


int main(){
	/* KIM API potiner declarations */
	intptr_t * pkim;
	double * penergy;
	double * pcutoff;
	intptr_t * numberOfAtoms;
	double * x;
	double * f;
	intptr_t * neighObject;
	void (*model_compute)(intptr_t **); /* Defines function pointer to model compute routine */

	/* test  and model name */
	char testname[80] ="Sample_01_compute_example_c";
	char modelname[80] ="Sample_01_lj_cutoff_cpp";  /* in the future will be passed as argument for the test */

        /* Local declarations */
	char infile[80] = "./data/dumpval10.xyz";
	double cutofeps;
	int i,n,id,ntypes; float t0,t1,t2;
	FILE*fl;

	/* Initialized KIM API object */
	KIM_API_init((void *)&pkim, testname ,modelname);

	/* open input atomic configuration file */
	fl=fopen(&infile[0],"r");
	/* read number of atoms in configuration */
	fscanf(fl,"%d",&n);
	ntypes = 1; /* one atomic species only */

	/* Allocate memory and associated it with the KIM API object */
	KIM_API_allocate((void*)pkim,(intptr_t)n,ntypes);
	
	/* Make local pointers point to allocated memory (in KIM API object) */
	penergy=(double *)KIM_API_get_data((void *)pkim,"energy");
	pcutoff=(double *) KIM_API_get_data((void *)pkim,"cutoff");
	numberOfAtoms = (intptr_t *)KIM_API_get_data(pkim,"numberOfAtoms");  
	*numberOfAtoms = (intptr_t)n;
	x  = (double *)KIM_API_get_data(pkim,"coordinates");
	f  = (double *)KIM_API_get_data(pkim,"forces");

	/* Read in the atomic positions for all atoms */
	for(i=0; i<n;i++){
		fscanf(fl,"%d %f %f %f",&id,&t0,&t1,&t2);
		*(x+i*3+0)=t0; *(x+i*3+1)=t1; *(x+i*3+2)=t2;
	}
	/* close input file */
	fclose(fl);

	/* Setup neighbor list */
	neighobj_allocate_(&neighObject);
	/* Set calculation parameters */
	*pcutoff=1.8;        
	cutofeps=2.1;
	/* calculate neighbor list for the configuration */
	neighborscalculate_(&neighObject,&x,&n,&cutofeps);	
	/* Inform KIM API object about neighbor list iterator and object */
	KIM_API_set_data(pkim,"neighIterator",1,(void*) get_neigh_iterator_());
	KIM_API_set_data(pkim,"neighObject",1,(void*) neighObject);

	/* READY to call Model Initiation routine */
	sample_01_lj_cutoff_cpp_init_(&pkim);

	/* All setup finished -- ready to compute */

	/*  Call Model compute routine -- e.g., compute energy & force */
	KIM_API_model_compute(pkim);
	
	/* output KIM API object to screen (optional) */
	KIM_API_print(pkim);

	/* Print out energy and list of forces */
	printf("\n\n===============================================\n");
	printf("Energy = %10.5f\n",*penergy);
	printf("Forces:\n");
	for (i=0;i<n;i++) {
		printf("%10.5f, %10.5f, %10.5f \n",f[i*3+0],f[i*3+1],f[i*3+2]);
 	}

	/* clean up */
	neighobj_deallocate_(&neighObject);
	KIM_API_free(&pkim);
}


