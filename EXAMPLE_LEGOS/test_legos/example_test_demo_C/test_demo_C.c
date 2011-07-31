/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */
/* Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor       */
/*                                                                      */


#include <stdlib.h>
#include <stdio.h>
#include "KIMserviceC.h"

/* Define prototypes for neighbor list handeling */
void neighobj_both_allocate_(intptr_t **);
void neighobj_both_deallocate_(intptr_t **);
void neighborscalculate_both_(intptr_t **,double**,intptr_t*,double*);
void set_kim_neighobj_index_(int *);
intptr_t * get_neigh_half_both_();
intptr_t * get_neigh_full_both_();


int main(){
	/* KIM API potiner declarations */
	void* pkim;
	double * penergy;
	double * pcutoff;
	long long * numberOfAtoms;
	double * x;
	double * f;
	int kimerr;
	intptr_t * neighObject;
	
	int mode=0,request=0,atom,numnei, **nei1atom;
        double **Rij;

	/* test  and model name */
	char testname[80] ="test_demo_C";
	char modelname[80] ="";  /* model name string */
        

	/* reading model name from the std input (screen)*/
	printf("input KIM model name:\n");
        scanf("%79s", modelname);
        /* Local declarations */
	char infile[256];
	double cutofeps;
	int i,id,ntypes,ind;
        intptr_t n;
        float t0,t1,t2;
	FILE*fl;

	/* Initialized KIM API object */
	if (KIM_API_init(&pkim, testname ,modelname)!=1) return -1;

	/* open input atomic configuration file */
        sprintf(infile,"%s%s%s",KIM_DIR_TESTS,testname,"/data/dumpval10.xyz");
	fl=fopen(&infile[0],"r");
        if (NULL==fl){
           printf("Can't open file: %s! Exiting...\n",infile);
           exit(-3);
        }
        
	/* read number of atoms in configuration */
	fscanf(fl,"%d",&n);
	ntypes = 1; /* one atomic species only */

	/* Allocate memory and associated it with the KIM API object */
	KIM_API_allocate(pkim,n,ntypes,&kimerr);
	
	/* Make local pointers point to allocated memory (in KIM API object) */
	penergy=(double *)KIM_API_get_data(pkim,"energy",&kimerr);
	pcutoff=(double *) KIM_API_get_data(pkim,"cutoff",&kimerr);
	numberOfAtoms = (long long *)KIM_API_get_data(pkim,"numberOfAtoms",&kimerr);  
	*numberOfAtoms = n;
	x  = (double *)KIM_API_get_data(pkim,"coordinates",&kimerr);
	f  = (double *)KIM_API_get_data(pkim,"forces",&kimerr);
        ind = KIM_API_get_index(pkim,"neighObject",&kimerr);

	/* Read in the atomic positions for all atoms */
	for(i=0; i<n;i++){
		fscanf(fl,"%d %f %f %f",&id,&t0,&t1,&t2);
		*(x+i*3+0)=t0;
                *(x+i*3+1)=t1;
                *(x+i*3+2)=t2;
	}
	/* close input file */
	fclose(fl);

	/* Setup neighbor list */
	neighobj_both_allocate_(&neighObject);
	set_kim_neighobj_index_(&ind);
	       
	/* calculate neighbor list for the configuration */
	cutofeps=2.1;
	neighborscalculate_both_(&neighObject,&x,&n,&cutofeps);	

	/* Inform KIM API object about neighbor list iterator and object */
	KIM_API_set_data(pkim,"get_half_neigh",1,(void*) get_neigh_half_both_());
	KIM_API_set_data(pkim,"neighObject",1,(void*) neighObject);
	

	/* READY to call Model Initiation routine */
	
	if (KIM_API_model_init(pkim) != 1) return -1;
               

	/* All setup finished -- ready to compute */

	/*  Call Model compute routine -- e.g., compute energy & force */

	KIM_API_model_compute(pkim,&kimerr);

	/* output KIM API object to screen (optional) */

	KIM_API_print(pkim,&kimerr);

	/* Print out energy and list of forces */
	printf("\n\n===============================================\n");
	printf("Energy = %10.5f\n",*penergy);
	printf("Forces:\n");
	for (i=0;i<n;i++) {
		printf("%10.5f, %10.5f, %10.5f \n",f[i*3+0],f[i*3+1],f[i*3+2]);
 	}

	/* clean up */
	KIM_API_model_destroy(pkim,&kimerr);
	neighobj_both_deallocate_(&neighObject);
	KIM_API_free(&pkim,&kimerr);
}
