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
void sample_01_lj_cutoff_c_init_(intptr_t **);


int main(){
	/* KIM API POINTER DECLARATIONS */
	intptr_t * pkim 	= NULL;						// Pointer to the KIM Model object
	double * penergy 	= NULL;						// Pointer to the calculated energy
	double * LjCuttoff 	= NULL;					// Pointer to the cutoff length in LJ Potential
	double * LjParamA = NULL;						// Pointer to the parameter 'A' in the LJ Potential
	double * LjParamB	= NULL ;					// Pointer to the parameter 'B' in the LJ Potential
	intptr_t * numberOfAtoms	= NULL;		// Pointer to the number of atoms... ignore intptr_t
	double * atomPos	= NULL;						// Pointer to the vector of atomic positions
	double * force	= NULL;							// Pointer to the vector of forces
	intptr_t * neighObject	= NULL;			
	void (*model_compute)(intptr_t **); /* Defines function pointer to model compute routine */

	/* TEST AND MODEL NAME */
	char testname[80] ="Sample_01_compute_example_c";
	char modelname[80] ="Sample_01_lj_cutoff_c";  /* in the future will be passed as argument for the test */

  /* LOCAL DECLARATIONS */
	char infile[80] = "./data/dumpval10.xyz";		// File containts data on the number of atoms and their positions
	FILE	*fl = NULL;				// Pointer to "infile"	
	double cutoffeps = 2.1;	// Cutoff criteria for the neighbor list	
	int n,ntypes; 					// number and types of atoms
	int i; 									// Generic counter	

	/* Initialized KIM API object */
	KIM_API_init((void *)&pkim, testname ,modelname);

	/* open input atomic configuration file */
	fl=fopen(&infile[0],"r");
	/* read number of atoms in configuration */
	fscanf(fl,"%d",&n);
	ntypes = 1; /* one atomic species only */

	/* Allocate memory and associate it with the KIM API object */
	KIM_API_allocate((void*)pkim,(intptr_t)n,ntypes);
	
	/* Get local pointers point to allocated memory (in KIM API object) */
	penergy=(double *)KIM_API_get_data((void *)pkim,"energy");
	LjCuttoff=(double *) KIM_API_get_data((void *)pkim,"cutoff");
	LjParamA=(double *) KIM_API_get_data((void *)pkim,"LjParamA");
	LjParamB=(double *) KIM_API_get_data((void *)pkim,"LjParamB");
	numberOfAtoms = (intptr_t *)KIM_API_get_data(pkim,"numberOfAtoms");  
	atomPos  = (double *)KIM_API_get_data(pkim,"coordinates");
	force  = (double *)KIM_API_get_data(pkim,"forces");

	*numberOfAtoms = (intptr_t)n;

	/* Parameters for LJ Potential */
	*LjCuttoff=1.8;        
	*LjParamA = 0.0002441;
	*LjParamB = 0.03125;

	/* Read in the atomic positions for all atoms */
	
	for(i=0; i<n;i++){
		int atom_id;								// Atom id
		float pos_x, pos_y, pos_z;	// Position of the atom
		fscanf(fl,"%d %f %f %f",&atom_id,&pos_x,&pos_y,&pos_z);
		*(atomPos+i*3+0)=pos_x; 
		*(atomPos+i*3+1)=pos_y; 
		*(atomPos+i*3+2)=pos_z;
	}
	/* close input file */
	fclose(fl);

	/* Setup neighbor list */
	neighobj_allocate_(&neighObject);


	/* calculate neighbor list for the configuration */
	neighborscalculate_(&neighObject,&atomPos,&n,&cutoffeps);	
	/* Inform KIM API object about neighbor list iterator and object */
	KIM_API_set_data(pkim,"neighIterator",1,(void*) get_neigh_iterator_());
	KIM_API_set_data(pkim,"neighObject",1,(void*) neighObject);

	/* Call Model Initiation routine */
	sample_01_lj_cutoff_c_init_(&pkim);

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
		printf("%10.5f, %10.5f, %10.5f \n",force[i*3+0],force[i*3+1],force[i*3+2]);
 	}

	/* clean up */
	neighobj_deallocate_(&neighObject);
	KIM_API_free(&pkim);
}
