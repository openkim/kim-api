/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */
/* Author: Valeriu Smirichinski                                         */
/*                                                                      */

#include <stdlib.h>
#include <stdio.h>
#include "KIMserviceC.h"

#define SQ_MIN_ATOMIC_DIST 1e-16
#define MAX_NEIGHBORS 512

/* PROTOTYPE DECLARATION */
void sample_01_lj_cutoff_c_init_(void * km);
void sample_01_lj_cutoff_c_calculate(void *km);

/* Model Initiation routine */
void sample_01_lj_cutoff_c_init_(void * km){
	/* cast pointer for KIM API object */	
	intptr_t * pkim = * ((intptr_t **)km);

	/* Provide KIM API object with function pointer of compute routine */
	KIM_API_set_data(pkim,"compute",1,(void*) &sample_01_lj_cutoff_c_calculate);
}

/* Computational core of LJ potential */
void ljpotr (double r2, double A, double B, double *v,double *dvmr){
	/*
		This routine computes the Lennard-Jones potential and force terms.
		double r2:	Square of distance between the atoms
		double A: 	Lennard-Jones parameter A
		double B: 	Lennard-Jones parameter B
		double *v: 	Pointer to be set to the calculated potential
		double *dvmr:	Pointer to be set to the calcualted force terms

		v = A/r^12 - B/r^6             (r2 = r^2, r = atomic distance)
		dvmr = -12A/r^14 + 6B/r^8
	*/

		// Diagnostics
		if (r2 < SQ_MIN_ATOMIC_DIST){
			printf("Distance between atoms (=%.2e) smaller than the minimum allowed distance (=%.2e)\n",r2,SQ_MIN_ATOMIC_DIST); 
			exit(338);
		}

		// compute relevant powers of the atomic distance
		double rm6,rm8;
		rm6=1.0/(r2*r2*r2); rm8=rm6/r2;

		// Calculate and return the potential and the force terms
		*v=(A*rm6 - B)*rm6; *dvmr = 6.0*rm8*(-2*rm6*A + B);
}


void sample_01_lj_cutoff_c_calculate(void *km){
	/* KIM API RELATED DECLARATIONS */

	
	// POINTERS SET BY THE TEST
	intptr_t * pkim = * ((intptr_t **)km);																					// Pointer to the KIM Object
	double * p_Lj_cutoff_dist       = (double*) KIM_API_get_data(pkim,"cutoff");		// Pointer to the LJ cut-off distance
	double * LjParamA      = (double*) KIM_API_get_data(pkim,"LjParamA");						// Pointer to the LJ parameter 'A'
	double * LjParamB      = (double*) KIM_API_get_data(pkim,"LjParamB");						// Pointer to the LJ parameter 'B'
	intptr_t * numberOfAtoms = (intptr_t*) KIM_API_get_data(pkim,"numberOfAtoms");	// Pointer to the number of atoms
	double * coord_list  = (double*) KIM_API_get_data(pkim,"coordinates");					// Pointer to the atomic positions
	intptr_t * neighObject  = (intptr_t*) KIM_API_get_data(pkim,"neighObject");			// Pointer to the neighbor object
	void (*nei_iterator)(void *,int **,int *,int *); 																//prototype for iterator
	nei_iterator = (void (*)(void *,int **,int *,int *)) KIM_API_get_data(pkim,"neighIterator");

	// POINTERS TO BE SET AFTER COMPUTING THE MODEL
	double * penergy = (double*) KIM_API_get_data(pkim,"energy");				// Pointer to set to the computed energy
	double * f  = (double *) KIM_API_get_data(pkim,"forces");						// Pointer to the inter-atomic forces

	/* local declaration */
	int nei1atom[MAX_NEIGHBORS]; 
	int restart,numnei,i,j,jj; 
	int *n1atom=&nei1atom[0];
	double sumv=0.0; /* running total for energy */
	double vij,dvmr,v,Lj_cutoff_dist,Lj_energy_cutoff,r2,dv;
	double coord_i[3],coord_j[3],dx[3],fij[3];
	
	// INITIALIZATIONS
	for (i=0;i<(*numberOfAtoms)*3;i++) f[i]=0.0; 		/* initialize forces to zero */
	for (i=0;i<MAX_NEIGHBORS;i++) nei1atom[i]=0.0;	/* initialize neighbor list to default*/
	
	/* store energy of LJ at rcut in 'Lj_energy_cutoff'. */
	/* to be used for shifting LJ so energy is zero at cutoff */
	Lj_cutoff_dist = (*p_Lj_cutoff_dist)*(*p_Lj_cutoff_dist);
	ljpotr(Lj_cutoff_dist,*LjParamA,*LjParamB,&Lj_energy_cutoff,&dvmr);  
   	        
	restart = 0; /* reset neighbor iterator to beginning */
	(*nei_iterator)(&neighObject,&n1atom,&numnei,&restart);
	restart=1; /* increment flag for neighbor iterator */

	numnei = 0; /* initialize the number of neighbors */

	// DONE INITIALIZATIONS. START COMPUTATIONS
    
	while(numnei >= 0)
	{ /* loop over the iterator */
		(*nei_iterator)(&neighObject,&n1atom,&numnei,&restart); /* increment iterator */
		
		i = nei1atom[1]-1; /* iterator is FORTRAN so, 1-based counting */

		// Extract the coordinates of the ith atom from the list of all coordinates
		coord_i[0] = coord_list[(i)*3 +0]; 
		coord_i[1] = coord_list[(i)*3 +1]; 
		coord_i[2] = coord_list[(i)*3 +2];

		for( jj=3-1;jj< nei1atom[0];jj++)
		{ /* loop over neighbors for current atom */
			j=nei1atom[jj]-1;
			// Extract the coordinates of the jth atom from the list of all coordinates
			coord_j[0] = coord_list[(j)*3 +0]; 
			coord_j[1] = coord_list[(j)*3 +1]; 
			coord_j[2] = coord_list[(j)*3 +2];

			// Compute square of the distance between the atoms
			dx[0]=coord_i[0]-coord_j[0]; 
			dx[1]=coord_i[1]-coord_j[1]; 
			dx[2]=coord_i[2]-coord_j[2]; 
			r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

			if (r2 <= Lj_cutoff_dist) 
			{
				// Invoke the Lennard-Jones Potential subroutine
				ljpotr(r2,*LjParamA, *LjParamB, &vij,&dvmr);	

				// Increment the potential energy
				sumv = sumv + vij-Lj_energy_cutoff;
				
				// Increment the forces on both atoms
				f[(i)*3 +0] = f[(i)*3 +0] - dvmr*dx[0];
				f[(i)*3 +1] = f[(i)*3 +1] - dvmr*dx[1];
				f[(i)*3 +2] = f[(i)*3 +2] - dvmr*dx[2];

				f[(j)*3 +0] = f[(j)*3 +0] + dvmr*dx[0];
				f[(j)*3 +1] = f[(j)*3 +1] + dvmr*dx[1];
				f[(j)*3 +2] = f[(j)*3 +2] + dvmr*dx[2];

			}
		}
	}
	*penergy = sumv; /* set energy in KIM API object */
	/* forces are already stored in KIM API object. */
}

