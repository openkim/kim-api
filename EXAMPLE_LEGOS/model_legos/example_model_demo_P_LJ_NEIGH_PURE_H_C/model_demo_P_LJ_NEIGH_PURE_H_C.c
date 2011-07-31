/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */
/* Author: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor       */
/*                                                                      */

#include <stdlib.h>
#include <stdio.h>
#include "KIMserviceC.h"

/* Define prototypes for compute routine */

static void model_demo_P_LJ_NEIGH_PURE_H_C_calculate(void *km,int *);

/* static local variables */
static double *pcutoff, *penergy;
static long long *numberOfAtoms;

/* Model Initiation routine */
void model_demo_p_lj_neigh_pure_h_c_init_(void * km){
	/* cast pointer for KIM API object */	
	intptr_t * pkim = * ((intptr_t **)km);
	int kimerr;
	/* get  from pkim energy, cutoff and number of atoms */
	penergy       = (double*) KIM_API_get_data(pkim,"energy",&kimerr);
	if (kimerr!=1) {
		printf("error in energy: code %d\n",kimerr);
		exit(337);
	}
	pcutoff       = (double*) KIM_API_get_data(pkim,"cutoff",&kimerr);
	if (kimerr!=1) {
		printf("error in cutoff: code %d\n",kimerr);
		exit(337);
	}
	numberOfAtoms = (long long *) KIM_API_get_data(pkim,"numberOfAtoms",&kimerr);
	if (kimerr!=1) {
		printf("error in numberOfAtoms: code %d\n",kimerr);
		exit(337);
	}
	/* Provide KIM API object with function pointer of compute routine */
	KIM_API_set_data(pkim,"compute",1,(void*) &model_demo_P_LJ_NEIGH_PURE_H_C_calculate);
	/*  set  default cutoff*/
	*pcutoff = 1.8;
}

/* Computational core of LJ potential */
static void ljpotr (double rr,double *v,double *dvmr){
		double a,b,r1,r2,rm6,rm8;
		a=0.0002441; b=0.03125; // LJ parameters
		if (rr < 1.0e-16){
			printf("rr to small\n"); 
			exit(338);
		}
		r2=rr; rm6=1.0/(r2*r2*r2); rm8=rm6/r2;
		*v=(a*rm6 - b)*rm6; *dvmr = 6.0*rm8*(-2*rm6*a + b);
}



static void  model_demo_P_LJ_NEIGH_PURE_H_C_calculate(void *km,int *kimerror){
	/* kim related declaration */
	intptr_t * pkim = * ((intptr_t **)km);
	double * x;
	double * f;
	double * ea;
	intptr_t * neighObject;
	void (*nei_iterator)(void *,int **,int *,int *); //prototype for iterator
	/* local declaration */
	int retcode, mode,request,atom=0,numnei,i,j,jj; int*n1atom;
	double vij,dvmr,v,sumv,cutof,cut2,energycutof,r2,dv,*Rij;
	double xi[3],xj[3],dx[3],fij[3];
        int kimerr,f_flag,e_flag;
	/* get forces and coordinayes from kim */

	x  = (double*) KIM_API_get_data(pkim,"coordinates",&kimerr);
	if (kimerr!=1) {
		printf("error in coordinates\n");
		*kimerror = kimerr;
		return;
	}
	f  = (double *) KIM_API_get_data(pkim,"forces",&kimerr);
	if (kimerr!=1) {
		printf("error in forces\n");
		*kimerror = kimerr;
		return;
	}
	ea  = (double *) KIM_API_get_data(pkim,"energyPerAtom",&kimerr);
	if (kimerr!=1) {
		printf("error in energyPerAtom\n");
		*kimerror = kimerr;
		return;
	}
	f_flag=KIM_API_isit_compute(pkim,"forces",&kimerr);
	e_flag=KIM_API_isit_compute(pkim,"energyPerAtom",&kimerr);
	/* Ready to do energy and force computation */
	sumv=0.0; /* running total for energy */

	if (f_flag==1) for (i=0;i<(*numberOfAtoms)*3;i++) f[i]=0.0; /* initialize forces to zero */
	if (e_flag==1) for (i=0;i<(*numberOfAtoms);i++) ea[i]=0.0;/* energy per atom to zero*/

	numnei = 0; /* initialize the number of neighbors */
	//for (i=0;i<512;i++) nei1atom[i]=0.0;
	cut2 = (*pcutoff)*(*pcutoff);
	
	/* store energy of LJ at rcut in 'energycutof'. */
	/* to be used for shifting LJ so energy is zero at cutoff */
	ljpotr(cut2,&energycutof,&dvmr);
  
      
        mode=0; //iterator mode
	request=0;// reset neighbor iterator to begining
      	retcode=KIM_API_get_half_neigh(pkim,mode,request,&atom,&numnei,&n1atom,&Rij);
	if(retcode!=2){
		printf("model_demo_P_LJ_NEIGH_PURE_H_C_calculate: iterator get_half_neigh has not been reset successfully:retcode=  %d\n",retcode);
		*kimerror=retcode;
		return;
	}

    	
	retcode=1;
       	while(retcode==1){ /* loop over the iterator */
		
		mode=0; 
		request=1;//increment increment mode
		retcode=KIM_API_get_half_neigh(pkim,mode,request,&atom,&numnei,&n1atom,&Rij);

		if( retcode < 0 ){
			printf("model_demo_P_LJ_NEIGH_PURE_H_C_calculate: error in get_half_neigh: retcode=  %d atom = %d \n",retcode,atom);
			*kimerror=retcode;
			return;
			exit(retcode);
		}else if(retcode==0){
			*kimerror=1;
			break;
		}

		i = atom;
		xi[0] = x[(i)*3 +0]; xi[1] = x[(i)*3 +1]; xi[2] = x[(i)*3 +2];

		for( jj=0;jj< numnei;jj++){ /* loop over neighbors for current atom */
				
			j=n1atom[jj];
			xj[0] = x[(j)*3 +0]; xj[1] = x[(j)*3 +1]; xj[2] = x[(j)*3 +2];
			dx[0]=xi[0]-xj[0]; dx[1]=xi[1]-xj[1]; dx[2]=xi[2]-xj[2]; 
			r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
			if (r2 <= cut2) {
				ljpotr(r2,&vij,&dvmr);
				sumv = sumv + vij-energycutof;
				if (e_flag==1){
					ea[i]=ea[i]+(vij-energycutof)/2;
					ea[j]=ea[j]+(vij-energycutof)/2;
				}
                                if (f_flag==1){
					f[(i)*3 +0] = f[(i)*3 +0] - dvmr*dx[0];
					f[(i)*3 +1] = f[(i)*3 +1] - dvmr*dx[1];
					f[(i)*3 +2] = f[(i)*3 +2] - dvmr*dx[2];

					f[(j)*3 +0] = f[(j)*3 +0] + dvmr*dx[0];
					f[(j)*3 +1] = f[(j)*3 +1] + dvmr*dx[1];
					f[(j)*3 +2] = f[(j)*3 +2] + dvmr*dx[2];
                                }
			}
		}
	}
	*penergy = sumv; /* set energy in KIM API object */
	/* forces are already stored in KIM API object. */
}

