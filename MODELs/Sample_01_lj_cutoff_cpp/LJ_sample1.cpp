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
extern "C" {
//prototype for model initialization routine
void sample_01_lj_cutoff_cpp_init_(void * km);
}
//prptotype for model compute routine
void sample_01_lj_cutoff_cpp_calculate(void *km);

//Model initialization routine
void sample_01_lj_cutoff_cpp_init_(void * km){
	KIM_API_model ** ppkim=(KIM_API_model **)km;
	KIM_API_model * pkim = *ppkim;
	//Provide KIM API object with function pointer of compute routine
	if (!pkim->set_data("compute",1,(void*)&sample_01_lj_cutoff_cpp_calculate)) {
		cout<< ":compute: is not in kim "<<endl;
		exit(335);
	}
	
}
//Computational core of LJ potential
void ljpotr (double rr,double *v,double *dvmr){
		double a,b,r1,r2,rm6,rm8;
		a=0.0002441; b=0.03125; // LJ parameters
		if (rr < 1.0e-16){
			printf("rr to small\n");
			exit(338);
		}
		r2=rr; rm6=1.0/(r2*r2*r2); rm8=rm6/r2;
		*v=(a*rm6 - b)*rm6; *dvmr = 6.0*rm8*(-2*rm6*a + b);
}

//model compute routine
void sample_01_lj_cutoff_cpp_calculate(void *km){
	//kim related declaration
	KIM_API_model ** ppkim=(KIM_API_model **)km;
	KIM_API_model * pkim = *ppkim;
	double * penergy;
	double * pcutoff;
	intptr_t * numberOfAtoms;
	double * x;
	double * f;
	intptr_t * neighObject;  
        typedef void (*NEI_Iterator)(void *,int **,int *, int *);//prototype for iterator
        NEI_Iterator nei_iterator;
	
	// local declaration
	int nei1atom[512]; int restart,numnei,i,j,jj; int*n1atom=&nei1atom[0];
	double vij,dvmr,v,sumv,cutof,cut2,energycutof,r2,dv;
	double xi[3],xj[3],dx[3],fij[3];
	// get everything from kim
	penergy       = (double *) pkim->get_data("energy");
	pcutoff       = (double *)pkim->get_data("cutoff");
	numberOfAtoms = (intptr_t *)pkim->get_data("numberOfAtoms");
	x  = (double *)pkim->get_data("coordinates");
	f  = (double *)pkim->get_data("forces");
	neighObject  =(intptr_t *)pkim->get_data("neighObject");
	nei_iterator = (NEI_Iterator) pkim->get_data("neighIterator");

	//Ready to do energy and force computation
	sumv=0.0; 
	for (i=0;i<(*numberOfAtoms)*3;i++) f[i]=0.0;
	numnei = 0;
	for (i=0;i<512;i++) nei1atom[i]=0.0;
	cut2 = (*pcutoff)*(*pcutoff);
	// store energy of LJ at rcut in 'energycutof'
	// to be used for shifting LJ so energy is zero at cutoff
	ljpotr(cut2,&energycutof,&dvmr);              
   	        
        restart = 0; //reset neighbor iterator to beginning
        (*nei_iterator)(&neighObject,&n1atom,&numnei,&restart);

      
	restart=1;  //increment flag for neighbor iterator
       	while(numnei >= 0){ //loop over the iterator
		(*nei_iterator)(&neighObject,&n1atom,&numnei,&restart); //increment iterator
		i = nei1atom[1]-1;

		xi[0] = x[(i)*3 +0]; xi[1] = x[(i)*3 +1]; xi[2] = x[(i)*3 +2];

		for( jj=3-1;jj< nei1atom[0];jj++){
				
			j=nei1atom[jj]-1;
			xj[0] = x[(j)*3 +0]; xj[1] = x[(j)*3 +1]; xj[2] = x[(j)*3 +2];
			dx[0]=xi[0]-xj[0]; dx[1]=xi[1]-xj[1]; dx[2]=xi[2]-xj[2]; 
			r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
			if (r2 <= cut2) {
				ljpotr(r2,&vij,&dvmr);
				sumv = sumv + vij-energycutof;
				f[(i)*3 +0] = f[(i)*3 +0] - dvmr*dx[0];
				f[(i)*3 +1] = f[(i)*3 +1] - dvmr*dx[1];
				f[(i)*3 +2] = f[(i)*3 +2] - dvmr*dx[2];

				f[(j)*3 +0] = f[(j)*3 +0] + dvmr*dx[0];
				f[(j)*3 +1] = f[(j)*3 +1] + dvmr*dx[1];
				f[(j)*3 +2] = f[(j)*3 +2] + dvmr*dx[2];

			}
		}
	}
	v=sumv;
	*penergy = v;//set energy in KIM API object
	//forces are already stored in KIM API object

}

