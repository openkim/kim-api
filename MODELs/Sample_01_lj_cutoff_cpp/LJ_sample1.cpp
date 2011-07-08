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
extern "C" {
//prototype for model initialization routine
void sample_01_lj_cutoff_cpp_init_(void * km);
}

//prototype for model compute routine & ljpotr as part of unnamed namespace 
// (in c it would be static global variables) 
// 
namespace {
void sample_01_lj_cutoff_cpp_calculate(void *km,int*);

void ljpotr (double rr,double *v,double *dvmr);

void sample_01_lj_cutoff_cpp_destroy_(void * km,int * kimerror){
	cout<<endl<<"sample_01_lj_cutoff_cpp_destroy_ (void*)  called!"<<endl;
}

}
//Model initialization routine
void sample_01_lj_cutoff_cpp_init_(void * km){
	KIM_API_model ** ppkim=(KIM_API_model **)km;
	KIM_API_model * pkim = *ppkim;
	int kimerr;
	double * pcutoff;
	//Provide KIM API object with function pointer of compute routine
	if (!pkim->set_data("compute",1,(void*)&sample_01_lj_cutoff_cpp_calculate)) {
		cout<< ":compute: is not in kim "<<endl;
		exit(335);
	}
	
	if (!pkim->set_data("destroy",1,(void*)&sample_01_lj_cutoff_cpp_destroy_)) {
		cout<< ":destroy: is not in kim "<<endl;
		exit(335);
	}
	/* get cutoff from pkim and set it to 1.8 default value */
	pcutoff       = (double*) KIM_API_get_data(pkim,"cutoff",&kimerr);
	if (kimerr!=1) {
		printf("error in cutoff: code %d\n",kimerr);
		exit(337);
	}
	*pcutoff = 1.8;
}

namespace {
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
void sample_01_lj_cutoff_cpp_calculate(void *km,int * kimerror){
	//kim related declaration
	KIM_API_model ** ppkim=(KIM_API_model **)km;
	KIM_API_model * pkim = *ppkim;
	double * penergy;
	double * pcutoff;
	long long * numberOfAtoms;
	double * x;
	double * f;
	double * ea;
	int kimerr;
	bool f_flag,e_flag;
	
	// local declaration
	int restart,numnei,i,j,jj,mode,request,atom=0,retcode, *n1atom;
	double vij,dvmr,v,sumv,cutof,cut2,energycutof,r2,dv,*Rij;
	double xi[3],xj[3],dx[3],fij[3];
	// get everything from kim
	penergy       = (double *) pkim->get_data("energy",kimerror); if(*kimerror != 1) return;
	pcutoff       = (double *)pkim->get_data("cutoff",kimerror);  if(*kimerror != 1) return;
	numberOfAtoms = (long long *)pkim->get_data("numberOfAtoms",kimerror); if(*kimerror != 1) return;
	x  = (double *)pkim->get_data("coordinates",kimerror);        if(*kimerror != 1) return;
	f  = (double *)pkim->get_data("forces",kimerror);             if(*kimerror != 1) return;
	ea  = (double *)pkim->get_data("energyPerAtom",kimerror);     if(*kimerror != 1) return;

        f_flag=pkim->isit_compute("forces");
	e_flag=pkim->isit_compute("energyPerAtom");

	//Ready to do energy and force computation
	sumv=0.0; 
	if (f_flag) for (i=0;i<(*numberOfAtoms)*3;i++) f[i]=0.0;
        if (e_flag) for (i=0;i<(*numberOfAtoms);i++) ea[i]=0.0;

	numnei = 0;
	cut2 = (*pcutoff)*(*pcutoff);

	// store energy of LJ at rcut in 'energycutof'
	// to be used for shifting LJ so energy is zero at cutoff
	ljpotr(cut2,&energycutof,&dvmr);              
 	        
	retcode =1;
	for(i=0;i < (int) *numberOfAtoms;i++){
		
		mode=1; request = i;

		retcode = pkim->get_half_neigh(mode,request,&atom,&numnei,&n1atom,&Rij);
		//if(!(retcode==1 || retcode==0)) {
		if(retcode!=1) {
			cout<<"sample_01_lj_cutoff_cpp_calculate: unsuccesful  get_half_neigh, retcode = "<<retcode<<endl;
			*kimerror = retcode;
			return;
		}

		xi[0] = x[(i)*3 +0]; xi[1] = x[(i)*3 +1]; xi[2] = x[(i)*3 +2];
		for( jj=0;jj<numnei;jj++){
				
			j=n1atom[jj];
			xj[0] = x[(j)*3 +0]; xj[1] = x[(j)*3 +1]; xj[2] = x[(j)*3 +2];
			dx[0]=xi[0]-xj[0]; dx[1]=xi[1]-xj[1]; dx[2]=xi[2]-xj[2]; 
			r2=dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];
			if (r2 <= cut2) {
				ljpotr(r2,&vij,&dvmr);
				sumv = sumv + vij-energycutof;
				if (e_flag){
					ea[i]=ea[i]+(vij-energycutof)/2;
					ea[j]=ea[j]+(vij-energycutof)/2;
				}
                                if (f_flag){
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
	v=sumv;
	*penergy = v;//set energy in KIM API object
	//forces are already stored in KIM API object

}
}

