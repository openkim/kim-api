/*******************************************************************************
*
*  model_Ne_P_MLJ_NEIGH_RVEC_F
*
*  Modified Lennard-Jones pair potential model for neon
*  (modified to have smooth cutoff)
*
*  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
*
*  Release: This file is part of the openkim-api.git repository.
*
*  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
*  All rights reserved.
*
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "KIM_API_C.h"
#include "KIM_API_status.h"

#define DIM 3
#define Ar  1

/* Define prototypes for model init */
void model_ne_p_mlj_neigh_rvec_f_init_(void* km);

/* Define prototypes for model reinit, compute, and destroy */
static void reinit(void* km);
static void destroy(void* km);
static void compute(void* km, int* ier);

static void pair(double* epsilon, double* sigma, double* A, double* B, double* C,
                 double R, double* phi, double* dphi, double* d2phi)
{
   /* local variables */
   double rsq;
   double sor;
   double sor6;
   double sor12;

   rsq   = R*R;
   sor   = *sigma/R;
   sor6  = pow(sor, 6.0);
   sor12 = sor6*sor6;

   *phi   =  4.0*(*epsilon)*(sor12 - sor6) + (*A)*rsq + (*B)*R + (*C);
   *dphi  = 24.0*(*epsilon)*(-2.0*sor12 + sor6)/R + 2.0*(*A)*R + (*B);
   *d2phi = 24.0*(*epsilon)*(26.0*sor12 - 7.0*sor6)/R + 2.0*(*A);

   return;
}

/* compute function */
static void compute(void* km, int* ier)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dphi;
   double d2phi;
   double dEidr;
   int i;
   int j;
   int jj;
   int k;
   int numOfAtomNeigh;
   int currentAtom;
   int comp_energy;
   int comp_force;
   int comp_particleEnergy;
   int comp_virial;

   int* nAtoms;
   int* nparticleTypes;
   int* atomTypes;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutnorm;
   double* A;
   double* B;
   double* C;
   double* sigmasq;
   double* cutsq;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   double* virial;
   int* neighListOfCurrentAtom;

   /* check to see if we have been asked to compute the forces, particleEnergy, energy and virial */
   KIM_API_getm_compute(pkim, ier, 4*3,
                        "energy",         &comp_energy,         1,
                        "forces",         &comp_force,          1,
                        "particleEnergy", &comp_particleEnergy, 1,
                        "virial",         &comp_virial,         1);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute", *ier);
      return;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data(pkim, ier, 17*3,
                     "numberOfParticles",   &nAtoms,         1,
                     "numberParticleTypes", &nparticleTypes, 1,
                     "atomTypes",           &atomTypes,      1,
                     "cutoff",              &cutoff,         1,
                     "PARAM_FREE_epsilon",  &epsilon,        1,
                     "PARAM_FREE_sigma",    &sigma,          1,
                     "PARAM_FIXED_cutnorm", &cutnorm,        1,
                     "PARAM_FIXED_A",       &A,              1,
                     "PARAM_FIXED_B",       &B,              1,
                     "PARAM_FIXED_C",       &C,              1,
                     "PARAM_FIXED_sigmasq", &sigmasq,        1,
                     "PARAM_FIXED_cutsq",   &cutsq,          1,
                     "coordinates",         &coords,         1,
                     "energy",              &energy,         (comp_energy==1),
                     "forces",              &force,          (comp_force==1),
                     "particleEnergy",      &particleEnergy, (comp_particleEnergy==1),
                     "virial",              &virial,         (comp_virial==1));
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", *ier);
      return;
   }

   /* Check to be sure that the atom types are correct */
   /**/
   *ier = KIM_STATUS_FAIL; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if (Ar != atomTypes[i])
      {
         KIM_API_report_error(__LINE__, __FILE__, "Wrong atomType", i);
         return;
      }
   }
   *ier = KIM_STATUS_OK; /* everything is ok */

   /* initialize potential energies, forces, and virial term */
   if (comp_particleEnergy)
   {
      for (i = 0; i < *nAtoms; ++i)
      {
         particleEnergy[i] = 0.0;
      }
   }
   else if (comp_energy)
   {
      *energy = 0.0;
   }

   if (comp_force)
   {
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }
   }

   if (comp_virial)
   {
      for (i = 0; i < 6; ++i)
      {
         virial[i] = 0.0;
      }
   }

   /* reset neighbor iterator */
   *ier = KIM_API_get_neigh(pkim, 0, 0, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", *ier);
      *ier = KIM_STATUS_FAIL;
      return;
   }

   /* Compute energy and forces */
   *ier = KIM_API_get_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   while (KIM_STATUS_OK == *ier)
   {
      i = currentAtom;
      
      /* loop over the neighbors of currentAtom */
      for (jj = 0; jj < numOfAtomNeigh; ++ jj)
      {
         j = neighListOfCurrentAtom[jj];
         
         /* compute square distance */
         Rsqij = 0.0;
         for (k = 0; k < DIM; ++k)
         {
            Rsqij += Rij[jj*DIM + k]*Rij[jj*DIM + k];
         }

         /* particles are interacting ? */
         if (Rsqij < *cutsq)
         {
            R = sqrt(Rsqij);
            /* compute pair potential */
            pair(epsilon, sigma, A, B, C, R, &phi, &dphi, &d2phi);

            /* compute dEidr -- regular contribution */
            dEidr = 0.5*dphi; 

            /* accumulate energy */
            if (comp_particleEnergy)
            {
               particleEnergy[currentAtom] += 0.5*phi;
            }
            else if (comp_energy)
            {
               *energy += 0.5*phi;
            }

            /* accumulate virial */
            if (comp_virial)
            {
               /* virial(i,j) = r(i)*r(j)*(dV/dr)/r */
	       virial[0] += Rij[jj*DIM + 0]*Rij[jj*DIM + 0]*dEidr/R;
	       virial[1] += Rij[jj*DIM + 1]*Rij[jj*DIM + 1]*dEidr/R;
	       virial[2] += Rij[jj*DIM + 2]*Rij[jj*DIM + 2]*dEidr/R;
	       virial[3] += Rij[jj*DIM + 1]*Rij[jj*DIM + 2]*dEidr/R;
	       virial[4] += Rij[jj*DIM + 0]*Rij[jj*DIM + 2]*dEidr/R;
	       virial[5] += Rij[jj*DIM + 0]*Rij[jj*DIM + 1]*dEidr/R;
            }

            /* accumulate force */
            if (comp_force)
            {
               for (k = 0; k < DIM; ++k)
               {
                  force[i*DIM + k] += dEidr*Rij[jj*DIM + k]/R;
                  force[j*DIM + k] -= dEidr*Rij[jj*DIM + k]/R;
               }
            }
         }
      }
      
      /* increment iterator */
      *ier = KIM_API_get_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   }


   /* perform final tasks */
   
   if (comp_particleEnergy && comp_energy)
   {
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         *energy += particleEnergy[i];
      }
   }

   /* everything is great */
   *ier = KIM_STATUS_OK;
   return;
}


/* Reinitialization function */
static void reinit(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_cutoff;
   double* model_epsilon;
   double* model_sigma;
   double* model_Pcutoff;
   double* model_cutnorm;
   double* model_A;
   double* model_B;
   double* model_C;
   double* model_sigmasq;
   double* model_cutsq;
   int ier;

   /* get (changed) parameters from KIM object */
   KIM_API_getm_data(pkim, &ier, 3*3,
                     "PARAM_FREE_sigma",   &model_sigma,   1,
                     "PARAM_FREE_epsilon", &model_epsilon, 1,
                     "PARAM_FREE_cutoff",  &model_Pcutoff, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      exit(1);
   }

   /* set new values in KIM object */
   KIM_API_getm_data(pkim, &ier, 7*3,
                     "cutoff",              &model_cutoff,  1,
                     "PARAM_FIXED_cutnorm", &model_cutnorm, 1,
                     "PARAM_FIXED_A",       &model_A,       1,
                     "PARAM_FIXED_B",       &model_B,       1,
                     "PARAM_FIXED_C",       &model_C,       1,
                     "PARAM_FIXED_sigmasq", &model_sigmasq, 1,
                     "PARAM_FIXED_cutsq",   &model_cutsq,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      exit(1);
   }

   *model_cutoff = *model_Pcutoff;
   *model_cutnorm = *model_cutoff / *model_sigma;
   *model_A = 12.0*(*model_epsilon)*(-26.0 + 7.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 14.0) * pow(*model_sigma, 2.0));
   *model_B = 96.0*(*model_epsilon)*(7.0 - 2.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 13.0) * (*model_sigma));
   *model_C = 28.0*(*model_epsilon)*(-13.0 + 4.0*pow(*model_cutnorm, 6.0)) /
      pow(*model_cutnorm, 12.0);
   *model_sigmasq = (*model_sigma)*(*model_sigma);
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

   return;
}

/* destroy function */
static void destroy(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_epsilon;
   double* model_sigma;
   double* model_Pcutoff;
   double* model_cutnorm;
   double* model_A;
   double* model_B;
   double* model_C;
   double* model_sigmasq;
   double* model_cutsq;
   int ier;

   /* get parameters from KIM object */
   KIM_API_getm_data(pkim, &ier, 9*3,
                     "PARAM_FREE_sigma",    &model_sigma,   1,
                     "PARAM_FREE_epsilon",  &model_epsilon, 1,
                     "PARAM_FREE_cutoff",   &model_Pcutoff, 1,
                     "PARAM_FIXED_cutnorm", &model_cutnorm, 1,
                     "PARAM_FIXED_A",       &model_A,       1,
                     "PARAM_FIXED_B",       &model_B,       1,
                     "PARAM_FIXED_C",       &model_C,       1,
                     "PARAM_FIXED_sigmasq", &model_sigmasq, 1,
                     "PARAM_FIXED_cutsq",   &model_cutsq,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      exit(1);
   }

   /* free parameter memory */
   free(model_sigma);
   free(model_epsilon);
   free(model_Pcutoff);
   free(model_cutnorm);
   free(model_A);
   free(model_B);
   free(model_C);
   free(model_sigmasq);
   free(model_cutsq);

   return;
}


/* Initialization function */
void model_ne_p_mlj_neigh_rvec_f_init_(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_cutoff;
   double* model_epsilon;
   double* model_sigma;
   double* model_Pcutoff;
   double* model_cutnorm;
   double* model_A;
   double* model_B;
   double* model_C;
   double* model_sigmasq;
   double* model_cutsq;
   int ier;

   /* store function pointers in KIM object */
   KIM_API_setm_data(pkim, &ier, 3*4,
                     "compute", 1, &compute, 1,
                     "reinit",  1, &reinit,  1,
                     "destroy", 1, &destroy, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);
      exit(1);
   }

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = 8.15; /* cutoff distance in angstroms */

   /* allocate memory for parameters */
   model_sigma = (double*) malloc(1*sizeof(double));
   if (NULL == model_sigma)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_epsilon = (double*) malloc(1*sizeof(double));
   if (NULL == model_epsilon)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_Pcutoff = (double*) malloc(1*sizeof(double));
   if (NULL == model_Pcutoff)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_cutnorm = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutnorm)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_A = (double*) malloc(1*sizeof(double));
   if (NULL == model_A)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_B = (double*) malloc(1*sizeof(double));
   if (NULL == model_B)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_C = (double*) malloc(1*sizeof(double));
   if (NULL == model_C)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_sigmasq = (double*) malloc(1*sizeof(double));
   if (NULL == model_sigmasq)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_cutsq = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutsq)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }

   /* store parameters in KIM object */
   KIM_API_setm_data(pkim, &ier, 9*4,
                     "PARAM_FREE_sigma",    1, model_sigma,   1,
                     "PARAM_FREE_epsilon",  1, model_epsilon, 1,
                     "PARAM_FREE_cutoff",   1, model_Pcutoff, 1,
                     "PARAM_FIXED_cutnorm", 1, model_cutnorm, 1,
                     "PARAM_FIXED_A",       1, model_A,       1,
                     "PARAM_FIXED_B",       1, model_B,       1,
                     "PARAM_FIXED_C",       1, model_C,       1,
                     "PARAM_FIXED_sigmasq", 1, model_sigmasq, 1,
                     "PARAM_FIXED_cutsq",   1, model_cutsq,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);
      exit(1);
   }

   *model_sigma = 2.74; /* LJ sigma in angstroms */
   *model_epsilon = 0.0031; /* LJ epsilon in eV */
   *model_Pcutoff = *model_cutoff;
   *model_cutnorm = *model_cutoff / *model_sigma;
   *model_A = 12.0*(*model_epsilon)*(-26.0 + 7.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 14.0) * pow(*model_sigma, 2.0));
   *model_B = 96.0*(*model_epsilon)*(7.0 - 2.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 13.0) * (*model_sigma));
   *model_C = 28.0*(*model_epsilon)*(-13.0 + 4.0*pow(*model_cutnorm, 6.0)) /
      pow(*model_cutnorm, 12.0);
   *model_sigmasq = (*model_sigma)*(*model_sigma);
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

   return;
}
