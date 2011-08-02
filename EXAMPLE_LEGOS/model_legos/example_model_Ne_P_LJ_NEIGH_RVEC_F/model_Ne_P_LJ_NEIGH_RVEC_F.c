/*******************************************************************************
*
*  model_Ne_P_LJ_NEIGH_RVEC_F
*
*  Lennard-Jones pair potential model for neon
*  (modified to have smooth cutoff)
*
*  Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor
*
*  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
*  All rights reserved.
*
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "KIMserviceC.h"

#define DIM 3
#define Ar  1

/* Define prototypes for model init */
void model_ne_p_lj_neigh_rvec_f_init_(void* km);

/* Define prototypes for model reinit, compute, and destroy */
static void reinit(void* km);
static void destroy(void* km);
static void compute(void* km, int* ier);
static void report_error(int line, char* str, int status);

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
   int i;
   int jj;
   int k;
   int numOfAtomNeigh;
   int requestedAtom;
   int currentAtom;
   int comp_force;
   int comp_energyPerAtom;
   int comp_virial;

   intptr_t* nAtoms;
   int* nAtomTypes;
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
   double* energyPerAtom;
   double* virial;
   int* neighListOfCurrentAtom;

   /* unpack data from KIM object */
   nAtoms = (intptr_t*) KIM_API_get_data(pkim, "numberOfAtoms", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   nAtomTypes = (int*) KIM_API_get_data(pkim, "numberAtomTypes", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   atomTypes = (int*) KIM_API_get_data(pkim, "atomTypes", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   cutoff = (double*) KIM_API_get_data(pkim, "cutoff", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   epsilon = (double*) KIM_API_get_data(pkim, "PARAM_FREE_epsilon", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   sigma = (double*) KIM_API_get_data(pkim, "PARAM_FREE_sigma", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   cutnorm = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_cutnorm", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   A = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_A", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   B = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_B", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   C = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_C", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   sigmasq = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_sigmasq", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   cutsq = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_cutsq", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   energy = (double*) KIM_API_get_data(pkim, "energy", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   coords = (double*) KIM_API_get_data(pkim, "coordinates", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }

   /* check to see if we have been asked to compute the forces, energyPerAtom, and virial */
   comp_force = KIM_API_isit_compute(pkim, "forces", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_isit_compute", *ier);
      return;
   }
   comp_energyPerAtom = KIM_API_isit_compute(pkim, "energyPerAtom", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_isit_compute", *ier);
      return;
   }
   comp_virial = KIM_API_isit_compute(pkim, "virial", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_isit_compute", *ier);
      return;
   }

   /* check to be sure the atom types are correct */
   *ier = 0; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if (Ar != atomTypes[i])
      {
         report_error(__LINE__, "Wrong atomType", i);
         return;
      }
   }
   *ier = 1; /* everything is ok */

   /* initialize potential energies, forces, and virial term */
   if (comp_energyPerAtom)
   {
      energyPerAtom = (double*) KIM_API_get_data(pkim, "energyPerAtom", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
      }
      for (i = 0; i < *nAtoms; ++i)
      {
         energyPerAtom[i] = 0.0;
      }
   }
   else
   {
      *energy = 0.0;
   }

   if (comp_force)
   {
      force = (double*) KIM_API_get_data(pkim, "forces", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
      }
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
      virial = (double*) KIM_API_get_data(pkim, "virial", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
      }
      *virial = 0.0;
   }

   /* reset neighbor iterator */
   *ier = KIM_API_get_full_neigh(pkim, 0, 0, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (2 != *ier)
   {
      report_error(__LINE__, "KIM_API_get_full_neigh", *ier);
      *ier = 0;
      return;
   }

   /* compute energy and forces */
   *ier = KIM_API_get_full_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   while (1 == *ier)
   {
      /* loop over the neighbors of currentAtom */
      for (jj = 0; jj < numOfAtomNeigh; ++ jj)
      {
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

            /* accumulate energy */
            if (comp_energyPerAtom)
            {
               energyPerAtom[currentAtom] += 0.5*phi;
            }
            else
            {
               *energy += 0.5*phi;
            }

            /* accumulate virial */
            if (comp_virial)
            {
               *virial += 0.5*R*dphi;
            }

            /* accumulate force */
            if (comp_force)
            {
               for (k = 0; k < DIM; ++k)
               {
                  force[currentAtom*DIM + k] += dphi*Rij[jj*DIM + k]/R;
               }
            }
         }
      }
      
      /* increment iterator */
      *ier = KIM_API_get_full_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   }


   /* perform final tasks */
   
   if (comp_virial)
   {
      *virial = -*virial/( (double) DIM); /* definition of virial term */
   }

   if (comp_energyPerAtom)
   {
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         *energy += energyPerAtom[i];
      }
   }

   /* everything is great */
   *ier = 1;
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

   /* get sigma from KIM object */
   model_sigma = (double*) KIM_API_get_data(pkim, "PARAM_FREE_sigma", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   
   /* get epsilon from KIM object */
   model_epsilon = (double*) KIM_API_get_data(pkim, "PARAM_FREE_epsilon", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get parameter cutoff from KIM object */
   model_Pcutoff = (double*) KIM_API_get_data(pkim, "PARAM_FREE_cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }


   /* set new values in KIM object */
   

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = *model_Pcutoff;

   /* store model_cutnorm in KIM object */
   model_cutnorm = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_cutnorm", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter cutnorm */
   *model_cutnorm = *model_cutoff / *model_sigma;

   /* store model_A in KIM object */
   model_A = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_A", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter A */
   *model_A = 12.0*(*model_epsilon)*(-26.0 + 7.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 14.0) * pow(*model_sigma, 2.0));

   /* store model_B in KIM object */
   model_B = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_B", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter B */
   *model_B = 96.0*(*model_epsilon)*(7.0 - 2.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 13.0) * (*model_sigma));

   /* store model_C in KIM object */
   model_C = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_C", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter C */
   *model_C = 28.0*(*model_epsilon)*(-13.0 + 4.0*pow(*model_cutnorm, 6.0)) /
      pow(*model_cutnorm, 12.0);

   /* store model_sigmasq in KIM object */
   model_sigmasq = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_sigmasq", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter sigmasq */
   *model_sigmasq = (*model_sigma)*(*model_sigma);

   /* store model_cutsq in KIM object */
   model_cutsq = KIM_API_get_data(pkim, "PARAM_FIXED_cutsq", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter cutsq */
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

   return;
}

/* destroy function */
static void destroy(void *km)
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

   /* get and free sigma from KIM object */
   model_sigma = (double*) KIM_API_get_data(pkim, "PARAM_FREE_sigma", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_sigma);
   
   /* get and free epsilon from KIM object */
   model_epsilon = (double*) KIM_API_get_data(pkim, "PARAM_FREE_epsilon", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_epsilon);

   /* get and free parameter cutoff from KIM object */
   model_Pcutoff = (double*) KIM_API_get_data(pkim, "PARAM_FREE_cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_Pcutoff);

   /* get and free model_cutnorm in KIM object */
   model_cutnorm = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_cutnorm", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_cutnorm);

   /* get and free model_A in KIM object */
   model_A = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_A", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_A);

   /* get and free model_B in KIM object */
   model_B = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_B", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_B);

   /* get and free model_C in KIM object */
   model_C = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_C", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_C);
   
   /* get and free model_sigmasq in KIM object */
   model_sigmasq = (double*) KIM_API_get_data(pkim, "PARAM_FIXED_sigmasq", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_sigmasq);

   /* get and free model_cutsq in KIM object */
   model_cutsq = KIM_API_get_data(pkim, "PARAM_FIXED_cutsq", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_cutsq);

   return;
}


/* Initialization function */
void model_ne_p_lj_neigh_rvec_f_init_(void *km)
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

   /* store pointer to compute function in KIM object */
   if (! KIM_API_set_data(pkim, "compute", 1, (void*) &compute))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }

   /* store pointer to reinit function in KIM object */
   if (! KIM_API_set_data(pkim, "reinit", 1, (void*) &reinit))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }

   /* store pointer to destroy function in KIM object */
   if (! KIM_API_set_data(pkim, "destroy", 1, (void*) &destroy))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = 8.15; /* cutoff distance in angstroms */

   /* allocate memory for sigma and store value */
   model_sigma = (double*) malloc(1*sizeof(double));
   if (NULL == model_sigma)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_sigma in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FREE_sigma", 1, (void*) model_sigma))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of sigma */
   *model_sigma = 2.74; /* LJ sigma in angstroms */

   /* allocate memory for epsilon and store value */
   model_epsilon = (double*) malloc(1*sizeof(double));
   if (NULL == model_epsilon)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_epsilon in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FREE_epsilon", 1, (void*) model_epsilon))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of epsilon */
   *model_epsilon = 0.0031; /* LJ epsilon in eV */

   /* allocate memory for parameter cutoff and store value */
   model_Pcutoff = (double*) malloc(1*sizeof(double));
   if (NULL == model_Pcutoff)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_epsilon in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FREE_cutoff", 1, (void*) model_Pcutoff))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter cutoff */
   *model_Pcutoff = *model_cutoff;

   /* allocate memory for parameter cutnorm and store value */
   model_cutnorm = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutnorm)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_cutnorm in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FIXED_cutnorm", 1, (void*) model_cutnorm))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter cutnorm */
   *model_cutnorm = *model_cutoff / *model_sigma;

   /* allocate memory for parameter A and store value */
   model_A = (double*) malloc(1*sizeof(double));
   if (NULL == model_A)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_A in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FIXED_A", 1, (void*) model_A))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter A */
   *model_A = 12.0*(*model_epsilon)*(-26.0 + 7.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 14.0) * pow(*model_sigma, 2.0));

   /* allocate memory for parameter B and store value */
   model_B = (double*) malloc(1*sizeof(double));
   if (NULL == model_B)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_B in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FIXED_B", 1, (void*) model_B))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter B */
   *model_B = 96.0*(*model_epsilon)*(7.0 - 2.0*pow(*model_cutnorm, 6.0)) /
      (pow(*model_cutnorm, 13.0) * (*model_sigma));

   /* allocate memory for parameter C and store value */
   model_C = (double*) malloc(1*sizeof(double));
   if (NULL == model_C)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_C in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FIXED_C", 1, (void*) model_C))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter C */
   *model_C = 28.0*(*model_epsilon)*(-13.0 + 4.0*pow(*model_cutnorm, 6.0)) /
      pow(*model_cutnorm, 12.0);

   /* allocate memory for parameter sigmasq and store value */
   model_sigmasq = (double*) malloc(1*sizeof(double));
   if (NULL == model_sigmasq)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_sigmasq in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FIXED_sigmasq", 1, (void*) model_sigmasq))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter sigmasq */
   *model_sigmasq = (*model_sigma)*(*model_sigma);

   /* allocate memory for parameter cutsq and store value */
   model_cutsq = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutsq)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_cutsq in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FIXED_cutsq", 1, (void*) model_cutsq))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter cutsq */
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

   return;
}

static void report_error(int line, char* str, int status)
{
   printf("* ERROR at line %i in %s: %s. kimerror = %i\n", line, __FILE__, str, status);
}
