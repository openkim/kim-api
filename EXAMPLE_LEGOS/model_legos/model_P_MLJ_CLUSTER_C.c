/*******************************************************************************
*
*  MODEL_NAME_STR
*
*  Morse pair potential model for SPECIES_NAME_STR
*  modified to have smooth cutoff
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
#include <string.h>
#include <math.h>
#include "KIMserviceC.h"
#include "KIMstatus.h"

/******************************************************************************
* Below are the definitions and values of all Model parameters
*******************************************************************************/
#define DIM 3       /* dimensionality of space */
#define SPECCODE SPECIES_CODE_STR  /* internal species code */


/* Define prototypes for model init */
/* must be all lowercase to be compatible with the KIM API (to support Fortran Tests) */
/**/
void MODEL_NAME_LC_STR_init_(void* km);

/* Define prototypes for model reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models */
/**/
static void reinit(void* km);
static void destroy(void* km);
static void compute(void* km, int* ier);
/**/
static void calc_phi(double* cutoff, double* epsilon, double* sigma,
                     double* A, double* B, double* C,
                     double r, double* phi);
static void calc_phi_dphi(double* cutoff, double* epsilon, double* sigma,
                          double* A, double* B, double* C,
                          double r, double* phi, double* dphi);


/* Calculate pair potential phi(r) */
static void calc_phi(double* cutoff, double* epsilon, double* sigma,
                     double* A, double* B, double* C,
                     double r, double* phi)
{
   /* local variables */
   double rsq;
   double sor;
   double sor6;
   double sor12;

   rsq   = r*r;
   sor   = (*sigma)/r;
   sor6  = sor*sor*sor;
   sor6  = sor6*sor6;
   sor12 = sor6*sor6;

   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi = 0.0;
   }
   else
   {
      *phi = 4.0*(*epsilon)*(sor12 - sor6) + (*A)*rsq + (*B)*r + (*C);
   }

   return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double* cutoff, double* epsilon, double* sigma,
                          double* A, double* B, double* C,
                          double r, double* phi, double* dphi)
{
   /* local variables */
   double rsq;
   double sor;
   double sor6;
   double sor12;

   rsq   = r*r;
   sor   = (*sigma)/r;
   sor6  = sor*sor*sor;
   sor6  = sor6*sor6;
   sor12 = sor6*sor6;

   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi  = 0.0;
      *dphi = 0.0;
   }
   else
   {
      *phi  = 4.0*(*epsilon)*(sor12 - sor6) + (*A)*rsq + (*B)*r + (*C);
      *dphi = 24.0*(*epsilon)*(-2.0*sor12 + sor6)/r + 2.0*(*A)*r + (*B);
   }

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
   double Rij[DIM];
   int i;
   int j;
   int k;
   int comp_force;
   int comp_energyPerAtom;
   int comp_virial;

   int* nAtoms;
   int* atomTypes;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* A;
   double* B;
   double* C;
   double* cutsq;
   double* coords;
   double* energy;
   double* force;
   double* energyPerAtom;
   double* virialGlobal;
   
   /* check to see if we have been asked to compute the forces, energyPerAtom, and virial */
   KIM_API_get_compute_multiple(pkim, ier, 3*3,
                                "forces",        &comp_force,         1,
                                "energyPerAtom", &comp_energyPerAtom, 1,
                                "virialGlobal",  &comp_virial,        1);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_compute_multiple", *ier);
      return;
   }

   /* unpack data from KIM object */
   KIM_API_get_data_multiple(pkim, ier, 7*3,
                             "numberOfAtoms", &nAtoms,        1,
                             "atomTypes",     &atomTypes,     1,
                             "energy",        &energy,        1,
                             "coordinates",   &coords,        1,
                             "forces",        &force,         comp_force,
                             "energyPerAtom", &energyPerAtom, comp_energyPerAtom,
                             "virialGlobal",  &virialGlobal,  comp_virial);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_multiple", *ier);
      return;
   }

   /* unpack the Model's parameters stored in the KIM API object */
   KIM_API_get_data_multiple(pkim, ier, 7*3,
                             "cutoff",             &cutoff,  1,
                             "PARAM_FREE_epsilon", &epsilon, 1,
                             "PARAM_FREE_sigma",   &sigma,   1,
                             "PARAM_FIXED_A",      &A,       1,
                             "PARAM_FIXED_B",      &B,       1,
                             "PARAM_FIXED_C",      &C,       1,
                             "PARAM_FIXED_cutsq",  &cutsq,   1);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_multiple", *ier);
      return;
   }

   /* Check to be sure that the atom types are correct */
   /**/
   *ier = KIM_STATUS_FAIL; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if ( SPECCODE != atomTypes[i])
      {
         KIM_API_report_error(__LINE__, __FILE__, "Unexpected species type detected", i);
         return;
      }
   }
   *ier = KIM_STATUS_OK; /* everything is ok */

   /* initialize potential energies, forces, and virial term */
   if (comp_energyPerAtom)
   {
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
         virialGlobal[i] = 0.0;
      }
   }

   /* Compute enery and forces */

   /* We'll use a half list approach                                  */
   /* Don't need to consider the last atom since all its interactions */
   /* are accounted for eariler in the loop                           */
   for (i = 0; i < *nAtoms-1; ++i)
   {
      for (j = i+1; j < *nAtoms; ++j)
      {
         /* compute relative position vector and squared distance */
         Rsqij = 0.0;
         for (k = 0; k < DIM; ++k)
         {
            Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];

            /* compute squared distance */
            Rsqij += Rij[k]*Rij[k];
         }
         
         /* compute energy and force */
         if (Rsqij < *cutsq) /* particles are interacting ? */
         {
            R = sqrt(Rsqij);
            if (comp_force || comp_virial)
            {
               /* compute pair potential and its derivative */
               calc_phi_dphi(cutoff, epsilon, sigma, A, B, C, R, &phi, &dphi);
            }
            else
            {
               /* compute just pair potential */
               calc_phi(cutoff, epsilon, sigma, A, B, C, R, &phi);
            }
            
            /* contribution to energy */
            if (comp_energyPerAtom)
            {
               energyPerAtom[i] += 0.5*phi;
               energyPerAtom[j] += 0.5*phi;
            }
            else
            {
               *energy += phi;
            }
            
            /* contribution to virial tensor */
            if (comp_virial)
            {
               /* virial(i,j) = r(i)*r(j)*(dV/dr)/r */
	       virialGlobal[0] += Rij[0]*Rij[0]*dphi/R;
	       virialGlobal[1] += Rij[1]*Rij[1]*dphi/R;
	       virialGlobal[2] += Rij[2]*Rij[2]*dphi/R;
	       virialGlobal[3] += Rij[1]*Rij[2]*dphi/R;
	       virialGlobal[4] += Rij[0]*Rij[2]*dphi/R;
	       virialGlobal[5] += Rij[0]*Rij[1]*dphi/R;
            }
            
            /* contribution to forces */
            if (comp_force)
            {
               for (k = 0; k < DIM; ++k)
               {
                  force[i*DIM + k] += dphi*Rij[k]/R; /* accumulate force on atom i */
                  force[j*DIM + k] -= dphi*Rij[k]/R; /* accumulate force on atom j */
               }
            }
         }
      } /* loop on j */
   }    /* loop on i */
   

   /* perform final tasks */
   
   if (comp_energyPerAtom)
   {
      *energy = 0.0;
      for (k = 0; k < *nAtoms; ++k)
      {
         *energy += energyPerAtom[k];
      }
   }

   /* everything is great */
   *ier = KIM_STATUS_OK;
   return;
}

/* Initialization function */
void MODEL_NAME_LC_STR_init_(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_cutoff;
   double* model_cutnorm;
   double* model_epsilon;
   double* model_sigma;
   double* model_Pcutoff;
   double* model_A;
   double* model_B;
   double* model_C;
   double* model_sigmasq;
   double* model_cutsq;
   int ier;

   /* store function pointers in KIM object */
   KIM_API_set_data_multiple(pkim, &ier, 3*4,
                             "compute", 1, &compute, 1,
                             "reinit",  1, &reinit,  1,
                             "destroy", 1, &destroy, 1);
   
   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = CUTOFF_VALUE_STR

   /* allocate memory */
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
   KIM_API_set_data_multiple(pkim, &ier, 9*4,
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
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data_multiple", ier);
      exit(1);
   }
   
   /* set value of sigma */
   *model_sigma = SIGMA_VALUE_STR
   /* set value of epsilon */
   *model_epsilon = EPSILON_VALUE_STR
   /* set value of parameter cutoff */
   *model_Pcutoff = *model_cutoff;
   /* set value of parameter cutnorm */
   *model_cutnorm = (*model_cutoff)/(*model_sigma);
   /* set value of parameter A */
   *model_A = 12.0*(*model_epsilon)*(-26.0 + 7.0*pow(*model_cutnorm,6))/
      (pow(*model_cutnorm,14)*(*model_sigma)*(*model_sigma));
   /* set value of parameter B */
   *model_B = 96.0*(*model_epsilon)*(7.0 - 2.0*pow(*model_cutnorm,6))/
               (pow(*model_cutnorm,13)*(*model_sigma));
   /* set value of parameter C */
   *model_C = 28.0*(*model_epsilon)*(-13.0 + 4.0*pow(*model_cutnorm,6))/
               pow(*model_cutnorm,12);
   /* set value of parameter sigmasq */
   *model_sigmasq = (*model_sigma)*(*model_sigma);
   /* set value of parameter cutsq */
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

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
   double* model_cutnorm;
   double* model_Pcutoff;
   double* model_A;
   double* model_B;
   double* model_C;
   double* model_sigmasq;
   double* model_cutsq;
   int ier;

   /* get parameters from KIM object */
   KIM_API_get_data_multiple(pkim, &ier, 10*3,
                             "cutoff",              &model_cutoff,  1,
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
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_multiple", ier);
      exit(1);
   }

   /* set value of cutoff in KIM object */
   *model_cutoff = *model_Pcutoff;
   /* set value of parameter cutnorm */
   *model_cutnorm = (*model_sigma)/(*model_cutoff);
   /* set value of parameter A */
   *model_A = 12.0*(*model_epsilon)*(-26.0 + 7.0*pow(*model_cutnorm,6))/
      (pow(*model_cutnorm,14)*(*model_sigma)*(*model_sigma));
   /* set value of parameter B */
   *model_B = 96.0*(*model_epsilon)*(7.0 - 2.0*pow(*model_cutnorm,6))/
               (pow(*model_cutnorm,13)*(*model_sigma));
   /* set value of parameter C */
   *model_C = 28.0*(*model_epsilon)*(-13.0 + 4.0*pow(*model_cutnorm,6))/
               pow(*model_cutnorm,12);
   /* set value of parameter sigmasq */
   *model_sigmasq = (*model_sigma)*(*model_sigma);
   /* set value of parameter cutsq */
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
   KIM_API_get_data_multiple(pkim, &ier, 9*3,
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
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_multiple", ier);
      exit(1);
   }

   /*free memory for the parameters */
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
