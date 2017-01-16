/*
*
* CDDL HEADER START
*
* The contents of this file are subject to the terms of the Common Development
* and Distribution License Version 1.0 (the "License").
*
* You can obtain a copy of the license at
* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
* specific language governing permissions and limitations under the License.
*
* When distributing Covered Code, include this CDDL HEADER in each file and
* include the License file in a prominent location with the name LICENSE.CDDL.
* If applicable, add the following below this CDDL HEADER, with the fields
* enclosed by brackets "[]" replaced with your own identifying information:
*
* Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
*
* CDDL HEADER END
*

*
* Copyright (c) 2013--2017, Regents of the University of Minnesota.
* All rights reserved.
*
* Contributors:
*    Ryan S. Elliott
*    Ellad B. Tadmor
*    Valeriu Smirichinski
*
*/

/*******************************************************************************
*
*  MODEL_NAME_STR
*
*  Morse pair potential model for SPECIES_NAME_STR
*  modified to have smooth cutoff
*
*  Release: This file is part of the kim-api.git repository.
*
*******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "KIM_API_C.h"
#include "KIM_API_status.h"

/******************************************************************************
* Below are the definitions and values of all Model parameters
*******************************************************************************/
#define DIM 3       /* dimensionality of space */
#define SPECCODE SPECIES_CODE_STR  /* internal species code */


/* Define prototypes for model init */
/**/
int model_init(void* km);

/* Define prototypes for model reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models */
/**/
static int reinit(void* km);
static int destroy(void* km);
static int compute(void* km);
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
static int compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dphi = 0.0;
   double Rij[DIM];
   int ier;
   int i;
   int j;
   int k;
   int comp_energy;
   int comp_force;
   int comp_particleEnergy;
   int comp_virial;

   int* nParts;
   int* particleSpecies;
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
   double* particleEnergy;
   double* virial;

   /* check to see if we have been asked to compute the forces, particleEnergy, and virial */
   KIM_API_getm_compute(pkim, &ier, 4*3,
                        "energy",         &comp_energy,         1,
                        "forces",         &comp_force,          1,
                        "particleEnergy", &comp_particleEnergy, 1,
                        "virial",         &comp_virial,         1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data(pkim, &ier, 7*3,
                     "numberOfParticles", &nParts,         1,
                     "particleSpecies",   &particleSpecies,1,
                     "energy",            &energy,         comp_energy,
                     "coordinates",       &coords,         1,
                     "forces",            &force,          comp_force,
                     "particleEnergy",    &particleEnergy, comp_particleEnergy,
                     "virial",            &virial,         comp_virial);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      return ier;
   }

   /* unpack the Model's parameters stored in the KIM API object */
   KIM_API_getm_data(pkim, &ier, 7*3,
                     "cutoff",             &cutoff,  1,
                     "PARAM_FREE_epsilon", &epsilon, 1,
                     "PARAM_FREE_sigma",   &sigma,   1,
                     "PARAM_FIXED_A",      &A,       1,
                     "PARAM_FIXED_B",      &B,       1,
                     "PARAM_FIXED_C",      &C,       1,
                     "PARAM_FIXED_cutsq",  &cutsq,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      return ier;
   }

   /* Check to be sure that the particle species are correct */
   /**/
   ier = KIM_STATUS_FAIL; /* assume an error */
   for (i = 0; i < *nParts; ++i)
   {
      if ( SPECCODE != particleSpecies[i])
      {
         KIM_API_report_error(__LINE__, __FILE__, "Unexpected species detected", ier);
         return ier;
      }
   }
   ier = KIM_STATUS_OK; /* everything is ok */

   /* initialize potential energies, forces, and virial term */
   if (comp_particleEnergy)
   {
      for (i = 0; i < *nParts; ++i)
      {
         particleEnergy[i] = 0.0;
      }
   }
   if (comp_energy)
   {
      *energy = 0.0;
   }

   if (comp_force)
   {
      for (i = 0; i < *nParts; ++i)
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

   /* Compute energy and forces */

   /* We'll use a half list approach                                      */
   /* Don't need to consider the last particle since all its interactions */
   /* are accounted for eariler in the loop                               */
   for (i = 0; i < *nParts-1; ++i)
   {
      for (j = i+1; j < *nParts; ++j)
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
            if (comp_particleEnergy)
            {
               particleEnergy[i] += 0.5*phi;
               particleEnergy[j] += 0.5*phi;
            }
            if (comp_energy)
            {
               *energy += phi;
            }

            /* contribution to virial tensor */
            if (comp_virial)
            {
               /* virial(i,j) = r(i)*r(j)*(dV/dr)/r */
               virial[0] += Rij[0]*Rij[0]*dphi/R;
               virial[1] += Rij[1]*Rij[1]*dphi/R;
               virial[2] += Rij[2]*Rij[2]*dphi/R;
               virial[3] += Rij[1]*Rij[2]*dphi/R;
               virial[4] += Rij[0]*Rij[2]*dphi/R;
               virial[5] += Rij[0]*Rij[1]*dphi/R;
            }

            /* contribution to forces */
            if (comp_force)
            {
               for (k = 0; k < DIM; ++k)
               {
                  force[i*DIM + k] += dphi*Rij[k]/R; /* accumulate force on particle i */
                  force[j*DIM + k] -= dphi*Rij[k]/R; /* accumulate force on particle j */
               }
            }
         }
      } /* loop on j */
   }    /* loop on i */

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/* Initialization function */
int model_init(void *km)
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
   KIM_API_setm_data(pkim, &ier, 3*4,
                     "compute", 1, &compute, 1,
                     "reinit",  1, &reinit,  1,
                     "destroy", 1, &destroy, 1);

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      return ier;
   }
   *model_cutoff = CUTOFF_VALUE_STR

   /* allocate memory */
   model_sigma = (double*) malloc(1*sizeof(double));
   if (NULL == model_sigma)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_epsilon = (double*) malloc(1*sizeof(double));
   if (NULL == model_epsilon)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_Pcutoff = (double*) malloc(1*sizeof(double));
   if (NULL == model_Pcutoff)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_cutnorm = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutnorm)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_A = (double*) malloc(1*sizeof(double));
   if (NULL == model_A)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_B = (double*) malloc(1*sizeof(double));
   if (NULL == model_B)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_C = (double*) malloc(1*sizeof(double));
   if (NULL == model_C)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_sigmasq = (double*) malloc(1*sizeof(double));
   if (NULL == model_sigmasq)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }
   model_cutsq = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutsq)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
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
      return ier;
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

   ier = KIM_STATUS_OK;
   return ier;
}

/* Reinitialization function */
static int reinit(void *km)
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
   KIM_API_getm_data(pkim, &ier, 10*3,
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
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      return ier;
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

   ier = KIM_STATUS_OK;
   return ier;
}

/* destroy function */
static int destroy(void *km)
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
      return ier;
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

   ier = KIM_STATUS_OK;
   return ier;
}
