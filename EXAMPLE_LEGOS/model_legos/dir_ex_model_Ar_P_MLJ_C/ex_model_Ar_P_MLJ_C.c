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
* Copyright (c) 2013--2014, Regents of the University of Minnesota.
* All rights reserved.
*
* Contributors:
*    Ryan S. Elliott
*    Ellad B. Tadmor
*    Valeriu Smirichinski
*    Stephen M. Whalen
*
*/

/*******************************************************************************
*
*  model_Ar_P_MLJ_C
*
*  Modified LJ pair potential (with smooth cutoff) Model for Ar
*
*  Language: C
*
*  Release: This file is part of the openkim-api.git repository.
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
#define SPECCODE 1  /* internal species code */
#define MODEL_CUTOFF 8.15 /* cutoff radius in angstroms */
#define MODEL_CUTSQ  (MODEL_CUTOFF * MODEL_CUTOFF)
#define SIGMA 3.40
#define EPSILON 0.0104
#define CUTNORM (MODEL_CUTOFF/SIGMA)
#define CUTNORM3 (CUTNORM*CUTNORM*CUTNORM)
#define CUTNORM6 (CUTNORM3*CUTNORM3)
#define CUTNORM14 (CUTNORM6*CUTNORM6*CUTNORM*CUTNORM)
#define APARAM (12.0*EPSILON*(-26.0 + 7.0*CUTNORM6)/(CUTNORM14*SIGMA*SIGMA))
#define BPARAM (96.0*EPSILON*(7.0 - 2.0*CUTNORM6)/(CUTNORM6*CUTNORM6*CUTNORM*SIGMA))
#define CPARAM (28.0*EPSILON*(-13.0 + 4.0*CUTNORM6)/(CUTNORM6*CUTNORM6))


/* Define prototypes for model init */
/**/
int model_init(void* km);

/* Define prototypes for model reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models */
/**/
static int compute(void* km);
/**/
static void calc_phi(double r, double* phi);
static void calc_phi_dphi(double r, double* phi, double* dphi);


/* Calculate pair potential phi(r) */
static void calc_phi(double r, double* phi)
{
   /* local variables */
   double rsq   = r*r;
   double sor   = SIGMA/r;
   double sor6  = sor*sor*sor*sor*sor*sor;
   double sor12 = sor6*sor6;

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *phi = 0.0;
   }
   else
   {
      *phi = 4.0*EPSILON*(sor12 - sor6) + APARAM*rsq + BPARAM*r + CPARAM;
   }

   return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double r, double* phi, double* dphi)
{
   /* local variables */
   double rsq   = r*r;
   double sor   = SIGMA/r;
   double sor6  = sor*sor*sor*sor*sor*sor;
   double sor12 = sor6*sor6;

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *phi  = 0.0;
      *dphi = 0.0;
   }
   else
   {
      *phi  = 4.0*EPSILON*(sor12 - sor6) + APARAM*rsq + BPARAM*r + CPARAM;
      *dphi = 24.0*EPSILON*(-2.0*sor12 + sor6)/r + 2.0*APARAM*r + BPARAM;
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
   double dphi;
   double dEidr = 0.0;
   double Rij[DIM];
   int ier;
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
   int IterOrLoca;
   int HalfOrFull;
   int NBC;
   char* NBCstr;
   int numberContrib;

   int* nAtoms;
   int* particleTypes;
   double* Rij_list;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   double* virial;
   int* neighListOfCurrentAtom;
   double* boxSideLengths;
   int* numContrib;


   /* Determine neighbor list boundary condition (NBC) */
   /* and half versus full mode: */
   /*****************************
    * HalfOrFull = 1 -- Half
    *            = 2 -- Full
    *****************************/
   NBCstr = (char*) KIM_API_get_NBC_method(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);
      return ier;
   }
   if (!strcmp("NEIGH_RVEC_H",NBCstr))
   {
      NBC = 0;
      HalfOrFull = 1;
   }
   else if (!strcmp("NEIGH_PURE_H",NBCstr))
   {
      NBC = 1;
      HalfOrFull = 1;
   }
   else if (!strcmp("NEIGH_RVEC_F",NBCstr))
   {
      NBC = 0;
      HalfOrFull = 2;
   }
   else if (!strcmp("NEIGH_PURE_F",NBCstr))
   {
      NBC = 1;
      HalfOrFull = 2;
   }
   else if (!strcmp("MI_OPBC_H",NBCstr))
   {
      NBC = 2;
      HalfOrFull = 1;
   }
   else if (!strcmp("MI_OPBC_F",NBCstr))
   {
      NBC = 2;
      HalfOrFull = 2;
   }
   else if (!strcmp("CLUSTER",NBCstr))
   {
      NBC = 3;
      HalfOrFull = 1;
   }
   else
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);
      return ier;
   }
   free(NBCstr); /* don't forget to release the memory... */

   /* determine neighbor list handling mode */
   if (NBC != 3)
   {
      /*****************************
       * IterOrLoca = 1 -- Iterator
       *            = 2 -- Locator
       *****************************/
      IterOrLoca = KIM_API_get_neigh_mode(pkim, &ier);
      if (KIM_STATUS_OK > ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh_mode", ier);
         return ier;
      }
      if ((IterOrLoca != 1) && (IterOrLoca != 2))
      {
         printf("* ERROR: Unsupported IterOrLoca mode = %i\n", IterOrLoca);
         return KIM_STATUS_FAIL;
      }
   }
   else
   {
      IterOrLoca = 2;   /* for CLUSTER NBC */
   }

   /* check to see if we have been asked to compute the forces, particleEnergy, energy and virial */
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
   KIM_API_getm_data(pkim, &ier, 9*3,
                     "numberOfParticles",           &nAtoms,         1,
                     "particleTypes",               &particleTypes,  1,
                     "coordinates",                 &coords,         1,
                     "numberContributingParticles", &numContrib,     (HalfOrFull==1),
                     "boxSideLengths",              &boxSideLengths, (NBC==2),
                     "energy",                      &energy,         (comp_energy==1),
                     "forces",                      &force,          (comp_force==1),
                     "particleEnergy",              &particleEnergy, (comp_particleEnergy==1),
                     "virial",                      &virial,         (comp_virial==1));
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
      return ier;
   }

   if (HalfOrFull == 1)
   {
      if (3 != NBC) /* non-CLUSTER cases */
      {
         numberContrib = *numContrib;
      }
      else
      {
         numberContrib = *nAtoms;
      }
   }
   else
   { /* provide initialization even if not used */
      numberContrib = *nAtoms;
   }

   /* Check to be sure that the atom types are correct */
   /**/
   ier = KIM_STATUS_FAIL; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if ( SPECCODE != particleTypes[i])
      {
         KIM_API_report_error(__LINE__, __FILE__, "Unexpected species type detected", ier);
         return ier;
      }
   }
   ier = KIM_STATUS_OK; /* everything is ok */

   /* initialize potential energies, forces, and virial term */
   if (comp_particleEnergy)
   {
      for (i = 0; i < *nAtoms; ++i)
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

   /* Initialize neighbor handling for CLUSTER NBC */
   if (3 == NBC) /* CLUSTER */
   {
      neighListOfCurrentAtom = (int *) malloc((*nAtoms)*sizeof(int));
   }

   /* Initialize neighbor handling for Iterator mode */

   if (1 == IterOrLoca)
   {
      ier = KIM_API_get_neigh(pkim, 0, 0, &currentAtom, &numOfAtomNeigh,
                               &neighListOfCurrentAtom, &Rij_list);
      /* check for successful initialization */
      if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
         ier = KIM_STATUS_FAIL;
         return ier;
      }
   }

   /* Compute energy and forces */

   /* loop over particles and compute enregy and forces */
   i = -1;
   while( 1 )
   {

      /* Set up neighbor list for next atom for all NBC methods */
      if (1 == IterOrLoca) /* ITERATOR mode */
      {
         ier = KIM_API_get_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh,
                                  &neighListOfCurrentAtom, &Rij_list);
         if (KIM_STATUS_NEIGH_ITER_PAST_END == ier) /* the end of the list, terminate loop */
         {
            break;
         }
         if (KIM_STATUS_OK > ier) /* some sort of problem, return */
         {
            KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
            return ier;
         }

         i = currentAtom;
      }
      else
      {
         i++;
         if (*nAtoms <= i) /* incremented past end of list, terminate loop */
         {
            break;
         }

         if (3 == NBC)     /* CLUSTER NBC method */
         {
            numOfAtomNeigh = *nAtoms - (i + 1);
            for (k = 0; k < numOfAtomNeigh; ++k)
            {
               neighListOfCurrentAtom[k] = i + k + 1;
            }
            ier = KIM_STATUS_OK;
         }
         else              /* All other NBCs */
         {
            ier = KIM_API_get_neigh(pkim, 1, i, &currentAtom, &numOfAtomNeigh,
                                     &neighListOfCurrentAtom, &Rij_list);
            if (KIM_STATUS_OK != ier) /* some sort of problem, return */
            {
            KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
            ier = KIM_STATUS_FAIL;
            return ier;
            }
         }
      }

      /* loop over the neighbors of atom i */
      for (jj = 0; jj < numOfAtomNeigh; ++ jj)
      {

         j = neighListOfCurrentAtom[jj]; /* get neighbor ID */

         /* compute relative position vector and squared distance */
         Rsqij = 0.0;
         for (k = 0; k < DIM; ++k)
         {
            if (0 != NBC) /* all methods except NEIGH_RVEC */
            {
               Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];
            }
            else          /* NEIGH_RVEC_F method */
            {
               Rij[k] = Rij_list[jj*DIM + k];
            }

            /* apply periodic boundary conditions if required */
            if (2 == NBC)
            {
               if (abs(Rij[k]) > 0.5*boxSideLengths[k])
               {
                  Rij[k] -= (Rij[k]/fabs(Rij[k]))*boxSideLengths[k];
               }
            }

            /* compute squared distance */
            Rsqij += Rij[k]*Rij[k];
         }

         /* compute energy and force */
         if (Rsqij < MODEL_CUTSQ) /* particles are interacting ? */
         {
            R = sqrt(Rsqij);
            if (comp_force || comp_virial)
            {
               /* compute pair potential and its derivative */
               calc_phi_dphi(R, &phi, &dphi);

               /* compute dEidr */
               if ((1 == HalfOrFull) && (j < numberContrib))
               {
                  /* HALF mode -- double contribution */
                  dEidr = dphi;
               }
               else
               {
                  /* FULL mode -- regular contribution */
                  dEidr = 0.5*dphi;
               }
            }
            else
            {
               /* compute just pair potential */
               calc_phi(R, &phi);
            }

            /* contribution to energy */
            if (comp_particleEnergy)
            {
               particleEnergy[i] += 0.5*phi;
               /* if half list add energy for the other atom in the pair */
               if ((1 == HalfOrFull) && (j < numberContrib)) particleEnergy[j] += 0.5*phi;
            }
            if (comp_energy)
            {
               if ((1 == HalfOrFull) && (j < numberContrib))
               {
                  /* Half mode -- add v to total energy */
                  *energy += phi;
               }
               else
               {
                  /* Full mode -- add half v to total energy */
                  *energy += 0.5*phi;
               }
            }

            /* contribution to virial tensor */
            if (comp_virial)
            {
               /* virial(i,j) = r(i)*r(j)*(dV/dr)/r */
               virial[0] += Rij[0]*Rij[0]*dEidr/R;
               virial[1] += Rij[1]*Rij[1]*dEidr/R;
               virial[2] += Rij[2]*Rij[2]*dEidr/R;
               virial[3] += Rij[1]*Rij[2]*dEidr/R;
               virial[4] += Rij[0]*Rij[2]*dEidr/R;
               virial[5] += Rij[0]*Rij[1]*dEidr/R;
            }

            /* contribution to forces */
            if (comp_force)
            {
               for (k = 0; k < DIM; ++k)
               {
                  force[i*DIM + k] += dEidr*Rij[k]/R; /* accumulate force on atom i */
                  force[j*DIM + k] -= dEidr*Rij[k]/R; /* accumulate force on atom j */
               }
            }
         }
      } /* loop on jj */
   }    /* infinite while loop (terminated by break statements above */


   /* Free temporary storage */
   if (3 == NBC)
   {
      free(neighListOfCurrentAtom);
   }

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
   int ier;

   /* store pointer to compute function in KIM object */
   ier = KIM_API_set_method(pkim, "compute", 1, (func_ptr) &compute);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_method", ier);
      return ier;
   }

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      return ier;
   }
   *model_cutoff = MODEL_CUTOFF; /* cutoff distance in angstroms */

   ier = KIM_STATUS_OK;
   return ier;
}
