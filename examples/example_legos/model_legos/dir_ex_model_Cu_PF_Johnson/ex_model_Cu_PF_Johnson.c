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
*    Stephen M. Whalen
*
*/

/*******************************************************************************
*
*  model_Cu_PF_Johnson
*
*  Johnson pair functional model for Cu
*
*  Reference: R. A. Johnson, "Analytic nearest-neighbor model for fcc metals",
*             Phys. Rev. B, 55(8):4941-4946, 1988.
*
*  Language: C
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
#define SPECCODE 1  /* internal species code */
#define MODEL_CUTOFF 3.5 /* cutoff radius in angstroms */
#define MODEL_CUTSQ  (MODEL_CUTOFF * MODEL_CUTOFF)
#define JEAM_R0  2.556 /* A */
#define JEAM_PHI0 0.59 /* eV */
#define JEAM_GAM  8.00
#define JEAM_G0   0.30 /* eV */
#define JEAM_BET  5.85
#define JEAM_EC   3.54 /* eV/atom */
#define JEAM_ALF  5.09
#define JEAM_RHO0 3.60 /* eV (=12*JEAM_G0) */

/* Define prototypes for model init */
/**/
int model_init(void* km);

/* Define prototypes for model reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models */
/**/
static int compute(void* km);
static int destroy(void* km);
/**/
static void calc_phi(double r, double* phi);
static void calc_phi_dphi(double r, double* phi, double* dphi);
static void calc_g(double r, double* g);
static void calc_dg(double r, double* dg);
static void calc_U(double rho, double* U);
static void calc_U_dU(double rho, double* U, double* dU);
static void get_current_atom_neighbors(int IterOrLoca, int HalfOrFull, int NBC,
                                       int nAtoms, intptr_t* pkim, int* i, int* numOfAtomNeigh,
                                       int** neighListOfCurrentAtom, double** Rij_list,
                                       int* ier);


/* Calculate pair potential phi(r) */
static void calc_phi(double r, double* phi)
{
   /* local variables */
   double rnorm;

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *phi = 0.0;
   }
   else
   {
      rnorm = r/JEAM_R0;
      *phi = JEAM_PHI0 * exp(-JEAM_GAM*(rnorm - 1.0));
   }

   return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double r, double* phi, double* dphi)
{
   /* local variables */
   double rnorm;

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *phi  = 0.0;
      *dphi = 0.0;
   }
   else
   {
      rnorm = r/JEAM_R0;
      *phi  = JEAM_PHI0 * exp(-JEAM_GAM*(rnorm - 1.0));
      *dphi = -(JEAM_GAM/JEAM_R0)*(*phi);
   }

   return;
}

/* Calculate electron density g(r) */
static void calc_g(double r, double* g)
{
   /* local variables */
   double rnorm;

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *g = 0.0;
   }
   else
   {
      rnorm = r/JEAM_R0;
      *g = JEAM_G0 * exp(-JEAM_BET*(rnorm - 1.0));
   }

   return;
}

/* Calculate electron density derivative dg(r) */
static void calc_dg(double r, double* dg)
{
   /* local variables */
   double rnorm;
   double g;

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *dg = 0.0;
   }
   else
   {
      rnorm = r/JEAM_R0;
      g = JEAM_G0 * exp(-JEAM_BET*(rnorm - 1.0));
      *dg = -(JEAM_BET/JEAM_R0)*g;
   }

   return;
}

/* Calculate embedding function U(rho) */
static void calc_U(double rho, double* U)
{
   /* local variables */
   double rhonorm;
   double rhonorm_gob;
   double rhonorm_aob;
   double logrhonorm;
   double aob;
   double gob;

   if (rho == 0.0)
   {
      *U = 0.0;

      return;
   }
   
   aob = JEAM_ALF/JEAM_BET;
   gob = JEAM_GAM/JEAM_BET;
   rhonorm = rho/JEAM_RHO0;
   rhonorm_aob = pow(rhonorm, aob);
   rhonorm_gob = pow(rhonorm, gob);
   logrhonorm = log(rhonorm);

   *U = -JEAM_EC * (1.0 - aob*logrhonorm)*rhonorm_aob - 6.0*JEAM_PHI0*rhonorm_gob;

   return;
}

/* Calculate embedding function U(rho) and first derivative dU(rho) */
static void calc_U_dU(double rho, double* U, double* dU)
{
   /* local variables */
   double rhonorm;
   double rhonorm_gob;
   double rhonorm_aob;
   double logrhonorm;
   double aob;
   double gob;

   if (rho == 0.0)
   {
      *U = 0.0;
      *dU = 0.0;

      return;
   }
   
   aob = JEAM_ALF/JEAM_BET;
   gob = JEAM_GAM/JEAM_BET;
   rhonorm = rho/JEAM_RHO0;
   rhonorm_aob = pow(rhonorm, aob);
   rhonorm_gob = pow(rhonorm, gob);
   logrhonorm = log(rhonorm);

   *U = -JEAM_EC * (1.0 - aob*logrhonorm)*rhonorm_aob - 6.0*JEAM_PHI0*rhonorm_gob;
   *dU = (JEAM_EC*aob*aob*logrhonorm*rhonorm_aob - 6.0*JEAM_PHI0*gob*rhonorm_gob)/rho;

   return;
}

/* compute function */
static int compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double Rij[DIM];
   double r;
   double Rsqij;
   double phi;
   double dphi;
   double g;
   double dg;
   double dU;
   double dphieff = 0.0;
   double dphii;
   double dUi;
   double Ei;
   double dphij;
   double dUj;
   double Ej;
   int ier;
   int i;
   int j;
   int jj;
   int k;
   int numOfAtomNeigh;
   int comp_force;
   int comp_particleEnergy;
   int comp_virial;
   int comp_energy;
   double* rho;
   double U;
   double* derU = 0;
   
   int* nAtoms;
   double* energy;
   double* coords;
   double* force;
   double* particleEnergy;
   double* boxSideLengths;
   double* Rij_list;
   int* numContrib;
   int* neighListOfCurrentAtom;
   int* particleSpecies;
   double* virial;
   const char* NBCstr;
   int IterOrLoca;
   int HalfOrFull;
   int NBC;
   int numberContrib;
   int currentAtom;

   /* Determine neighbor list boundary condition (NBC) */
   /* and half versus full mode: */
   /*****************************
    * HalfOrFull = 1 -- Half
    *            = 2 -- Full
    *****************************/
   ier = KIM_API_get_NBC_method(pkim, &NBCstr);
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
         KIM_API_report_error(__LINE__, __FILE__, "Unsupported IterOrLoca mode", KIM_STATUS_FAIL);
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
                     "particleSpecies",             &particleSpecies,1,
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

   /* Check to be sure that the atom species are correct */
   ier = KIM_STATUS_FAIL; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if ( SPECCODE != particleSpecies[i])
      {
         KIM_API_report_error(__LINE__, __FILE__, "Unexpected species detected", ier);
         return ier;
      }
   }
   ier = KIM_STATUS_OK; /* everything is ok */

   /* initialize potential energies, forces, and virial term */
   /* Note: that the variable `particleEnergy' does not need to be initialized
    * because it's initial value is set during the embedding energy calculation.
    */
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

   /* pair functional electron density */
   rho = (double*) malloc((*nAtoms)*sizeof(double));
   for (i = 0; i < *nAtoms; ++i)
   {
      rho[i] = 0.0;
   }

   /* EAM embedded energy deriv */
   if ((comp_force) || (comp_virial))
   {
      derU = (double*) malloc((*nAtoms)*sizeof(double));
   }

   /* Initialize neighbor handling for CLUSTER NBC */
   if (3 == NBC) /* CLUSTER */
   {
      neighListOfCurrentAtom = (int *) malloc((*nAtoms)*sizeof(int));
   }

   /* Compute energy and forces */

   /* Reset iterator if one is being used */
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

   /* loop over particles in the neighbor list a first time,
    * to compute electron density (=coordination)
    */
   i = -1;
   while( 1 )
   {
      /* Set up neighbor list for next atom for all NBC methods */
      get_current_atom_neighbors(IterOrLoca, HalfOrFull, NBC, *nAtoms, pkim,
                                 &i, &numOfAtomNeigh, &neighListOfCurrentAtom,
                                 &Rij_list, &ier);

      if (ier == KIM_STATUS_NEIGH_ITER_PAST_END) break; /* atom counter incremented past end of list */
      if (KIM_STATUS_OK > ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "get_current_atom_neighbors", ier);
         return ier;
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

         /* compute contribution to electron density */
         if (Rsqij < MODEL_CUTSQ) /* particles are interacting ? */
         {
            r = sqrt(Rsqij); /* compute distance */
            calc_g(r,&g);     /* compute electron density */
            rho[i] += g;     /* accumulate electron density */
            if ((HalfOrFull == 1) && (j < numberContrib))
            {
               rho[j] += g;  /* HALF mode (add contrib to j) */
            }
         }
      } /* loop on jj */
   }    /* infinite while loop (terminated by break statements above */

   
   /* Now that we know the electron densities, calculate embedding part
    * of energy U and its derivative U' (derU)
    */
   for (i = 0; i < *nAtoms; ++i)
   {
      if (comp_force || comp_virial)
      {
         calc_U_dU(rho[i], &U, &dU); /* compute embedding energy and its derivative */
         derU[i] = dU;               /* store dU for later use */
      }
      else
      {
         calc_U(rho[i],&U);         /* compute just embedding energy */
      }

      /* accumulate the embedding energy contribution */
      /* Assuming U)rho=0) = 0.0 */
      if (comp_particleEnergy)
      {
         particleEnergy[i] += U;
      }
      else if (comp_energy)
      {
         *energy += U;
      }

      if ((HalfOrFull == 1) && (i >= numberContrib)) break;
   }

   /* Loop over particles in the neighbor list a second time to compute
    * the forces and complete energy calculation
    */

   /* Reset iterator if one is being used */
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

   i = -1;
   while( 1 )
   {
      /* Set up neighbor list for next atom for all NBC methods */
      get_current_atom_neighbors(IterOrLoca, HalfOrFull, NBC, *nAtoms, pkim,
                                 &i, &numOfAtomNeigh, &neighListOfCurrentAtom,
                                 &Rij_list, &ier);

      if (ier == KIM_STATUS_NEIGH_ITER_PAST_END) break; /* atom counter incremented past end of list */
      if (KIM_STATUS_OK > ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "get_current_atom_neighbors", ier);
         return ier;
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
            r = sqrt(Rsqij);
            if (comp_force || comp_virial)
            {
               /* compute pair potential and its derivative */
               calc_phi_dphi(r, &phi, &dphi);

               /* copmute elect dens first deriv */
               calc_dg(r, &dg);

               /* compute dEidr */
               if ((1 == HalfOrFull) && (j < numberContrib))
               {
                  /* HALF mode -- double contribution */
                  dphii = 0.5*dphi;
                  dphij = 0.5*dphi;
                  dUi = derU[i]*dg;
                  dUj = derU[j]*dg;
               }
               else
               {
                  /* FULL mode -- regular contribution */
                  dphii = 0.5*dphi;
                  dphij = 0.0;
                  dUi = derU[i]*dg;
                  dUj = 0.0;
               }
               dphieff = dphii + dphij + dUi + dUj;
            }
            else
            {
               /* compute just pair potential */
               calc_phi(r, &phi);
            }

            if ((HalfOrFull == 1) && (j < numberContrib))
            {
               /* HALF mode */
               Ei = 0.5*phi;
               Ej = 0.5*phi;
            }
            else
            {
               /* FULL mode */
               Ei = 0.5*phi;
               Ej = 0.0;
            }

            /* contribution to energy */
            if (comp_particleEnergy)
            {
               particleEnergy[i] += Ei; /* accumulate energy Ei */
               particleEnergy[j] += Ej; /* accumulate energy Ej */
            }
            if (comp_energy)
            {
               *energy += Ei; /* accumulate energy */
               *energy += Ej; /* accumulate energy */
            }

            /* contribution to virial tensor */
            if (comp_virial)
            {
               /* virial(i,j) = r(i)*r(j)*(dV/dr)/r */
               virial[0] += Rij[0]*Rij[0]*dphieff/r;
               virial[1] += Rij[1]*Rij[1]*dphieff/r;
               virial[2] += Rij[2]*Rij[2]*dphieff/r;
               virial[3] += Rij[1]*Rij[2]*dphieff/r;
               virial[4] += Rij[0]*Rij[2]*dphieff/r;
               virial[5] += Rij[0]*Rij[1]*dphieff/r;
            }

            /* contribution to forces */
            if (comp_force)
            {  /* Ei contribution */
               for (k = 0; k < DIM; ++k)
               {
                  force[i*DIM + k] += dphieff*Rij[k]/r; /* accumulate force on atom i */
                  force[j*DIM + k] -= dphieff*Rij[k]/r; /* accumulate force on atom j */
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
   free(rho);
   if (comp_force || comp_virial)
   {
      free(derU);
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

static void get_current_atom_neighbors(int IterOrLoca, int HalfOrFull, int NBC, int nAtoms,
                                       intptr_t* pkim, int* i, int* numOfAtomNeigh,
                                       int** neighListOfCurrentAtom, double** Rij_list, int* ier)
{
   int currentAtom;
   int k;
   /* Set up neighbor list for next atom for all NBC methods */
   if (1 == IterOrLoca) /* ITERATOR mode */
   {
      *ier = KIM_API_get_neigh(pkim, 0, 1, &currentAtom, numOfAtomNeigh,
                               neighListOfCurrentAtom, Rij_list);
      if (KIM_STATUS_NEIGH_ITER_PAST_END == *ier) /* the end of the list, terminate loop */
      {
         return;
      }
      if (KIM_STATUS_OK > *ier) /* some sort of problem, return */
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", *ier);
         return;
      }
      
      *i = currentAtom;
   }
   else
   {
      (*i)++;
      if (nAtoms <= *i) /* incremented past end of list, terminate loop */
      {
         *ier = KIM_STATUS_NEIGH_ITER_PAST_END;
         return;
      }
      
      if (3 == NBC)     /* CLUSTER NBC method */
      {
         *numOfAtomNeigh = nAtoms - (*i + 1);
         for (k = 0; k < *numOfAtomNeigh; ++k)
         {
            (*neighListOfCurrentAtom)[k] = *i + k + 1;
         }
         *ier = KIM_STATUS_OK;
      }
      else              /* All other NBCs */
      {
         *ier = KIM_API_get_neigh(pkim, 1, *i, &currentAtom, numOfAtomNeigh,
                                  neighListOfCurrentAtom, Rij_list);
         if (KIM_STATUS_OK != *ier) /* some sort of problem, return */
         {
            KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", *ier);
            *ier = KIM_STATUS_FAIL;
            return;
         }
      }
   }

   return;
}

/* Model destroy routine */
int destroy(void *km)
{
   /* Local variables */
   /* intptr_t* pkim = *((intptr_t**) km); */

   /* nothing to do */

   return KIM_STATUS_OK;
}

/* Initialization function */
int model_init(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* cutoff;
   int ier;

   /* store pointer to compute function in KIM object */
   KIM_API_setm_method(pkim, &ier, 2*4,
                     "compute", 1, (func_ptr) &compute, 1,
                     "destroy", 1, (func_ptr) &destroy, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_method", ier);
      return ier;
   }

   /* store model cutoff in KIM object */
   cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      return ier;
   }
   *cutoff = MODEL_CUTOFF; /* cutoff distance in angstroms */

   ier = KIM_STATUS_OK;
   return ier;
}
