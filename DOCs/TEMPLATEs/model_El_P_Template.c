/*******************************************************************************
*
*  model_<FILL element name>_P_<FILL model name>
*
*  <FILL model name> pair potential model for <FILL element name>
*
*  Reference: <FILL>
*
*  Author: <FILL>
*  Date  : <FILL>
*
*  Language: C
*
*  Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna
*  All rights reserved.
*
*******************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "KIMserviceC.h"

/******************************************************************************
* Below are the definitions and values of all Model parameters
*******************************************************************************/
#define DIM 3       /* dimensionality of space */
#define SPECCODE 1  /* internal species code */
#define MODEL_CUTOFF <FILL cutoff value> /* cutoff radius in angstroms */
#define <FILL parameter name> <FILL parameter value>

/* Define prototypes for model init */
/* must be all lowercase to be compatible with the KIM API (to support Fortran Tests) */
/**/
void model_<FILL (lowercase) element name>_p_<FILL (lowercase) model name>_init_(void* km);

/* Define prototypes for model reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models */
/**/
static void compute(void* km, int* ier);
static void report_error(int line, char* str, int status);
/**/
static void calc_phi(double r, double* phi);
static void calc_phi_dphi(double r, double* phi, double* dphi);


/* Calculate pair potential phi(r) */
static void calc_phi(double r, double* phi)
{
   /* local variables */
   /*<FILL place any local variable definitions here>*/

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *phi = 0.0;
   }
   else
   {
      *phi = <FILL functional form of phi(r)>;
   }

   return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double r, double* phi)
{
   /* local variables */
   /*<FILL place any local variable definitions here>*/

   if (r > MODEL_CUTOFF)
   {
      /* Argument exceeds cutoff radius */
      *phi  = 0.0;
      *dphi = 0.0;
   }
   else
   {
      *phi  = <FILL functional form of phi(r)>;
      *dphi = <FILL functional form of dphi(r)>;
   }

   return;
}

/* compute function */
static void compute(void* km, int* ier)
{
   /* local static parameters */
   const static cutsq = MODEL_CUTOFF * MODEL_CUTOFF;
   
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dphi;
   double dx[DIM];
   int i;
   int j;
   int jj;
   int k;
   int numOfAtomNeigh;
   int requestedAtom;
   int currentAtom;
   int comp_force;
   int comp_energyPerAtom;
   int comp_virial;
   int IterOrLoca;
   int HalfOrFull;
   int NBC;
   char* NBCstr;

   intptr_t* nAtoms;
   int* atomTypes;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* energyPerAtom;
   double* virial;
   int* neighListOfCurrentAtom;
   double* boxlength;
   
   
   /* Determine neighbor list boundary condition (NBC) */
   /* and half versus full mode: */
   /*****************************
    * HalfOrFull = 1 -- Half
    *            = 2 -- Full
    *****************************/
   NBCstr = KIM_API_get_NBC_method(pkim, ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_NBC_method", *ier);
      return;
   }
   if (!strcmp("CLUSTER",NBCstr))
   {	   
      NBC = 0;
      HalfOrFull = 1;
   }
   else if (!strcmp("MI-OPBC-H",NBCstr))
   {	   
      NBC = 1;
      HalfOrFull = 1;
   }
   else if (!strcmp("MI-OPBC-F",NBCstr))
   {	   
      NBC = 1;
      HalfOrFull = 2;
   }

   else if (!strcmp("NEIGH-PURE-H",NBCstr))
   {	   
      NBC = 2;
      HalfOrFull = 1;
   }
   else if (!strcmp("NEIGH-PURE-F",NBCstr))
   {	   
      NBC = 2;
      HalfOrFull = 2;
   }
   else if (!strcmp("NEIGH-RVEC-F",NBCstr))
   {	   
      NBC = 3;
      HalfOrFull = 2;
   }
   else
   {
      *ier = 0;
      report_error(__LINE__, "Unknown NBC type", *ier);
      return;
   }
   free(NBCstr);

   /* determine neighbor list handling mode */
   if (NBC != 0)
   {
      /*****************************
       * IterOrLoca = 1 -- Iterator
       *            = 2 -- Locator
       *****************************/
      IterOrLoca = KIM_API_get_neigh_mode(pkim, ier);
      if ((1 > *ier) || ((IterOrLoca != 1) && (IterOrLoca != 2)))
      {
         report_error(__LINE__, "KIM_API_get_neigh_mode", *ier);
         return;
      }
   }
   else
   {
      IterOrLoca = 2;   /* for CLUSTER NBC */
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


   /* unpack data from KIM object */
   nAtoms = (intptr_t*) KIM_API_get_data(pkim, "numberOfAtoms", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }
   atomTypes= (int*) KIM_API_get_data(pkim, "atomTypes", ier);
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
   if (NBC == 1)
   {
      boxlength = (double*) KIM_API_get_data(pkim, "boxlength", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
      }
   }

   if (comp_energyPerAtom)
   {
      energyPerAtom = (double*) KIM_API_get_data(pkim, "energyPerAtom", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
      }
   }

   if (comp_force)
   {
      force = (double*) KIM_API_get_data(pkim, "forces", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
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
   }

   /* check to be sure the atom types are correct by comparing the provided species */
   /* codes to the value given here (which should be the same as that given in the  */
   /* .kim file).                                                                   */
   /**/
   *ier = 0; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if ( SPECCODE != atomTypes[i])
      {
         report_error(__LINE__, "Wrong atomType", i);
         return;
      }
   }
   *ier = 1; /* everything is ok */

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
      *virial = 0.0;
   }

   /* Initialize neighbor handling for CLUSTER NBC */
   if (0 == NBC) /* CLUSTER */
   {
      neighListOfCurrentAtom = (int *) malloc((*nAtoms)*sizeof(int));
   }

   /* Ready to setup for and perform the computation */

   if (1 == IterOrLoca) /* Iterator mode */
   {
      /* reset neighbor iterator */
      if (0 == (NBC%2)) /* full list */
      {
         *ier = KIM_API_get_full_neigh(pkim, 0, 0, &currentAtom,
                                       &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
      else /* half list */
      {
         *ier = KIM_API_get_half_neigh(pkim, 0, 0, &currentAtom,
                                       &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
      if (2 != *ier)
      {
         *ier = 0;
         return;
      }

      if (0 == (NBC%2)) /* full list */
      {
         *ier = KIM_API_get_full_neigh(pkim, 0, 1, &currentAtom,
                                       &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
      else /* half list */
      {
         *ier = KIM_API_get_half_neigh(pkim, 0, 1, &currentAtom,
                                       &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
      while (1 == *ier)
      {
         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            /* compute square distance */
            Rsqij = 0.0;
            for (k = 0; k < DIM; ++k)
            {
               if (NBC < 5) /* MI-OPBC-H/F & NEIGH-PURE-H/F */
               {
                  dx[k] = coords[neighListOfCurrentAtom[jj]*DIM + k] - coords[currentAtom*DIM + k];
                  
                  if ((NBC < 3) && (fabs(dx[k]) > 0.5*boxlength[k])) /* MI-OPBC-H/F */
                  {
                     dx[k] -= (dx[k]/fabs(dx[k]))*boxlength[k];
                  }
               }
               else /* NEIGH-RVEC-F */
               {
                  dx[k] = Rij[jj*DIM + k];
               }
               
               Rsqij += dx[k]*dx[k];
            }
            
            /* particles are interacting ? */
            if (Rsqij < cutsq)
            {
               R = sqrt(Rsqij);
               /* compute pair potential */
               if (comp_force || comp_virial)
               {
                  calc_phi_dphi(R, &phi, &dphi);
               }
               else
               {
                  calc_phi(R, &phi);
               }
               
               /* accumulate energy */
               if (comp_energyPerAtom)
               {
                  energyPerAtom[currentAtom] += 0.5*phi;
                  /* if half list add energy for the other atom in the pair */
                  if (1 == (NBC%2)) energyPerAtom[neighListOfCurrentAtom[jj]] += 0.5*phi;
               }
               else
               {
                  /* add the appropriate energy value based on half or full neighbor list */
                  *energy += ( (0 == (NBC%2)) ? (0.5*phi) : (phi) );
               }
               
               /* accumulate virial */
               if (comp_virial)
               {
                  /* add the appropriate viraial contribution based on half or full list */
                  *virial += ( (0 == (NBC%2)) ? 0.5 : 1.0 )*R*dphi;
               }
               
               /* accumulate force */
               if (comp_force)
               {
                  for (k = 0; k < DIM; ++k)
                  {
                     force[currentAtom*DIM + k] += dphi*dx[k]/R;
                     /* if half list add force terms for the other atom in the pair */
                     if (1 == (NBC%2)) force[neighListOfCurrentAtom[jj]*DIM + k] -= dphi*dx[k]/R;
                  }
               }
            }
         }
         
         /* increment iterator */
         if (0 == (NBC%2)) /* full list */
         {
            *ier = KIM_API_get_full_neigh(pkim, 0, 1, &currentAtom,
                                          &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
         }
         else /* half list */
         {
            *ier = KIM_API_get_half_neigh(pkim, 0, 1, &currentAtom,
                                          &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
         }
      }
   }
   else if (2 == IterOrLoca) /* Locator mode */
   {
      /* loop over atoms */
      for (i = 0; i < *nAtoms; ++i)
      {
         /* get neighbor list for atom i */
         if (0 == (NBC%2)) /* full list */
         {
            *ier = KIM_API_get_full_neigh(pkim, 1, i, &currentAtom,
                                          &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
         }
         else /* half list */
         {
            *ier = KIM_API_get_half_neigh(pkim, 1, i, &currentAtom,
                                          &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
         }
         
         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            /* compute square distance */
            Rsqij = 0.0;
            for (k = 0; k < DIM; ++k)
            {
               if (NBC < 5) /* MI-OPBC-H/F & NEIGH-PURE-H/F */
               {
                  dx[k] = coords[neighListOfCurrentAtom[jj]*DIM + k] - coords[currentAtom*DIM + k];
                  
                  if ((NBC < 3) && (fabs(dx[k]) > 0.5*boxlength[k])) /* MI-OPBC-H/F */
                  {
                     dx[k] -= (dx[k]/fabs(dx[k]))*boxlength[k];
                  }
               }
               else /* NEIGH-RVEC-F */
               {
                  dx[k] = Rij[jj*DIM + k];
               }
               
               Rsqij += dx[k]*dx[k];
            }
            
            /* particles are interacting ? */
            if (Rsqij < cutsq)
            {
               R = sqrt(Rsqij);
               /* compute pair potential */
               if (comp_force || comp_virial)
               {
                  calc_phi_dphi(R, &phi, &dphi);
               }
               else
               {
                  calc_phi(R, &phi);
               }
               
               /* accumulate energy */
               if (comp_energyPerAtom)
               {
                  energyPerAtom[currentAtom] += 0.5*phi;
                  /* if half list add energy for the other atom in the pair */
                  if (1 == (NBC%2)) energyPerAtom[neighListOfCurrentAtom[jj]] += 0.5*phi;
               }
               else
               {
                  /* add the appropriate energy value based on half or full neighbor list */
                  *energy += ( (0 == (NBC%2)) ? (0.5*phi) : (phi) );
               }
               
               /* accumulate virial */
               if (comp_virial)
               {
                  /* add the appropriate viraial contribution based on half or full list */
                  *virial += ( (0 == (NBC%2)) ? 0.5 : 1.0 )*R*dphi;
               }
               
               /* accumulate force */
               if (comp_force)
               {
                  for (k = 0; k < DIM; ++k)
                  {
                     force[currentAtom*DIM + k] += dphi*dx[k]/R;
                     /* if half list add force terms for the other atom in the pair */
                     if (1 == (NBC%2)) force[neighListOfCurrentAtom[jj]*DIM + k] -= dphi*dx[k]/R;
                  }
               }
            }
         }

      }
   }
   else /* unsupported IterOrLoca mode returned from KIM_API_get_neigh_mode() */
   {
      report_error(__LINE__, "KIM_API_get_neigh_mode", IterOrLoca);
      exit(-1);
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

   /* Free temporary storage */
   if (0 == NBC) 
   {
      free(neighListOfCurrentAtom);
   }

   /* everything is great */
   *ier = 1;
   return;
}

/* Initialization function */
void model_<FILL (lowercase) elemenet name>_p_<FILL (lowercase) model name>_init_(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_cutoff;
   int ier;

   /* store pointer to compute function in KIM object */
   if (! KIM_API_set_data(pkim, "compute", 1, (void*) &compute))
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
   *model_cutoff = MODEL_CUTOFF; /* cutoff distance in angstroms */

   return;
}

static void report_error(int line, char* str, int status)
{
   printf("* ERROR at line %i in %s: %s. kimerror = %i\n", line, __FILE__, str, status);
}
