/*******************************************************************************
*
*  model_driver_P_<FILL model driver name>
*
*  <FILL model drive name> pair potential KIM Model Driver
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
#include <string.h>
#include <math.h>
#include "KIMserviceC.h"

/******************************************************************************
* Below are the definitions and values of all Model parameters
*******************************************************************************/
#define DIM 3       /* dimensionality of space */
#define SPECCODE 1  /* internal species code */


/* Define prototypes for Model Driver init */
/* must be all lowercase to be compatible with the KIM API (to support Fortran Tests) */
/**/
void model_driver_p_<FILL (lowercase) model driver name>_init_(void* km, char** paramfile);

/* Define prototypes for Model (Driver) reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models    */
/**/
static void reinit(void* km);
static void destroy(void* km);
static void compute(void* km, int* ier);
static void report_error(int line, char* str, int status);
/**/
static void calc_phi(double* <FILL parameter 1>,
                     double* <FILL parameter 2>,
                     /* FILL as many parameters as needed */
                     double* cutoff, double r, double* phi);
static void calc_phi_dphi(double* <FILL parameter 1>,
                          double* <FILL parameter 2>,
                          /* FILL as many parameters as needed */
                          double* cutoff, double r, double* phi, double* dphi);


/* Calculate pair potential phi(r) */
static void calc_phi(double* <FILL parameter 1>,
                     double* <FILL parameter 2>,
                     /* FILL as many parameters as needed */
                     double* cutoff, double r, double* phi)
{
   /* local variables */
   /* FILL: place any local variable definitions here */
   
   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi = 0.0;
   }
   else
   {
      *phi   = <FILL functional form of phi(r)>;
   }

   return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double* <FILL parameter 1>,
                          double* <FILL parameter 2>,
                          /* FILL as many parameters as needed */
                          double* cutoff, double r, double* phi, double* dphi)
{
   /* local variables */
   /* FILL: place any local variable definitions here */

   if (r > *cutoff)
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
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dphi;
   double Rij[DIM];
   int i;
   int j;
   int jj;
   int k;
   int numOfAtomNeigh;
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
   double* cutoff;
   double* cutsq;
   double* <FILL parameter 1>;
   double* <FILL parameter 2>;
   /* FILL as many parameters as needed */
   double* Rij_list;
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
      report_error(__LINE__, "Unknown NBC method", *ier);
      return;
   }
   free(NBCstr); /* don't forget to release the memory... */

   /* determine neighbor list handling mode */
   if (NBC != 0)
   {
      /*****************************
       * IterOrLoca = 1 -- Iterator
       *            = 2 -- Locator
       *****************************/
      IterOrLoca = KIM_API_get_neigh_mode(pkim, ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_neigh_mode", *ier);
         return;
      }
      if ((IterOrLoca != 1) && (IterOrLoca != 2))
      {
         printf("* ERROR: Unsupported IterOrLoca mode = %i\n", IterOrLoca);
         exit(-1);
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

   if (comp_force)
   {
      force = (double*) KIM_API_get_data(pkim, "forces", ier);
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

   if (comp_virial)
   {
      virial = (double*) KIM_API_get_data(pkim, "virial", ier);
      if (1 > *ier)
      {
         report_error(__LINE__, "KIM_API_get_data", *ier);
         return;
      }
   }


   /* unpack the Model's parameters stored in the KIM API object */

   cutoff = (double*) KIM_API_get_data(pkim, "cutoff", ier);
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

   /* FILL as many parameters as needed */

   <FILL parameter 1> = (double*) KIM_API_get_data(pkim,"PARAM_FREE_<FILL parameter 1>", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }

   <FILL parameter 2> = (double*) KIM_API_get_data(pkim,"PARAM_FREE_<FILL parameter 2>", ier);
   if (1 > *ier)
   {
      report_error(__LINE__, "KIM_API_get_data", *ier);
      return;
   }

   /* also FILL PARAM_FIXED_* parameters here if there are any ... */

   /* Check to be sure that the atom types are correct */
   /**/
   *ier = 0; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if ( SPECCODE != atomTypes[i])
      {
         report_error(__LINE__, "Unexpected species type detected", i);
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

   /* Initialize neighbor handling for Iterator mode */

   if (1 == IterOrLoca)
   {
      if (1 == HalfOrFull) /* HALF list */
      {
         *ier = KIM_API_get_half_neigh(pkim, 0, 0, &currentAtom, &numOfAtomNeigh,
                                       &neighListOfCurrentAtom, &Rij_list);
      }
      else                 /* FULL list */
      {
         *ier = KIM_API_get_full_neigh(pkim, 0, 0, &currentAtom, &numOfAtomNeigh,
                                       &neighListOfCurrentAtom, &Rij_list);
      }
      /* check for successful initialization */
      if (2 != *ier) /* ier == 2 upon successful initialization */
      {
         if (1 == HalfOrFull)
         {
            report_error(__LINE__, "KIM_API_get_half_neigh", *ier);
         }
         else
         {
            report_error(__LINE__, "KIM_API_get_full_neigh", *ier);
         }
         ier = 0;
         return;
      }
   }

   /* Compute enery and forces */

   /* loop over particles and compute enregy and forces */
   i = -1;
   while( 1 )
   {

      /* Set up neighbor list for next atom for all NBC methods */
      if (1 == IterOrLoca) /* ITERATOR mode */
      {
         if (1 == HalfOrFull) /* HALF list */
         {
            *ier = KIM_API_get_half_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh,
                                          &neighListOfCurrentAtom, &Rij_list);  
         }
         else
         {
            *ier = KIM_API_get_full_neigh(pkim, 0, 1, &currentAtom, &numOfAtomNeigh,
                                          &neighListOfCurrentAtom, &Rij_list);
         }
         if (0 > *ier) /* some sort of problem, exit */
         {
            if (1 == HalfOrFull)
            {
               report_error(__LINE__, "KIM_API_get_half_neigh", *ier);
            }
            else
            {
               report_error(__LINE__, "KIM_API_get_full_neigh", *ier);
            }
            return;
         }
         if (0 == *ier) /* ier==0 means that the iterator has been incremented past */
         {              /* the end of the list, terminate loop                      */
            break;
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

         if (1 == HalfOrFull) /* HALF list */
         {
            if (0 == NBC)     /* CLUSTER NBC method */
            {
               numOfAtomNeigh = *nAtoms - (i + 1);
               for (k = 0; k < numOfAtomNeigh; ++k)
               {
                  neighListOfCurrentAtom[k] = i + k + 1;
               }
               *ier = 1;
            }
            else
            {
               *ier = KIM_API_get_half_neigh(pkim, 1, i, &currentAtom, &numOfAtomNeigh,
                                             &neighListOfCurrentAtom, &Rij_list);
            }
         }
         else                 /* FULL list */
         {
            *ier = KIM_API_get_full_neigh(pkim, 1, i, &currentAtom, &numOfAtomNeigh,
                                          &neighListOfCurrentAtom, &Rij_list);
         }
      }
      if (1 != *ier) /* some sort of problem, exit */
      {
         if (1 == HalfOrFull)
         {
            report_error(__LINE__, "KIM_API_get_half_neigh", *ier);
         }
         else
         {
            report_error(__LINE__, "KIM_API_get_full_neigh", *ier);
         }
         *ier = 0;
         return;
      }
            
      /* loop over the neighbors of atom i */
      for (jj = 0; jj < numOfAtomNeigh; ++ jj)
      {

         j = neighListOfCurrentAtom[jj]; /* get neighbor ID */

         /* compute relative position vector and squared distance */
         Rsqij = 0.0;
         for (k = 0; k < DIM; ++k)
         {
            if (3 != NBC) /* all methods except NEIGH-RVEC-F */
            {
               Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];
            }
            else          /* NEIGH-RVEC-F method */
            {
               Rij[k] = Rij_list[jj*DIM + k];
            }

            /* apply periodic boundary conditions if required */
            if (1 == NBC)
            {
               if (abs(Rij[k]) > 0.5*boxlength[k])
               {
                  Rij[k] -= (Rij[k]/fabs(Rij[k]))*boxlength[k];
               }
            }
            
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
               calc_phi_dphi(<FILL parameter 1>,
                             <FILL parameter 2>,
                             /* FILL as many parameters as needed */
                             cutoff, R, &phi, &dphi);
            }
            else
            {
               /* compute just pair potential */
               calc_phi(<FILL parameter 1>,
                        <FILL parameter 2>,
                        /* FILL as many parameters as needed */
                        cutoff, R, &phi);
            }
            
            /* contribution to energy */
            if (comp_energyPerAtom)
            {
               energyPerAtom[i] += 0.5*phi;
               /* if half list add energy for the other atom in the pair */
               if (1 == HalfOrFull) energyPerAtom[j] += 0.5*phi;
            }
            else
            {
               if (1 == HalfOrFull)
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
            
            /* contribution to virial perssure */
            if (comp_virial)
            {
               if (1 == HalfOrFull)
               {
                  /* Half mode -- varial = sum r*(dphi/dr) */
                  *virial += R*dphi;
               }
               else
               {
                  /* Full mode -- varial = sum 0.6*r*(dphi/dr) */
                  *virial += 0.5*R*dphi;
               }
            }
            
            /* contribution to forces */
            if (comp_force)
            {
               for (k = 0; k < DIM; ++k)
               {
                  force[i*DIM + k] += dphi*Rij[k]/R; /* accumulate force on atom i */
                  if (1 == HalfOrFull)
                  {
                     force[j*DIM + k] -= dphi*Rij[k]/R; /* Fji = -Fij */
                  }
               }
            }
         }
      } /* loop on jj */
   }    /* infinite while loop (terminated by break statements above */
   

   /* perform final tasks */
   
   if (comp_virial)
   {
      *virial = -*virial/( (double) DIM); /* definition of virial term */
   }

   if (comp_energyPerAtom)
   {
      *energy = 0.0;
      for (k = 0; k < *nAtoms; ++k)
      {
         *energy += energyPerAtom[k];
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
void model_driver_p_<FILL (lowercase) model driver name>_init_(void *km, char** paramfile)
{
   /* Local variables */
   double cutoff;
   double <FILL parameter 1>;
   double <FILL parameter 2>;
   /* FILL as many parameters as needed */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_Pcutoff;
   double* model_cutoff;
   double* model_cutsq;
   double* model_<FILL parameter 1>;
   double* model_<FILL parameter 2>;
   /* FILL as many parameters as needed */
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

   /* Read in model parameters from parameter file */
   ier = sscanf(*paramfile, "%lf \n%lf \n<FILL as many parameters as needed>",
                &cutoff,             /* cutoff distance in angstroms */
                &<FILL parameter 1>,
                &<FILL parameter 2>,
                /* FILL as many parameters as needed */
               );
   if (<FILL number of parameters (including cutoff)> != ier)
   {
      report_error(__LINE__, "Unable to read all <FILL model driver name> parameters", ier);
      exit(1);
   }

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = cutoff;

   /* allocate memory for parameter cutoff and store value */
   model_Pcutoff = (double*) malloc(1*sizeof(double));
   if (NULL == model_Pcutoff)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_Pcutoff in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FREE_cutoff", 1, (void*) model_Pcutoff))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter cutoff */
   *model_Pcutoff = *model_cutoff;

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

   /* allocate memory for <FILL parameter 1> and store value */
   model_<FILL parameter 1> = (double*) malloc(1*sizeof(double));
   if (NULL == model_<FILL parameter 1>)
   {
      report_error(__LINE__, "malloc", ier);
      exit(1);
   }
   /* store model_<FILL parameter 1> in KIM object */
   if (! KIM_API_set_data(pkim, "PARAM_FREE_<FILL parameter 1>", 1, (void*) model_<FILL parameter 1>))
   {
      report_error(__LINE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of <FILL parameter 1> */
   *model_<FILL parameter 1> = <FILL parameter 1>;

   /* FILL: repeat above statements as many times as necessary for all parameters.
      Use "FREE" and "FIXED" as appropriate. (Recall FREE parameters can be modified by
      the calling routine. FIXED parameters depend on the FREE parameters and must be
      appropriately adjusted in the reinit() method.)  */

   return;
}

static void report_error(int line, char* str, int status)
{
   printf("* ERROR at line %i in %s: %s. kimerror = %i\n", line, __FILE__, str, status);
}

/* Reinitialization function */
static void reinit(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_cutoff;
   double* model_cutsq;
   double* model_<FILL parameter 1>;
   double* model_<FILL parameter 2>;
   /* FILL as many parameters as needed */
   double* model_Pcutoff;
   int ier;

   /* get (changed) parameters from KIM object */

   /* get parameter cutoff from KIM object */
   model_Pcutoff = (double*) KIM_API_get_data(pkim, "PARAM_FREE_cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get <FILL parameter 1> from KIM object */
   model_<FILL parameter 1> = (double*) KIM_API_get_data(pkim, "PARAM_FREE_<FILL parameter 1>", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get <FILL parameter 2> from KIM object */
   model_<FILL parameter 2> = (double*) KIM_API_get_data(pkim, "PARAM_FREE_<FILL parameter 2>", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* FILL as many parameters as needed */


   
   /* set new values in KIM object */
   

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = *model_Pcutoff;

   /* store model_cutsq in KIM object */
   model_cutsq = KIM_API_get_data(pkim, "PARAM_FIXED_cutsq", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   /* set value of parameter cutsq */
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

   /* FILL: store any other FIXED parameters whose values depend on FREE parameters */

   return;
}

/* destroy function */
static void destroy(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* model_Pcutoff;
   double* model_cutsq;
   double* model_<FILL parameter 1>;
   double* model_<FILL parameter 2>;
   /* FILL as many parameters as needed */
   int ier;

   /* get and free parameter cutoff from KIM object */
   model_Pcutoff = (double*) KIM_API_get_data(pkim, "PARAM_FREE_cutoff", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_Pcutoff);

   /* get and free model_cutsq in KIM object */
   model_cutsq = KIM_API_get_data(pkim, "PARAM_FIXED_cutsq", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_cutsq);

   /* get and free <FILL parameter 1> from KIM object */
   model_<FILL parameter 1> = (double*) KIM_API_get_data(pkim, "PARAM_FREE_<FILL parameter 1>", &ier);
   if (1 > ier)
   {
      report_error(__LINE__, "KIM_API_get_data", ier);
      exit(1);
   }
   free(model_<FILL parameter 1>);

   /* FILL: repeat above statements as many times as necessary for all FREE and FIXED parameters. */

   return;
}
