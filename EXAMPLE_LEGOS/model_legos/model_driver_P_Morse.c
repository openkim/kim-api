/*******************************************************************************
*
*  MODEL_DRIVER_NAME_STR
*
*  Morse pair potential KIM Model Driver
*  shifted to have zero energy at the cutoff radius
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
#define SPECCODE 1  /* internal species code */


/* Define prototypes for Model Driver init */
/* must be all lowercase to be compatible with the KIM API (to support Fortran Tests) */
/**/
void MODEL_DRIVER_NAME_LC_STR_init_(void* km, char* paramfile, int* length);

/* Define prototypes for Model (Driver) reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models    */
/**/
static void reinit(void* km);
static void destroy(void* km);
static void compute(void* km, int* ier);
/**/
static void calc_phi(double* epsilon,
                     double* C,
                     double* Rzero,
		     double* shift,
                     double* cutoff, double r, double* phi);
static void calc_phi_dphi(double* epsilon,
                          double* C,
                          double* Rzero,
			  double* shift,
                          double* cutoff, double r, double* phi, double* dphi);

/* Define model_buffer structure */
struct model_buffer {
   int NBC;
   int HalfOrFull;
   int IterOrLoca;
   int energy_ind;
   int forces_ind;
   int energyPerAtom_ind;
   int process_d1Edr_ind;
   int model_index_shift;
   
   int* numberOfAtoms;
   int atomTypes_ind;
   int coordinates_ind;
   int* numberContributingAtoms;
   int boxlength_ind;
   double* cutoff;
   int (*get_half_neigh)(void *,int *,int *,int *, int *, int **, double **);
   int (*get_full_neigh)(void *,int *,int *,int *, int *, int **, double **);


   double* Pcutoff;
   double* cutsq;
   double* epsilon;
   double* C;
   double* Rzero;
   double* shift;
};
/* prototype for buffer setup routine */
static void setup_buffer(intptr_t* pkim, struct model_buffer* buffer);


/* Calculate pair potential phi(r) */
static void calc_phi(double* epsilon,
                     double* C,
                     double* Rzero,
		     double* shift,
                     double* cutoff, double r, double* phi)
{
   /* local variables */
   double ep;
   double ep2;
   
   ep  = exp(-(*C)*(r-*Rzero));
   ep2 = ep*ep;

   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi = 0.0;
   }
   else
   {
      *phi   = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
   }

   return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double* epsilon,
                          double* C,
                          double* Rzero,
			  double* shift,
                          double* cutoff, double r, double* phi, double* dphi)
{
   /* local variables */
   double ep;
   double ep2;
   
   ep  = exp(-(*C)*(r-*Rzero));
   ep2 = ep*ep;

   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi  = 0.0;
      *dphi = 0.0;
   }
   else
   {
      *phi  = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
      *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 );
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
   double dEidr;
   double Rij[DIM];
   double * pRij = Rij;
   int i;
   int j;
   int jj;
   int k;
   int currentAtom;
   int* neighListOfCurrentAtom;
   struct model_buffer* buffer;
   int comp_energy;
   int comp_force;
   int comp_energyPerAtom;
   int comp_process_d1Edr;
   int NBC;
   int HalfOrFull;
   int IterOrLoca;
   int model_index_shift;
   int zero = 0;
   int one = 1;
   int request;
   
   int* nAtoms;
   int* atomTypes;
   double* cutoff;
   double* cutsq;
   double* epsilon;
   double* C;
   double* Rzero;
   double* shift;
   double* Rij_list;
   double* coords;
   double* energy;
   double* force;
   double* energyPerAtom;
   double* boxlength;
   int* numContrib;
   int numberContrib;
   int numOfAtomNeigh;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", *ier);
      return;
   }

   /* unpack info from the buffer */
   NBC = buffer->NBC;
   HalfOrFull = buffer->HalfOrFull;
   IterOrLoca = buffer->IterOrLoca;
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutoff = buffer->cutoff;
   cutsq = buffer->cutsq;
   epsilon = buffer->epsilon;
   C = buffer->C;
   Rzero = buffer->Rzero;
   shift = buffer->shift;

   
   /* check to see if we have been asked to compute the forces, energyPerAtom, and d1Edr */
   comp_energy = KIM_API_isit_compute_byI(pkim, buffer->energy_ind, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_isit_compute_byI", *ier);
      return;
   }
   comp_force = KIM_API_isit_compute_byI(pkim, buffer->forces_ind, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_isit_compute_byI", *ier);
      return;
   }
   comp_energyPerAtom = KIM_API_isit_compute_byI(pkim, buffer->energyPerAtom_ind, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_isit_compute_byI", *ier);
      return;
   }
   comp_process_d1Edr = KIM_API_isit_compute_byI(pkim, buffer->process_d1Edr_ind, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_isit_compute_byI", *ier);
      return;
   }

   /* unpack data from KIM object */
   nAtoms = buffer->numberOfAtoms;
   atomTypes= (int*) KIM_API_get_data_byI(pkim, buffer->atomTypes_ind, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI", *ier);
      return;
   }
   coords = (double*) KIM_API_get_data_byI(pkim, buffer->coordinates_ind, ier);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI", *ier);
      return;
   }
   if (buffer->HalfOrFull == 1)
   {
      numContrib = buffer->numberContributingAtoms;
      if (0 != NBC) /* non-CLUSTER cases */
      {
         numberContrib = *numContrib;
      }
      else /* CLUSTER cases */
      {
         numberContrib = *nAtoms;
      }
   }
   if (NBC == 1)
   {
      boxlength = (double*) KIM_API_get_data_byI(pkim, buffer->boxlength_ind, ier);
      if (KIM_STATUS_OK > *ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI", *ier);
         return;
      }
   }

   if (comp_energy)
   {
      energy = (double*) KIM_API_get_data_byI(pkim, buffer->energy_ind, ier);
      if (KIM_STATUS_OK > *ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI", *ier);
         return;
      }
   }	   

   if (comp_force)
   {
      force = (double*) KIM_API_get_data_byI(pkim, buffer->forces_ind, ier);
      if (KIM_STATUS_OK > *ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI", *ier);
         return;
      }
   }

   if (comp_energyPerAtom)
   {
      energyPerAtom = (double*) KIM_API_get_data_byI(pkim, buffer->energyPerAtom_ind, ier);
      if (KIM_STATUS_OK > *ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI", *ier);
         return;
      }
   }

   /* Check to be sure that the atom types are correct */
   /**/
   *ier = KIM_STATUS_FAIL; /* assume an error */
   for (i = 0; i < *nAtoms; ++i)
   {
      if ( SPECCODE != atomTypes[i])
      {
         KIM_API_report_error(__LINE__, __FILE__, "Unexpected species type detected", *ier);
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
         *ier = (*buffer->get_half_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh,
                                          &neighListOfCurrentAtom, &Rij_list);
      }
      else                 /* FULL list */
      {
         *ier = (*buffer->get_full_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh,
                                          &neighListOfCurrentAtom, &Rij_list);
      }
      /* check for successful initialization */
      if (KIM_STATUS_NEIGH_ITER_INIT_OK != *ier)
      {
         if (1 == HalfOrFull)
         {
            KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_half_neigh", *ier);
         }
         else
         {
            KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_full_neigh", *ier);
         }
         *ier = KIM_STATUS_FAIL;
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
            *ier = (*buffer->get_half_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh,
                                             &neighListOfCurrentAtom, &Rij_list);  
         }
         else
         {
            *ier = (*buffer->get_full_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh,
                                             &neighListOfCurrentAtom, &Rij_list);
         }
         if (KIM_STATUS_NEIGH_ITER_PAST_END == *ier) /* the end of the list, terminate loop */
         {
            break;
         }
         if (KIM_STATUS_OK > *ier) /* some sort of problem, exit */
         {
            if (1 == HalfOrFull)
            {
               KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_half_neigh", *ier);
            }
            else
            {
               KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_full_neigh", *ier);
            }
            return;
         }

         i = currentAtom + model_index_shift;
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
                  neighListOfCurrentAtom[k] = i + k + 1 - model_index_shift;
               }
               *ier = KIM_STATUS_OK;
            }
            else
            {
               request = i - model_index_shift;
               *ier = (*buffer->get_half_neigh)(&pkim, &one, &request,
                                                &currentAtom, &numOfAtomNeigh,
                                                &neighListOfCurrentAtom, &Rij_list);
            }
         }
         else                 /* FULL list */
         {
            request = i - model_index_shift;
            *ier = (*buffer->get_full_neigh)(&pkim, &one, &request,
                                             &currentAtom, &numOfAtomNeigh,
                                             &neighListOfCurrentAtom, &Rij_list);
         }
      }
      if (KIM_STATUS_OK != *ier) /* some sort of problem, exit */
      {
         if (1 == HalfOrFull)
         {
            KIM_API_report_error(__LINE__, __FILE__, "get_half_neigh", *ier);
         }
         else
         {
            KIM_API_report_error(__LINE__, __FILE__, "get_full_neigh", *ier);
         }
         *ier = KIM_STATUS_FAIL;
         return;
      }
            
      /* loop over the neighbors of atom i */
      for (jj = 0; jj < numOfAtomNeigh; ++ jj)
      {

         j = neighListOfCurrentAtom[jj] + model_index_shift; /* get neighbor ID */

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
            if (comp_force || comp_process_d1Edr)
            {
               /* compute pair potential and its derivative */
               calc_phi_dphi(epsilon,
                             C,
                             Rzero,
			     shift,
                             cutoff, R, &phi, &dphi);

               /* compute dEidr */
               if ((1 == HalfOrFull) && (j < numberContrib))
               {
                  /* Half mode -- double contribution */
                  dEidr = dphi;
               }
               else
               {
                  /* Full mode -- regular contribution */
                  dEidr = 0.5*dphi;
               }
            }
            else
            {
               /* compute just pair potential */
               calc_phi(epsilon,
                        C,
                        Rzero,
			shift,
                        cutoff, R, &phi);
            }
            
            /* contribution to energy */
            if (comp_energyPerAtom)
            {
               energyPerAtom[i] += 0.5*phi;
               /* if half list add energy for the other atom in the pair */
               if ((1 == HalfOrFull) && (j < numberContrib)) energyPerAtom[j] += 0.5*phi;
            }
            else if (comp_energy)
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
            
            /* contribution to process_d1Edr */
            if (comp_process_d1Edr)
            {
               KIM_API_process_d1Edr(km, &dEidr, &R, &pRij, &i, &j, ier);
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
   
   /* perform final tasks */
   
   if (comp_energyPerAtom && comp_energy)
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
   *ier = KIM_STATUS_OK;

   return;
}

/* Initialization function */
void MODEL_DRIVER_NAME_LC_STR_init_(void *km, char* paramfile, int* length)
{
   /* Local variables */
   double cutoff;
   double epsilon;
   double C;
   double Rzero;
   intptr_t* pkim = *((intptr_t**) km);
   double* model_Pcutoff;
   double* model_cutoff;
   double* model_cutsq;
   double* model_epsilon;
   double* model_C;
   double* model_Rzero;
   double* model_shift;
   int ier;
   double dummy;
   struct model_buffer* buffer;

   /* store pointer to compute function in KIM object */
   ier = KIM_API_set_data(pkim, "compute", 1, (void*) &compute);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* store pointer to reinit function in KIM object */
   ier = KIM_API_set_data(pkim, "reinit", 1, (void*) &reinit);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* store pointer to destroy function in KIM object */
   ier = KIM_API_set_data(pkim, "destroy", 1, (void*) &destroy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }

   /* Read in model parameters from parameter file */
   ier = sscanf(paramfile, "%lf \n%lf \n%lf \n%lf",
                &cutoff,  /* cutoff distance in angstroms */
                &epsilon, /* Morse epsilon in eV */
                &C,       /* Morse C in 1/Angstroms */
                &Rzero);  /* Morse Rzero in Angstroms */
   if (4 != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "Unable to read all Morse parameters", KIM_STATUS_FAIL);
      exit(1);
   }

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }
   *model_cutoff = cutoff;

   /* allocate memory for parameter cutoff and store value */
   model_Pcutoff = (double*) malloc(1*sizeof(double));
   if (NULL == model_Pcutoff)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   /* store model_Pcutoff in KIM object */
   ier = KIM_API_set_data(pkim, "PARAM_FREE_cutoff", 1, (void*) model_Pcutoff);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter cutoff */
   *model_Pcutoff = *model_cutoff;

   /* allocate memory for parameter cutsq and store value */
   model_cutsq = (double*) malloc(1*sizeof(double));
   if (NULL == model_cutsq)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   /* store model_cutsq in KIM object */
   ier = KIM_API_set_data(pkim, "PARAM_FIXED_cutsq", 1, (void*) model_cutsq);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter cutsq */
   *model_cutsq = (*model_cutoff)*(*model_cutoff);

   /* allocate memory for epsilon and store value */
   model_epsilon = (double*) malloc(1*sizeof(double));
   if (NULL == model_epsilon)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   /* store model_epsilon in KIM object */
   ier = KIM_API_set_data(pkim, "PARAM_FREE_epsilon", 1, (void*) model_epsilon);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of epsilon */
   *model_epsilon = epsilon;

   /* allocate memory for C and store value */
   model_C = (double*) malloc(1*sizeof(double));
   if (NULL == model_C)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   /* store model_C in KIM object */
   ier = KIM_API_set_data(pkim, "PARAM_FREE_C", 1, (void*) model_C);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of C */
   *model_C = C;

   /* allocate memory for Rzero and store value */
   model_Rzero = (double*) malloc(1*sizeof(double));
   if (NULL == model_Rzero)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   /* store model_Rzero in KIM object */
   ier = KIM_API_set_data(pkim, "PARAM_FREE_Rzero", 1, (void*) model_Rzero);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of Rzero */
   *model_Rzero = Rzero;

   /* allocate memory for parameter shift and store value */
   model_shift = (double*) malloc(1*sizeof(double));
   if (NULL == model_shift)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   /* store model_shift in KIM object */
   ier = KIM_API_set_data(pkim, "PARAM_FIXED_shift", 1, (void*) model_shift);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);
      exit(1);
   }
   /* set value of parameter shift */
   dummy = 0.0;
   /* call calc_phi with r=cutoff and shift=0.0 */
   calc_phi(model_epsilon,
            model_C,
            model_Rzero,
	    &dummy,
            model_cutoff, *model_cutoff, model_shift);
   /* set shift to -shift */
   *model_shift = -(*model_shift);


   /* allocate buffer */
   buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));
   /* setup buffer */
   setup_buffer(pkim, buffer);
   /* store in model buffer */
   KIM_API_set_model_buffer(pkim, (void*) buffer, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_model_buffer", ier);
      exit(1);
   }
   
   return;
}

/* Reinitialization function */
static void reinit(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   int ier;
   double dummy;
   struct model_buffer* buffer;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      exit(1);
   }
   /* re-setup buffer */
   setup_buffer(pkim, buffer);

   /* set new values in KIM object     */
   /*                                  */
   /* store model cutoff in KIM object */
   *buffer->cutoff = *buffer->Pcutoff;

   /* set value of parameter cutsq */
   *buffer->cutsq = (*buffer->cutoff)*(*buffer->cutoff);

   /* set value of parameter shift */
   dummy = 0.0;
   /* call calc_phi with r=cutoff and shift=0.0 */
   calc_phi(buffer->epsilon,
            buffer->C,
            buffer->Rzero,
	    &dummy,
            buffer->cutoff, *buffer->cutoff, buffer->shift);
   /* set shift to -shift */
   *buffer->shift = -(*buffer->shift);

   return;
}

/* destroy function */
static void destroy(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   struct model_buffer* buffer;
   int ier;

   /* get model buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      exit(1);
   }

   /* free parameter cutoff */
   free(buffer->Pcutoff);

   /* free model_cutsq */
   free(buffer->cutsq);

   /* free epsilon */
   free(buffer->epsilon);

   /* free C */
   free(buffer->C);

   /* free Rzero */
   free(buffer->Rzero);

   /* free shift */
   free(buffer->shift);

   /* destroy the buffer */
   free(buffer);
   
   return;
}

/* buffer setup function */
static void setup_buffer(intptr_t* pkim, struct model_buffer* buffer)
{
   /* Local variables */
   int ier;
   char* NBCstr;

   
   /* Determine neighbor list boundary condition (NBC) */
   NBCstr = KIM_API_get_NBC_method(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);
      return;
   }
   if (!strcmp("CLUSTER",NBCstr))
   {	   
      buffer->NBC = 0;
   }
   else if ((!strcmp("MI-OPBC-H",NBCstr)) || (!strcmp("MI-OPBC-F",NBCstr)))
   {	   
      buffer->NBC = 1;
   }
   else if ((!strcmp("NEIGH-PURE-H",NBCstr)) || (!strcmp("NEIGH-PURE-F",NBCstr)))
   {	   
      buffer->NBC = 2;
   }
   else if (!strcmp("NEIGH-RVEC-F",NBCstr))
   {	   
      buffer->NBC = 3;
   }
   else
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);
      return;
   }
   free(NBCstr); /* don't forget to release the memory... */

   /* Determine if Half or Full neighbor lists are being used */
   /*****************************
    * HalfOrFull = 1 -- Half
    *            = 2 -- Full
    *****************************/
   if (KIM_API_isit_half_neighbors(pkim, &ier))
   {
      buffer->HalfOrFull = 1;
   }
   else
   {
      buffer->HalfOrFull = 2;
   }

   /* determine neighbor list handling mode */
   if (buffer->NBC != 0)
   {
      /*****************************
       * IterOrLoca = 1 -- Iterator
       *            = 2 -- Locator
       *****************************/
      buffer->IterOrLoca = KIM_API_get_neigh_mode(pkim, &ier);
      if (KIM_STATUS_OK > ier)
      {
         KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh_mode", ier);
         return;
      }
      if ((buffer->IterOrLoca != 1) && (buffer->IterOrLoca != 2))
      {
         printf("* ERROR: Unsupported IterOrLoca mode = %i\n", buffer->IterOrLoca);
         exit(-1);
      }
   }
   else
   {
      buffer->IterOrLoca = 2;   /* for CLUSTER NBC */
   }

   buffer->energy_ind = KIM_API_get_index(pkim, "energy", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }
   buffer->forces_ind = KIM_API_get_index(pkim, "forces", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }
   buffer->energyPerAtom_ind = KIM_API_get_index(pkim, "energyPerAtom", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }
   buffer->process_d1Edr_ind = KIM_API_get_index(pkim, "process_d1Edr", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }

   buffer->model_index_shift = KIM_API_get_model_index_shift(pkim);
 
   buffer->numberOfAtoms = (int*) KIM_API_get_data(pkim, "numberOfAtoms", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   buffer->atomTypes_ind = KIM_API_get_index(pkim, "atomTypes", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }

   buffer->coordinates_ind = KIM_API_get_index(pkim, "coordinates", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }

   
   buffer->numberContributingAtoms =
      (int*) KIM_API_get_data(pkim, "numberContributingAtoms", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   buffer->boxlength_ind = KIM_API_get_index(pkim, "boxlength", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index", ier);
      exit(1);
   }

   /* get pointers to get_neigh functions */
   buffer->get_half_neigh = (int(*)(void *,int *,int *,int *, int *, int **, double **))
      KIM_API_get_data(pkim, "get_half_neigh", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }
   buffer->get_full_neigh = (int(*)(void *,int *,int *,int *, int *, int **, double **))
      KIM_API_get_data(pkim, "get_full_neigh", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get model cutoff */
   buffer->cutoff = (double*) KIM_API_get_data(pkim,"cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get parameter cutoff from KIM object */
   buffer->Pcutoff = (double*) KIM_API_get_data(pkim, "PARAM_FREE_cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get cutsq from KIM object */
   buffer->cutsq = KIM_API_get_data(pkim, "PARAM_FIXED_cutsq", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get epsilon from KIM object */
   buffer->epsilon = (double*) KIM_API_get_data(pkim, "PARAM_FREE_epsilon", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get C from KIM object */
   buffer->C = (double*) KIM_API_get_data(pkim, "PARAM_FREE_C", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get Rzero from KIM object */
   buffer->Rzero = (double*) KIM_API_get_data(pkim, "PARAM_FREE_Rzero", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   /* get shift from KIM object */
   buffer->shift = KIM_API_get_data(pkim, "PARAM_FIXED_shift", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      exit(1);
   }

   return;
}
