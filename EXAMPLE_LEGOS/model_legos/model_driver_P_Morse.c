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

static void calc_phi_d2phi(double* epsilon,
                           double* C,
                           double* Rzero,
                           double* shift,
                           double* cutoff, double r, double* phi, double* dphi, double* d2phi);

/* Define model_buffer structure */
struct model_buffer {
   int NBC;
   int HalfOrFull;
   int IterOrLoca;
   int energy_ind;
   int forces_ind;
   int energyPerAtom_ind;
   int process_d1Edr_ind;
   int process_d2Edr_ind;
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

/* Calculate pair potential phi(r) and its 1st & 2nd derivatives dphi(r), d2phi(r) */
static void calc_phi_d2phi(double* epsilon,
                           double* C,
                           double* Rzero,
                           double* shift,
                           double* cutoff, double r, double* phi, double* dphi, double* d2phi)
{
   /* local variables */
   double ep;
   double ep2;
   
   ep  = exp(-(*C)*(r-*Rzero));
   ep2 = ep*ep;

   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi   = 0.0;
      *dphi  = 0.0;
      *d2phi = 0.0;
   }
   else
   {
      *phi   = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
      *dphi  = 2.0*(*epsilon)*(*C)*( -ep + ep2 );
      *d2phi = 2.0*(*epsilon)*(*C)*(*C)*(ep - 2.0*ep2);
   }

   return;
}

/* compute function */
static void compute(void* km, int* ier)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double R_pairs[2];
   double *pR_pairs = &(R_pairs[0]);
   double Rsqij;
   double phi;
   double dphi;
   double d2phi;
   double dEidr;
   double d2Eidr;
   double Rij[DIM];
   double *pRij = &(Rij[0]);
   double Rij_pairs[2][3];
   double *pRij_pairs = &(Rij_pairs[0][0]);
   int i;
   int i_pairs[2];
   int *pi_pairs = &(i_pairs[0]);
   int j;
   int j_pairs[2];
   int *pj_pairs = &(j_pairs[0]);
   int jj;
   int k;
   int currentAtom;
   int* neighListOfCurrentAtom;
   struct model_buffer* buffer;
   int comp_energy;
   int comp_force;
   int comp_energyPerAtom;
   int comp_process_d1Edr;
   int comp_process_d2Edr;
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
   KIM_API_get_compute_byI_multiple(pkim, ier, 5*3,
                                    buffer->energy_ind,        &comp_energy,        1,
                                    buffer->forces_ind,        &comp_force,         1,
                                    buffer->energyPerAtom_ind, &comp_energyPerAtom, 1,
                                    buffer->process_d1Edr_ind, &comp_process_d1Edr, 1,
                                    buffer->process_d2Edr_ind, &comp_process_d2Edr, 1);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_compute_byI_multiple", *ier);
      return;
   }

   /* unpack data from KIM object and/or buffer */
   nAtoms = buffer->numberOfAtoms;
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

   KIM_API_get_data_byI_multiple(pkim, ier, 6*3,
                                 buffer->atomTypes_ind,     &atomTypes,     1,
                                 buffer->coordinates_ind,   &coords,        1,
                                 buffer->boxlength_ind,     &boxlength,     (NBC==1),
                                 buffer->energy_ind,        &energy,        comp_energy,
                                 buffer->forces_ind,        &force,         comp_force,
                                 buffer->energyPerAtom_ind, &energyPerAtom, comp_energyPerAtom);
   if (KIM_STATUS_OK > *ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_byI_multiple", *ier);
      return;
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
            if (comp_process_d2Edr)
            {
               /* compute pair potential and its derivatives */
               calc_phi_d2phi(epsilon,
                             C,
                             Rzero,
                             shift,
                             cutoff, R, &phi, &dphi, &d2phi);

               /* compute dEidr */
               if ((1 == HalfOrFull) && (j < numberContrib))
               {
                  /* Half mode -- double contribution */
                  dEidr = dphi;
                  d2Eidr = d2phi;
               }
               else
               {
                  /* Full mode -- regular contribution */
                  dEidr = 0.5*dphi;
                  d2Eidr = 0.5*d2phi;
               }
            }
            else if (comp_force || comp_process_d1Edr)
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
            
            /* contribution to process_d2Edr */
            if (comp_process_d2Edr)
            {
               R_pairs[0] = R_pairs[1] = R;
               Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];
               Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];
               Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];
               i_pairs[0] = i_pairs[1] = i;
               j_pairs[0] = j_pairs[1] = j;

               KIM_API_process_d2Edr(km, &d2Eidr, &pR_pairs, &pRij_pairs, &pi_pairs, &pj_pairs, ier);
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

   /* store pointer to functions in KIM object */
   KIM_API_set_data_multiple(pkim, &ier, 3*4,
                             "compute", 1, &compute, 1,
                             "reinit",  1, &reinit,  1,
                             "destroy", 1, &destroy, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data_multiple", ier);
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

   /* convert to appropriate units */
   cutoff *= KIM_API_convert_unit_from(pkim, "A", "eV", "e", "K", "fs",
                                             1.0,  0.0, 0.0, 0.0,  0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_unit_from", ier);
      exit(1);
   }
   epsilon *= KIM_API_convert_unit_from(pkim, "A", "eV", "e", "K", "fs",
                                              0.0,  1.0, 0.0, 0.0,  0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_unit_from", ier);
      exit(1);
   }

   C *= KIM_API_convert_unit_from(pkim, "A", "eV", "e", "K", "fs",
                                        -1.0,  0.0, 0.0, 0.0,  0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_unit_from", ier);
      exit(1);
   }
   Rzero *= KIM_API_convert_unit_from(pkim, "A", "eV", "e", "K", "fs",
                                            1.0,  0.0, 0.0, 0.0,  0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_unit_from", ier);
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

   /* allocate memory for parameters */
   model_Pcutoff = (double*) malloc(1*sizeof(double));
   if (NULL == model_Pcutoff)
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
   model_epsilon = (double*) malloc(1*sizeof(double));
   if (NULL == model_epsilon)
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
   model_Rzero = (double*) malloc(1*sizeof(double));
   if (NULL == model_Rzero)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   model_shift = (double*) malloc(1*sizeof(double));
   if (NULL == model_shift)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
   
   /* store parameters in KIM object */
   KIM_API_set_data_multiple(pkim, &ier, 6*4,
                             "PARAM_FREE_cutoff",  1, model_Pcutoff, 1,
                             "PARAM_FIXED_cutsq",  1, model_cutsq,   1,
                             "PARAM_FREE_epsilon", 1, model_epsilon, 1,
                             "PARAM_FREE_C",       1, model_C,       1,
                             "PARAM_FREE_Rzero",   1, model_Rzero,   1,
                             "PARAM_FIXED_shift",  1, model_shift,   1
                            );

   /* set value of parameters */
   *model_Pcutoff = *model_cutoff;
   *model_cutsq = (*model_cutoff)*(*model_cutoff);
   *model_epsilon = epsilon;
   *model_C = C;
   *model_Rzero = Rzero;
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
   if (NULL == buffer)
   {
      KIM_API_report_error(__LINE__, __FILE__, "malloc", KIM_STATUS_FAIL);
      exit(1);
   }
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

   /* free parameters */
   free(buffer->Pcutoff);
   free(buffer->cutsq);
   free(buffer->epsilon);
   free(buffer->C);
   free(buffer->Rzero);
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

   buffer->model_index_shift = KIM_API_get_model_index_shift(pkim);

   KIM_API_get_index_multiple(pkim, &ier, 8*3,
                              "energy",        &(buffer->energy_ind),        1,
                              "forces",        &(buffer->forces_ind),        1,
                              "energyPerAtom", &(buffer->energyPerAtom_ind), 1,
                              "process_d1Edr", &(buffer->process_d1Edr_ind), 1,
                              "process_d2Edr", &(buffer->process_d2Edr_ind), 1,
                              "atomTypes",     &(buffer->atomTypes_ind),     1,
                              "coordinates",   &(buffer->coordinates_ind),   1,
                              "boxlength",     &(buffer->boxlength_ind),     1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_index_multiple", ier);
      exit(1);
   }

   KIM_API_get_data_multiple(pkim, &ier, 11*3,
                             "numberOfAtoms",           &(buffer->numberOfAtoms),           1,
                             "numberContributingAtoms", &(buffer->numberContributingAtoms), 1,
                             "get_half_neigh",          &(buffer->get_half_neigh),          1,
                             "get_full_neigh",          &(buffer->get_full_neigh),          1,
                             "cutoff",                  &(buffer->cutoff),                  1,
                             "PARAM_FREE_cutoff",       &(buffer->Pcutoff),                 1,
                             "PARAM_FIXED_cutsq",       &(buffer->cutsq),                   1,
                             "PARAM_FREE_epsilon",      &(buffer->epsilon),                 1,
                             "PARAM_FREE_C",            &(buffer->C),                       1,
                             "PARAM_FREE_Rzero",        &(buffer->Rzero),                   1,
                             "PARAM_FIXED_shift",       &(buffer->shift),                   1
                            );
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_multiple", ier);
      exit(1);
   }

   return;
}
