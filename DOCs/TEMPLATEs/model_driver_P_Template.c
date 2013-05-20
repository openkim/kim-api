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
* Copyright (c) 2012, Regents of the University of Minnesota.  All rights reserved.
*
* Contributors:
*    Ryan S. Elliott
*    Ellad B. Tadmor
*    Valeriu Smirichinski
*
*/

/*******************************************************************************
*
*  model_driver_P_<FILL model driver name>
*
*  <FILL model drive name> pair potential KIM Model Driver
*
*  Reference: <FILL>
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


/* Define prototypes for Model Driver init */
/**/
int model_driver_init(void* km, char* paramfile_names, int* nmstrlen, int* numparamfiles);

/* Define prototypes for Model (Driver) reinit, compute, and destroy */
/* defined as static to avoid namespace clashes with other Models    */
/**/
static int reinit(void* km);
static int destroy(void* km);
static int compute(void* km);
/**/
static void calc_phi(double* <FILL parameter 1>,
                     double* <FILL parameter 2>,
                     /* FILL as many parameters as needed */
                     double* cutoff, double r, double* phi);
static void calc_phi_dphi(double* <FILL parameter 1>,
                          double* <FILL parameter 2>,
                          /* FILL as many parameters as needed */
                          double* cutoff, double r, double* phi, double* dphi);

static void calc_phi_d2phi(double* <FILL parameter 1>,
                           double* <FILL parameter 2>,
                           /* FILL as many parameters as needed */
                           double* cutoff, double r, double* phi, double* dphi, double* d2phi);

/* Define model_buffer structure */
struct model_buffer {
   int NBC;
   int HalfOrFull;
   int IterOrLoca;
   int energy_ind;
   int forces_ind;
   int particleEnergy_ind;
   int process_dEdr_ind;
   int process_d2Edr2_ind;
   int model_index_shift;
   int numberOfParticles_ind;
   int particleTypes_ind;
   int coordinates_ind;
   int numberContributingParticles_ind;
   int boxSideLengths_ind;
   int get_neigh_ind;
   int cutoff_ind;


   double Pcutoff;
   double cutsq;
   double <FILL parameter 1>;
   double <FILL parameter 2>;
   /* FILL as many parameters as needed */
};


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

/* Calculate pair potential phi(r) and its 1st & 2nd derivatives dphi(r), d2phi(r) */
static void calc_phi_d2phi(double* <FILL parameter 1>,
                           double* <FILL parameter 2>,
                           /* FILL as many parameters as needed */
                           double* cutoff, double r, double* phi, double* dphi, double* d2phi)
{
   /* local variables */
   /* FILL: place any local variable definitions here */

   if (r > *cutoff)
   {
      /* Argument exceeds cutoff radius */
      *phi   = 0.0;
      *dphi  = 0.0;
      *d2phi = 0.0;
   }
   else
   {
      *phi   = <FILL functional form of phi(r)>;
      *dphi  = <FILL functional form of dphi(r)>;
      *d2phi = <FILL functional form of d2phi(r)>;
   }

   return;
}

/* compute function */
static int compute(void* km)
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
   int ier;
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
   int comp_particleEnergy;
   int comp_process_dEdr;
   int comp_process_d2Edr2;
   int NBC;
   int HalfOrFull;
   int IterOrLoca;
   int model_index_shift;
   int zero = 0;
   int one = 1;
   int request;

   int* nAtoms;
   int* particleTypes;
   double* cutoff;
   double* cutsq;
   double* <FILL parameter 1>;
   double* <FILL parameter 2>;
   /* FILL as many parameters as needed */
   double* Rij_list;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   double* boxSideLengths;
   int* numContrib;
   int numberContrib;
   int numOfAtomNeigh;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);


   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   NBC = buffer->NBC;
   HalfOrFull = buffer->HalfOrFull;
   IterOrLoca = buffer->IterOrLoca;
   model_index_shift = buffer->model_index_shift;
   cutsq = &(buffer->cutsq);
   <FILL parameter 1> = &(buffer-><FILL parameter 1>);
   <FILL parameter 2> = &(buffer-><FILL parameter 2>);
   /* also FILL additional parameters here if there are any ... */

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 5*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1,
                                 buffer->process_d2Edr2_ind, &comp_process_d2Edr2, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   KIM_API_getm_data_by_index(pkim, &ier, 10*3,
                              buffer->cutoff_ind,                      &cutoff,         1,
                              buffer->numberOfParticles_ind,           &nAtoms,         1,
                              buffer->particleTypes_ind,               &particleTypes,  1,
                              buffer->coordinates_ind,                 &coords,         1,
                              buffer->numberContributingParticles_ind, &numContrib,     (HalfOrFull==1),
                              buffer->get_neigh_ind,                   &get_neigh,      (NBC!=0),
                              buffer->boxSideLengths_ind,              &boxSideLengths, (NBC==1),
                              buffer->energy_ind,                      &energy,         comp_energy,
                              buffer->forces_ind,                      &force,          comp_force,
                              buffer->particleEnergy_ind,              &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   if (HalfOrFull == 1)
   {
      if (0 != NBC) /* non-CLUSTER cases */
      {
         numberContrib = *numContrib;
      }
      else /* CLUSTER cases */
      {
         numberContrib = *nAtoms;
      }
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

   /* Initialize neighbor handling for CLUSTER NBC */
   if (0 == NBC) /* CLUSTER */
   {
      neighListOfCurrentAtom = (int *) malloc((*nAtoms)*sizeof(int));
   }

   /* Initialize neighbor handling for Iterator mode */

   if (1 == IterOrLoca)
   {
      ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh,
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
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh,
                             &neighListOfCurrentAtom, &Rij_list);
         if (KIM_STATUS_NEIGH_ITER_PAST_END == ier) /* the end of the list, terminate loop */
         {
            break;
         }
         if (KIM_STATUS_OK > ier) /* some sort of problem, exit */
         {
            KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
            return ier;
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

         if (0 == NBC) /* CLUSTER NBC method */
         {
            numOfAtomNeigh = *nAtoms - (i + 1);
            for (k = 0; k < numOfAtomNeigh; ++k)
            {
               neighListOfCurrentAtom[k] = i + k + 1 - model_index_shift;
            }
            ier = KIM_STATUS_OK;
         }
         else
         {
            request = i - model_index_shift;
            ier = (*get_neigh)(&pkim, &one, &request,
                                &currentAtom, &numOfAtomNeigh,
                                &neighListOfCurrentAtom, &Rij_list);
            if (KIM_STATUS_OK != ier) /* some sort of problem, exit */
            {
               KIM_API_report_error(__LINE__, __FILE__, "get_neigh", ier);
               ier = KIM_STATUS_FAIL;
               return ier;
            }
         }
      }

      /* loop over the neighbors of atom i */
      for (jj = 0; jj < numOfAtomNeigh; ++ jj)
      {

         j = neighListOfCurrentAtom[jj] + model_index_shift; /* get neighbor ID */

         /* compute relative position vector and squared distance */
         Rsqij = 0.0;
         for (k = 0; k < DIM; ++k)
         {
            if (3 != NBC) /* all methods except NEIGH_RVEC_F */
            {
               Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];
            }
            else          /* NEIGH_RVEC_F method */
            {
               Rij[k] = Rij_list[jj*DIM + k];
            }

            /* apply periodic boundary conditions if required */
            if (1 == NBC)
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
         if (Rsqij < *cutsq) /* particles are interacting ? */
         {
            R = sqrt(Rsqij);
            if (comp_process_d2Edr2)
            {
               /* compute pair potential and its derivatives */
               calc_phi_d2phi(<FILL parameter 1>,
                              <FILL parameter 2>,
                              /* FILL as many parameters as needed */
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
            else if (comp_force || comp_process_dEdr)
            {
               /* compute pair potential and its derivative */
               calc_phi_dphi(<FILL parameter 1>,
                             <FILL parameter 2>,
                             /* FILL as many parameters as needed */
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
               calc_phi(<FILL parameter 1>,
                        <FILL parameter 2>,
                        /* FILL as many parameters as needed */
                        cutoff, R, &phi);
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

            /* contribution to process_dEdr */
            if (comp_process_dEdr)
            {
               ier = KIM_API_process_dEdr(km, &dEidr, &R, &pRij, &i, &j);
            }

            /* contribution to process_d2Edr2 */
            if (comp_process_d2Edr2)
            {
               R_pairs[0] = R_pairs[1] = R;
               Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];
               Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];
               Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];
               i_pairs[0] = i_pairs[1] = i;
               j_pairs[0] = j_pairs[1] = j;

               ier = KIM_API_process_d2Edr2(km, &d2Eidr, &pR_pairs, &pRij_pairs, &pi_pairs, &pj_pairs);
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
   if (0 == NBC)
   {
      free(neighListOfCurrentAtom);
   }

   /* everything is great */
   ier = KIM_STATUS_OK;

   return ier;
}

/* Initialization function */
int model_driver_init(void *km, char* paramfile_names, int* nmstrlen, int* numparamfiles)
{
   /* KIM variables */
   intptr_t* pkim = *((intptr_t**) km);
   char* paramfile1name;
   char* paramfile2name;
   <FILL as many file name pointers as needed>

   /* Local variables */
   FILE* fid;
   double cutoff;
   double <FILL parameter 1>;
   double <FILL parameter 2>;
   /* FILL as many parameters as needed */
   double* model_cutoff;
   int ier;
   double dummy;
   struct model_buffer* buffer;
   char* NBCstr;

   /* set paramfile1name */
   if (*numparamfiles != <FILL number of parameter files>)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Incorrect number of parameter files.", ier);
      return ier;
   }
   paramfile1name = paramfile_names;
   paramfile2name = &(paramfile_names[1*(*nmstrlen)]);
   <FILL as many file name pointers as needed>

   /* store pointer to functions in KIM object */
   KIM_API_setm_data(pkim, &ier, 3*4,
                     "compute", 1, &compute, 1,
                     "reinit",  1, &reinit,  1,
                     "destroy", 1, &destroy, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);
      return ier;
   }

   /* Read in model parameters from parameter file */
   fid = fopen(paramfile1name, "r");
   if (fid == NULL)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Unable to open parameter file for <FILL model driver name> parameters", ier);
      return ier;
   }

   ier = fscanf(fid, "%lf \n%lf \n<FILL as many parameters as needed>",
                &cutoff,             /* cutoff distance in angstroms */
                &<FILL parameter 1>,
                &<FILL parameter 2>,
                /* FILL as many parameters as needed */
               );
   fclose(fid);

   /* check that we read the right number of parameters */
   if (<FILL number of parameters (including cutoff)> != ier)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Unable to read all <FILL model driver name> parameters", ier);
      return ier;
   }

   /* FILL process the remaining parameter files */

   /* convert to appropriate units */
   cutoff *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",
                                               1.0, 0.0,  0.0, 0.0, 0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_to_act_unit", ier);
      return ier;
   }

   <FILL parameter 1> *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",
                                                     <FILL exponents (5) for parameter 1>, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_to_act_unit", ier);
      return ier;
   }

   <FILL parameter 2> *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",
                                                     <FILL exponents (5) for parameter 2>, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_to_act_unit", ier);
      return ier;
   }

   /* FILL as many parameters as necessary */

   /* store model cutoff in KIM object */
   model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      return ier;
   }
   *model_cutoff = cutoff;

   /* allocate buffer */
   buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));
   if (NULL == buffer)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }

   /* setup buffer */
   /* set value of parameters */
   buffer->Pcutoff = *model_cutoff;
   buffer->cutsq = (*model_cutoff)*(*model_cutoff);
   buffer-><FILL parameter 1> = <FILL parameter 1>;
   buffer-><FILL parameter 2> = <FILL parameter 2>;
   /* FILL as many parameters as needed */

   /* Determine neighbor list boundary condition (NBC) */
   NBCstr = KIM_API_get_NBC_method(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);
      return ier;
   }
   if (!strcmp("CLUSTER",NBCstr))
   {
      buffer->NBC = 0;
   }
   else if ((!strcmp("MI_OPBC_H",NBCstr)) || (!strcmp("MI_OPBC_F",NBCstr)))
   {
      buffer->NBC = 1;
   }
   else if ((!strcmp("NEIGH_PURE_H",NBCstr)) || (!strcmp("NEIGH_PURE_F",NBCstr)))
   {
      buffer->NBC = 2;
   }
   else if (!strcmp("NEIGH_RVEC_F",NBCstr))
   {
      buffer->NBC = 3;
   }
   else
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);
      return ier;
   }
   free(NBCstr); /* don't forget to release the memory... */

   /* Determine if Half or Full neighbor lists are being used */
   /*****************************
    * HalfOrFull = 1 -- Half
    *            = 2 -- Full
    *****************************/
   if (KIM_API_is_half_neighbors(pkim, &ier))
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
         return ier;
      }
      if ((buffer->IterOrLoca != 1) && (buffer->IterOrLoca != 2))
      {
         ier = KIM_STATUS_FAIL;
         KIM_API_report_error(__LINE__, __FILE__, "Unsupported IterOrLoca mode", ier);
         return ier;
      }
   }
   else
   {
      buffer->IterOrLoca = 2;   /* for CLUSTER NBC */
   }

   buffer->model_index_shift = KIM_API_get_model_index_shift(pkim);

   KIM_API_getm_index(pkim, &ier, 12*3,
                      "cutoff",                      &(buffer->cutoff_ind),                      1,
                      "numberOfParticles",           &(buffer->numberOfParticles_ind),           1,
                      "particleTypes",               &(buffer->particleTypes_ind),               1,
                      "numberContributingParticles", &(buffer->numberContributingParticles_ind), 1,
                      "coordinates",                 &(buffer->coordinates_ind),                 1,
                      "get_neigh",                   &(buffer->get_neigh_ind),                   1,
                      "boxSideLengths",              &(buffer->boxSideLengths_ind),              1,
                      "energy",                      &(buffer->energy_ind),                      1,
                      "forces",                      &(buffer->forces_ind),                      1,
                      "particleEnergy",              &(buffer->particleEnergy_ind),              1,
                      "process_dEdr",                &(buffer->process_dEdr_ind),                1,
                      "process_d2Edr2",              &(buffer->process_d2Edr2_ind),              1
                     );
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_index", ier);
      return ier;
   }
   /* end setup buffer */

   /* store in model buffer */
   KIM_API_set_model_buffer(pkim, (void*) buffer, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_model_buffer", ier);
      return ier;
   }

   /* set pointers to parameters in KIM object */
   KIM_API_setm_data(pkim, &ier, 6*4,
                     "PARAM_FREE_cutoff",  1, &(buffer->Pcutoff), 1,
                     "PARAM_FIXED_cutsq",  1, &(buffer->cutsq),   1,
                     "PARAM_<FILL FREE or FIXED>_<FILL parameter 1>", 1, &(buffer-><FILL parameter 1>), 1,
                     "PARAM_<FILL FREE or FIXED>_<FILL parameter 2>", 1, &(buffer-><FILL parameter 2>), 1,
                     /* FILL as many parameters as needed */
                    );
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);
      return ier;
   }

   return KIM_STATUS_OK;
}

/* Reinitialization function */
static int reinit(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   int ier;
   double *cutoff;
   double dummy;
   struct model_buffer* buffer;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* set new values in KIM object     */
   /*                                  */
   /* store model cutoff in KIM object */
   cutoff = KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      return ier;
   }
   *cutoff = buffer->Pcutoff;

   /* set value of parameter cutsq */
   buffer->cutsq = (*cutoff)*(*cutoff);

   /* FILL: recompute any other FIXED parameters whose values depend on FREE parameters */

   ier = KIM_STATUS_OK;
   return ier;
}

/* destroy function */
static int destroy(void *km)
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
      return ier;
   }

   /* destroy the buffer */
   free(buffer);

   ier = KIM_STATUS_OK;
   return ier;
}
