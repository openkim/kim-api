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
* Copyright (c) 2013, Regents of the University of Minnesota.
* All rights reserved.
*
* Contributors:
*    Valeriu Smirichinski
*    Ryan S. Elliott
*    Stephen M. Whalen
*
*/

/*******************************************************************************
*
*  model_Ne_P_fastLJ
*
*  Modified Lennard-Jones pair potential model for neon
*  (modified to have smooth cutoff)
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

#define DIM 3
#define Ar  1

/* Define prototypes for model init */
int model_init(void* km);

/* Define prototypes for model reinit, compute, and destroy */
static int reinit(void* km);
static int destroy(void* km);
static int neigh_rvec_h_compute(void* km);
static int neigh_pure_h_compute(void* km);
static int neigh_rvec_f_compute(void* km);
static int neigh_pure_f_compute(void* km);
static int mi_opbc_h_compute(void* km);
static int mi_opbc_f_compute(void* km);
static int cluster_compute(void* km);


/* Define model_buffer structure */
struct model_buffer {
   int energy_ind;
   int forces_ind;
   int particleEnergy_ind;
   int process_dEdr_ind;
   int model_index_shift;
   int numberOfParticles_ind;
   int coordinates_ind;
   int numberContributingParticles_ind;
   int boxSideLengths_ind;
   int cutoff_ind;
   int get_neigh_ind;


   double Pcutoff;
   double cutsq;
   double epsilon;
   double sigma;
   double shift;
};



/******************************************************************************/
/* neigh_rvec_h_compute function */
static int neigh_rvec_h_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
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
   int comp_process_dEdr;
   struct model_buffer* buffer;
   int model_index_shift;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   int* neighListOfCurrentAtom;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);

   int *numberContrib;
   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;
   int zero = 0, one = 1;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 8*3,
                              buffer->cutoff_ind,                      &cutoff,         1,
                              buffer->coordinates_ind,                 &coords,         1,
                              buffer->numberOfParticles_ind,           &nAtoms,         1,
                              buffer->numberContributingParticles_ind, &numberContrib,  1,
                              buffer->get_neigh_ind,                   &get_neigh,      1,
                              buffer->energy_ind,                      &energy,         comp_energy,
                              buffer->forces_ind,                      &force,          comp_force,
                              buffer->particleEnergy_ind,              &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   /* reset neighbor iterator */
   ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
      ier = KIM_STATUS_FAIL;
      return ier;
   }

   /* Compute energy and forces */
   ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = Rij[DIM*jj];
            dx[1] = Rij[DIM*jj+1];
            dx[2] = Rij[DIM*jj+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;
               if (j>= *numberContrib) fac *=0.5;

               *energy += ((j < *numberContrib) ? phi : 0.5*phi);

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }
   else
   {
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

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = Rij[DIM*jj];
            dx[1] = Rij[DIM*jj+1];
            dx[2] = Rij[DIM*jj+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
                  if (j>= *numberContrib) fac *=0.5;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[currentAtom] += 0.5*phi;
                  if (j < *numberContrib) particleEnergy[j] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += ((j < *numberContrib) ? phi : 0.5*phi);
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/******************************************************************************/
/* neigh_pure_h_compute function */
static int neigh_pure_h_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
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
   int comp_process_dEdr;
   struct model_buffer* buffer;
   int model_index_shift;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   int* neighListOfCurrentAtom;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);

   int *numberContrib;
   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;
   int zero = 0, one = 1;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 8*3,
                              buffer->cutoff_ind,                      &cutoff,         1,
                              buffer->numberOfParticles_ind,           &nAtoms,         1,
                              buffer->numberContributingParticles_ind, &numberContrib,  1,
                              buffer->get_neigh_ind,                   &get_neigh,      1,
                              buffer->coordinates_ind,                 &coords,         1,
                              buffer->energy_ind,                      &energy,         comp_energy,
                              buffer->forces_ind,                      &force,          comp_force,
                              buffer->particleEnergy_ind,              &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   /* reset neighbor iterator */
   ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
      ier = KIM_STATUS_FAIL;
      return ier;
   }

   /* Compute energy and forces */
   ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = coords[zj] - coords[zi];
            dx[1] = coords[zj+1] - coords[zi+1];
            dx[2] = coords[zj+2] - coords[zi+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;
               if (j>= *numberContrib) fac *=0.5;

               *energy += ((j < *numberContrib) ? phi : 0.5*phi);

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }
   else
   {
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

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = coords[zj] - coords[zi];
            dx[1] = coords[zj+1] - coords[zi+1];
            dx[2] = coords[zj+2] - coords[zi+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
                  if (j>= *numberContrib) fac *=0.5;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[currentAtom] += 0.5*phi;
                  if (j < *numberContrib) particleEnergy[j] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += ((j < *numberContrib) ? phi : 0.5*phi);
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/******************************************************************************/
/* neigh_rvec_f_compute function */
static int neigh_rvec_f_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
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
   int comp_process_dEdr;
   struct model_buffer* buffer;
   int model_index_shift;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   int* neighListOfCurrentAtom;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);

   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;
   int zero = 0, one = 1;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 7*3,
                              buffer->cutoff_ind,            &cutoff,         1,
                              buffer->coordinates_ind,       &coords,         1,
                              buffer->numberOfParticles_ind, &nAtoms,         1,
                              buffer->get_neigh_ind,         &get_neigh,      1,
                              buffer->energy_ind,            &energy,         comp_energy,
                              buffer->forces_ind,            &force,          comp_force,
                              buffer->particleEnergy_ind,    &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   /* reset neighbor iterator */
   ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
      ier = KIM_STATUS_FAIL;
      return ier;
   }

   /* Compute energy and forces */
   ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = Rij[DIM*jj];
            dx[1] = Rij[DIM*jj+1];
            dx[2] = Rij[DIM*jj+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 3.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;

               *energy += 0.5*phi;

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }
   else
   {
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

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = Rij[DIM*jj];
            dx[1] = Rij[DIM*jj+1];
            dx[2] = Rij[DIM*jj+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 3.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[currentAtom] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += 0.5*phi;
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/******************************************************************************/
/* neigh_pure_f_compute function */
static int neigh_pure_f_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
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
   int comp_process_dEdr;
   struct model_buffer* buffer;
   int model_index_shift;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   int* neighListOfCurrentAtom;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);

   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;
   int zero = 0, one = 1;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 7*3,
                              buffer->cutoff_ind,            &cutoff,         1,
                              buffer->numberOfParticles_ind, &nAtoms,         1,
                              buffer->coordinates_ind,       &coords,         1,
                              buffer->get_neigh_ind,         &get_neigh,      1,
                              buffer->energy_ind,            &energy,         comp_energy,
                              buffer->forces_ind,            &force,          comp_force,
                              buffer->particleEnergy_ind,    &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   /* reset neighbor iterator */
   ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
      ier = KIM_STATUS_FAIL;
      return ier;
   }

   /* Compute energy and forces */
   ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = coords[zj] - coords[zi];
            dx[1] = coords[zj+1] - coords[zi+1];
            dx[2] = coords[zj+2] - coords[zi+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 3.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;

               *energy += 0.5*phi;

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }
   else
   {
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

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute square distance */
            dx[0] = coords[zj] - coords[zi];
            dx[1] = coords[zj+1] - coords[zi+1];
            dx[2] = coords[zj+2] - coords[zi+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 3.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[currentAtom] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += 0.5*phi;
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/******************************************************************************/
/* mi_opbc_h_compute function */
static int mi_opbc_h_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
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
   int comp_process_dEdr;
   struct model_buffer* buffer;
   int model_index_shift;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   double* boxSideLengths;
   int* neighListOfCurrentAtom;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);

   int *numberContrib;
   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;
   int zero = 0, one = 1;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 9*3,
                              buffer->cutoff_ind,                      &cutoff,         1,
                              buffer->coordinates_ind,                 &coords,         1,
                              buffer->numberOfParticles_ind,           &nAtoms,         1,
                              buffer->numberContributingParticles_ind, &numberContrib,  1,
                              buffer->boxSideLengths_ind,              &boxSideLengths, 1,
                              buffer->get_neigh_ind,                   &get_neigh,      1,
                              buffer->energy_ind,                      &energy,         comp_energy,
                              buffer->forces_ind,                      &force,          comp_force,
                              buffer->particleEnergy_ind,              &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   /* reset neighbor iterator */
   ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
      ier = KIM_STATUS_FAIL;
      return ier;
   }

   /* Compute energy and forces */
   ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute neighbor vector and square distance */
            Rsqij = 0.0;
            for (k = 0; k < DIM; k++)
            {
               dx[k] = coords[zj+k] - coords[zi+k];
               if (abs(dx[k]) > 0.5*boxSideLengths[k])
               {
                  dx[k] -= (dx[k]/fabs(dx[k]))*boxSideLengths[k];
               }
               Rsqij += dx[k]*dx[k];
            }

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;
               if (j>= *numberContrib) fac *=0.5;

               *energy += ((j < *numberContrib) ? phi : 0.5*phi);

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }
   else
   {
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

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute neighbor vector and square distance */
            Rsqij = 0.0;
            for (k = 0; k < DIM; k++)
            {
               dx[k] = coords[zj+k] - coords[zi+k];
               if (abs(dx[k]) > 0.5*boxSideLengths[k])
               {
                  dx[k] -= (dx[k]/fabs(dx[k]))*boxSideLengths[k];
               }
               Rsqij += dx[k]*dx[k];
            }

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
                  if (j>= *numberContrib) fac *=0.5;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[currentAtom] += 0.5*phi;
                  if (j < *numberContrib) particleEnergy[j] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += ((j < *numberContrib) ? phi : 0.5*phi);
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/******************************************************************************/
/* mi_opbc_f_compute function */
static int mi_opbc_f_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
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
   int comp_process_dEdr;
   struct model_buffer* buffer;
   int model_index_shift;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* Rij;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;
   double* boxSideLengths;
   int* neighListOfCurrentAtom;
   int (*get_neigh)(void *,int *,int *,int *, int *, int **, double **);

   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;
   int zero = 0, one = 1;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack info from the buffer */
   model_index_shift = buffer->model_index_shift;
   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 8*3,
                              buffer->cutoff_ind,            &cutoff,         1,
                              buffer->numberOfParticles_ind, &nAtoms,         1,
                              buffer->coordinates_ind,       &coords,         1,
                              buffer->boxSideLengths_ind,    &boxSideLengths, 1,
                              buffer->get_neigh_ind,         &get_neigh,      1,
                              buffer->energy_ind,            &energy,         comp_energy,
                              buffer->forces_ind,            &force,          comp_force,
                              buffer->particleEnergy_ind,    &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   /* reset neighbor iterator */
   ier = (*get_neigh)(&pkim, &zero, &zero, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
   if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
      ier = KIM_STATUS_FAIL;
      return ier;
   }

   /* Compute energy and forces */
   ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute neighbor vector and square distance */
            Rsqij = 0.0;
            for (k = 0; k < DIM; k++)
            {
               dx[k] = coords[zj+k] - coords[zi+k];
               if (abs(dx[k]) > 0.5*boxSideLengths[k])
               {
                  dx[k] -= (dx[k]/fabs(dx[k]))*boxSideLengths[k];
               }
               Rsqij += dx[k]*dx[k];
            }

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 3.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;

               *energy += 0.5*phi;

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }
   else
   {
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

      while (KIM_STATUS_OK == ier)
      {
         i = currentAtom + model_index_shift;;
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (jj = 0; jj < numOfAtomNeigh; ++ jj)
         {
            j = neighListOfCurrentAtom[jj] + model_index_shift;
            z = jj*DIM;
            zj = j*DIM;

            /* compute neighbor vector and square distance */
            Rsqij = 0.0;
            for (k = 0; k < DIM; k++)
            {
               dx[k] = coords[zj+k] - coords[zi+k];
               if (abs(dx[k]) > 0.5*boxSideLengths[k])
               {
                  dx[k] -= (dx[k]/fabs(dx[k]))*boxSideLengths[k];
               }
               Rsqij += dx[k]*dx[k];
            }

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 3.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[currentAtom] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += 0.5*phi;
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }

         /* increment iterator */
         ier = (*get_neigh)(&pkim, &zero, &one, &currentAtom, &numOfAtomNeigh, &neighListOfCurrentAtom, &Rij);
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}

/******************************************************************************/
/* cluster_compute function */
static int cluster_compute(void* km)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double R;
   double Rsqij;
   double phi;
   double dEidr;
   int ier;
   int i;
   int j;
   int k;
   int comp_energy;
   int comp_force;
   int comp_particleEnergy;
   int comp_process_dEdr;
   struct model_buffer* buffer;

   int* nAtoms;
   double* cutoff;
   double* epsilon;
   double* sigma;
   double* cutsq;
   double* shift;
   double* coords;
   double* energy;
   double* force;
   double* particleEnergy;

   double dx[3];
   double* pdx = &(dx[0]);
   double four_eps_sigma6;
   double four_eps_sigma12;
   double Rm2;
   double R6;
   double R12;
   double e_Rm12;
   double e_Rm6;
   int zi, zj;
   double fac = 0.0;
   double DE;

   /* get buffer from KIM object */
   buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);
      return ier;
   }

   /* unpack the Model's parameters stored in the KIM API object */
   cutsq = &(buffer->cutsq);
   epsilon = &(buffer->epsilon);
   sigma = &(buffer->sigma);
   shift = &(buffer->shift);

   /* check to see if we have been asked to compute the forces, particleEnergy, and d1Edr */
   KIM_API_getm_compute_by_index(pkim, &ier, 4*3,
                                 buffer->energy_ind,         &comp_energy,         1,
                                 buffer->forces_ind,         &comp_force,          1,
                                 buffer->particleEnergy_ind, &comp_particleEnergy, 1,
                                 buffer->process_dEdr_ind,   &comp_process_dEdr,   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute_by_index", ier);
      return ier;
   }

   /* unpack data from KIM object */
   KIM_API_getm_data_by_index(pkim, &ier, 6*3,
                              buffer->cutoff_ind,            &cutoff,         1,
                              buffer->numberOfParticles_ind, &nAtoms,         1,
                              buffer->coordinates_ind,       &coords,         1,
                              buffer->energy_ind,            &energy,         comp_energy,
                              buffer->forces_ind,            &force,          comp_force,
                              buffer->particleEnergy_ind,    &particleEnergy, comp_particleEnergy);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);
      return ier;
   }

   /* compute intermediate values */
   four_eps_sigma6 = 4.0*(*epsilon)*pow(*sigma,6.0);
   four_eps_sigma12 = 4.0*(*epsilon)*pow(*sigma,12.0);

   /* Assume the atom types are correct! */

   if ((comp_force == 1)          && (comp_energy == 1) &&
       (comp_particleEnergy == 0) && (comp_process_dEdr == 0))
   {
      /* initialize potential energies, forces, and virial term */
      *energy = 0.0;
      for (i = 0; i < *nAtoms; ++i)
      {
         for (k = 0; k < DIM; ++k)
         {
            force[i*DIM + k] = 0.0;
         }
      }

      for (i = 0; i < *nAtoms; i++)
      {
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (j = i+1; j < *nAtoms; ++ j)
         {
            zj = j*DIM;

            /* compute square distance */
            dx[0] = coords[zj] - coords[zi];
            dx[1] = coords[zj+1] - coords[zi+1];
            dx[2] = coords[zj+2] - coords[zi+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;

               dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
               fac=dEidr*Rm2;

               *energy += phi;

               force[zi] += fac*dx[0];
               force[zi+1] += fac*dx[1];
               force[zi+2] += fac*dx[2];
               force[zj] -= fac*dx[0];
               force[zj+1] -= fac*dx[1];
               force[zj+2] -= fac*dx[2];
            }
         }
      }
   }
   else
   {
      comp_particleEnergy = 0;

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

      for (i = 0; i < *nAtoms; i++)
      {
         zi=i*DIM;

         /* loop over the neighbors of currentAtom */
         for (j = i+1; j < *nAtoms; j++)
         {
            zj = j*DIM;

            /* compute square distance */
            dx[0] = coords[zj] - coords[zi];
            dx[1] = coords[zj+1] - coords[zi+1];
            dx[2] = coords[zj+2] - coords[zi+2];
            Rsqij = dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2];

            /* particles are interacting ? */
            if (Rsqij < *cutsq)
            {
               /* compute pair potential */
               Rm2 = 1.0/Rsqij;
               R6 = Rm2*Rm2*Rm2;
               R12= R6*R6;

               e_Rm12 = four_eps_sigma12*R12;
               e_Rm6 = four_eps_sigma6*R6;
               phi = e_Rm12 - e_Rm6 + *shift;
               if (comp_force==1)
               {
                  dEidr = 6.0*(-2.0*e_Rm12 + e_Rm6 );
                  fac=dEidr*Rm2;
               }

               /* accumulate energy */
               if (comp_particleEnergy)
               {
                  particleEnergy[i] += 0.5*phi;
                  particleEnergy[j] += 0.5*phi;
               }

               if (comp_energy)
               {
                  *energy += phi;
               }

               /* process dEdr */
               if (comp_process_dEdr)
               {
                  R = sqrt(Rsqij);
                  DE = fac*R;
                  ier = KIM_API_process_dEdr(km, &DE, &R, &pdx, &i, &j);
               }

               /* accumulate force */
               if (comp_force==1)
               {
                  force[zi] += fac*dx[0];
                  force[zi+1] += fac*dx[1];
                  force[zi+2] += fac*dx[2];
                  force[zj] -= fac*dx[0];
                  force[zj+1] -= fac*dx[1];
                  force[zj+2] -= fac*dx[2];
               }
            }
         }
      }
   }

   /* everything is great */
   ier = KIM_STATUS_OK;
   return ier;
}


/******************************************************************************/
/* Reinitialization function */
static int reinit(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   int ier;
   double *cutoff;
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
   cutoff = (double *) KIM_API_get_data_by_index(pkim, buffer->cutoff_ind, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_by_index", ier);
      return ier;
   }
   *cutoff = buffer->Pcutoff;


   /* set value of parameter cutsq and shift */
   buffer->cutsq = (*cutoff)*(*cutoff);
   buffer->shift = -4.0*(buffer->epsilon)*(pow((buffer->sigma/(*cutoff)),12.0)
                                         - pow((buffer->sigma/(*cutoff)),6.0));

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


/******************************************************************************/
/* Initialization function */
int model_init(void *km)
{
   /* Local variables */
   intptr_t* pkim = *((intptr_t**) km);
   double* cutoff;
   int ier;
   struct model_buffer* buffer;
   char* NBCstr;

   /* store function pointers in KIM object */
   KIM_API_setm_method_data(pkim, &ier, 2*4,
                             "reinit",  1, (func_ptr) &reinit,  1,
                             "destroy", 1, (func_ptr) &destroy, 1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_method_data", ier);
      return ier;
   }

   /* store model cutoff in KIM object */
   cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);
      return ier;
   }
   *cutoff = 8.15; /* cutoff distance in angstroms */
   /* convert to appropriate units */
   *cutoff *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",
                                                1.0, 0.0,  0.0, 0.0, 0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_to_act_unit", ier);
      return ier;
   }

   /* allocate buffer */
   buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));
   if (NULL == buffer)
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);
      return ier;
   }

   /* setup buffer */
   buffer->sigma = 2.74; /* LJ sigma in angstroms */
   /* convert to appropriate units */
   buffer->sigma *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",
                                                      1.0, 0.0,  0.0, 0.0, 0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_to_act_unit", ier);
      return ier;
   }

   buffer->epsilon = 0.0031; /* LJ epsilon in eV */
   /* convert to appropriate units */
   buffer->epsilon *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",
                                                        0.0, 1.0,  0.0, 0.0, 0.0, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_convert_to_act_unit", ier);
      return ier;
   }

   /* already in correct units */
   buffer->Pcutoff = *cutoff;
   buffer->cutsq = (*cutoff)*(*cutoff);
   buffer->shift = -4.0*(buffer->epsilon)*(pow((buffer->sigma/(*cutoff)),12.0)
                                         - pow((buffer->sigma/(*cutoff)),6.0));

   /* Determine neighbor list boundary condition (NBC) */
   NBCstr = KIM_API_get_NBC_method(pkim, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);
      return ier;
   }
   if (!strcmp("NEIGH_RVEC_H",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &neigh_rvec_h_compute);
   }
   else if (!strcmp("NEIGH_PURE_H",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &neigh_pure_h_compute);
   }
   else if (!strcmp("NEIGH_RVEC_F",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &neigh_rvec_f_compute);
   }
   else if (!strcmp("NEIGH_PURE_F",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &neigh_pure_f_compute);
   }
   else if (!strcmp("MI_OPBC_H",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &mi_opbc_h_compute);
   }
   else if (!strcmp("MI_OPBC_F",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &mi_opbc_f_compute);
   }
   else if (!strcmp("CLUSTER",NBCstr))
   {
      ier = KIM_API_set_method_data(pkim, "compute", 1,
                                    (func_ptr) &cluster_compute);
   }
   else
   {
      ier = KIM_STATUS_FAIL;
      KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);
      return ier;
   }
   free(NBCstr); /* don't forget to release the memory... */

   buffer->model_index_shift = KIM_API_get_model_index_shift(pkim);

   KIM_API_getm_index(pkim, &ier, 10*3,
                      "energy",                      &(buffer->energy_ind),                      1,
                      "forces",                      &(buffer->forces_ind),                      1,
                      "particleEnergy",              &(buffer->particleEnergy_ind),              1,
                      "process_dEdr",                &(buffer->process_dEdr_ind),                1,
                      "numberOfParticles",           &(buffer->numberOfParticles_ind),           1,
                      "coordinates",                 &(buffer->coordinates_ind),                 1,
                      "numberContributingParticles", &(buffer->numberContributingParticles_ind), 1,
                      "boxSideLengths",              &(buffer->boxSideLengths_ind),              1,
                      "cutoff",                      &(buffer->cutoff_ind),                      1,
                      "get_neigh",                   &(buffer->get_neigh_ind),                   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_index", ier);
      return ier;
   }
   /* end buffer setup */

   /* store in model buffer */
   KIM_API_set_model_buffer(pkim, (void*) buffer, &ier);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_model_buffer", ier);
      return ier;
   }

   /* store parameters in KIM object */
   KIM_API_setm_data(pkim, &ier, 5*4,
                     "PARAM_FREE_sigma",    1, &(buffer->sigma),   1,
                     "PARAM_FREE_epsilon",  1, &(buffer->epsilon), 1,
                     "PARAM_FREE_cutoff",   1, &(buffer->Pcutoff), 1,
                     "PARAM_FIXED_cutsq",   1, &(buffer->cutsq),   1,
                     "PARAM_FIXED_shift",   1, &(buffer->shift),   1);
   if (KIM_STATUS_OK > ier)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);
      return ier;
   }

   ier = KIM_STATUS_OK;
   return ier;
}
