/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.  If applicable, add the following below this CDDL HEADER,    */
/* with the fields enclosed by brackets "[]" replaced with your own           */
/* identifying information:                                                   */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2013--2017, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*    Ellad B. Tadmor                                                         */
/*    Stephen M. Whalen                                                       */
/*                                                                            */

/******************************************************************************/
/*                                                                            */
/* ex_model_driver_P_Morse pair potential KIM Model Driver                    */
/* shifted to have zero energy at the cutoff radius                           */
/*                                                                            */
/* Language: C                                                                */
/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */
/******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "KIM_Simulator.h"
#include "KIM_Compute.h"
#include "KIM_UTILITY_Compute.h"
#include "KIM_Logger.h"

#define TRUE 1
#define FALSE 0

/******************************************************************************/
/* Below are the definitions for some constants                               */
/******************************************************************************/
#define DIM 3  /* dimensionality of space */
#define SPECCODE 1  /* internal species code */


/* Define prototype for Model Driver init */
int model_driver_init(KIM_Simulator * const simulator,
                      char const * const parameterFileNames,
                      int const nameStringLength,
                      int const numberOfParameterFiles);

/* Define prototypes for destroy */
/* defined as static to avoid namespace clashes with other codes */
static int destroy(KIM_Simulator * const simulator);

/* Define prototype for compute routine */
static int compute(KIM_Simulator const * const simulator);

/* Define prototypes for pair potential calculations */
static void calc_phi(double const * epsilon,
                     double const * C,
                     double const * Rzero,
                     double const * shift,
                     double const cutoff, double const r, double* phi);

static void calc_phi_dphi(double const* epsilon,
                          double const* C,
                          double const* Rzero,
                          double const* shift,
                          double const cutoff, double const r,
                          double* phi, double* dphi);

/* Define model_buffer structure */
struct model_buffer
{
  double influenceDistance;
  double cutsq;
  double epsilon;
  double C;
  double Rzero;
  double shift;
};


/* Calculate pair potential phi(r) */
static void calc_phi(double const* epsilon,
                     double const* C,
                     double const* Rzero,
                     double const* shift,
                     double const cutoff, double const r, double* phi)
{
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C)*(r-*Rzero));
  ep2 = ep*ep;

  if (r > cutoff)
  {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
  }
  else
  {
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
  }

  return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double const* epsilon,
                          double const* C,
                          double const* Rzero,
                          double const* shift,
                          double const cutoff, double const r,
                          double* phi, double* dphi)
{
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C)*(r-*Rzero));
  ep2 = ep*ep;

  if (r > cutoff)
  {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
    *dphi = 0.0;
  }
  else
  {
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
    *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 );
  }

  return;
}

/* compute function */
static int compute(KIM_Simulator const * const simulator)
{
  /* local variables */
  double R;
  double Rsqij;
  double phi;
  double dphi;
  double dEidr;
  double Rij[DIM];
  int ier;
  int i;
  int j;
  int jj;
  int k;
  int const * neighListOfCurrentPart;
  struct model_buffer* buffer;
  int comp_energy;
  int comp_force;
  int comp_particleEnergy;

  int* nParts;
  int* particleSpecies;
  int* particleContributing;
  double cutoff;
  double* cutsq;
  double* epsilon;
  double* C;
  double* Rzero;
  double* shift;
  double* coords;
  double* energy;
  double* force;
  double* particleEnergy;
  int numOfPartNeigh;

  /* get buffer from KIM object */
  KIM_Simulator_get_model_buffer(simulator, (void **) &buffer);

  /* unpack info from the buffer */
  cutoff = buffer->influenceDistance;
  cutsq = &(buffer->cutsq);
  epsilon = &(buffer->epsilon);
  C = &(buffer->C);
  Rzero = &(buffer->Rzero);
  shift = &(buffer->shift);

  /* check to see if we have been asked to compute the forces, */
  /* particleEnergy, and d1Edr */
  ier = KIM_UTILITY_COMPUTE_getm_compute(
      simulator,
      KIM_COMPUTE_ARGUMENT_NAME_energy,         &comp_energy,         1,
      KIM_COMPUTE_ARGUMENT_NAME_forces,         &comp_force,          1,
      KIM_COMPUTE_ARGUMENT_NAME_particleEnergy, &comp_particleEnergy, 1,
      KIM_COMPUTE_ARGUMENT_NAME_End);
  if (ier)
  {
    KIM_report_error(__LINE__, __FILE__, "KIM_getm_compute", ier);
    return ier;
  }

  ier = KIM_UTILITY_COMPUTE_getm_data(
      simulator,
      KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles, &nParts,         1,
      KIM_COMPUTE_ARGUMENT_NAME_particleSpecies,   &particleSpecies,1,
      KIM_COMPUTE_ARGUMENT_NAME_particleContributing, &particleContributing,1,
      KIM_COMPUTE_ARGUMENT_NAME_coordinates,       &coords,         1,
      KIM_COMPUTE_ARGUMENT_NAME_energy,            &energy,         comp_energy,
      KIM_COMPUTE_ARGUMENT_NAME_forces,            &force,          comp_force,
      KIM_COMPUTE_ARGUMENT_NAME_particleEnergy,    &particleEnergy, comp_particleEnergy,
      KIM_COMPUTE_ARGUMENT_NAME_End);
  if (ier)
  {
    KIM_report_error(__LINE__, __FILE__, "KIM_getm_data", ier);
    return ier;
  }

  /* Check to be sure that the species are correct */
  /**/
  ier = TRUE; /* assume an error */
  for (i = 0; i < *nParts; ++i)
  {
    if ( SPECCODE != particleSpecies[i])
    {
      KIM_report_error(__LINE__, __FILE__,
                       "Unexpected species detected", ier);
      return ier;
    }
  }
  ier = FALSE;  /* everything is ok */

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

  /* Compute energy and forces */

  /* loop over particles and compute enregy and forces */
  for (i = 0; i < *nParts; ++i)
  {
    if (particleContributing[i])
    {
      ier = KIM_Simulator_get_neigh(simulator, 0, i, &numOfPartNeigh,
                                    &neighListOfCurrentPart);
      if (ier)
      {
        /* some sort of problem, exit */
        KIM_report_error(__LINE__, __FILE__, "KIM_get_neigh", ier);
        ier = TRUE;
        return ier;
      }

      /* loop over the neighbors of particle i */
      for (jj = 0; jj < numOfPartNeigh; ++ jj)
      {
        j = neighListOfCurrentPart[jj];  /* get neighbor ID */

        /* compute relative position vector and squared distance */
        Rsqij = 0.0;
        for (k = 0; k < DIM; ++k)
        {
          Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];
          /* compute squared distance */
          Rsqij += Rij[k]*Rij[k];
        }

        /* compute energy and force */
        if (Rsqij < *cutsq)
        {
          /* particles are interacting ? */
          R = sqrt(Rsqij);
          if (comp_force)
          {
            /* compute pair potential and its derivative */
            calc_phi_dphi(epsilon,
                          C,
                          Rzero,
                          shift,
                          cutoff, R, &phi, &dphi);

            /* compute dEidr */
            dEidr = 0.5*dphi;
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
          if (comp_particleEnergy)
          {
            particleEnergy[i] += 0.5*phi;
          }
          if (comp_energy)
          {
            *energy += 0.5*phi;
          }

          /* contribution to forces */
          if (comp_force)
          {
            for (k = 0; k < DIM; ++k)
            {
              force[i*DIM + k] += dEidr*Rij[k]/R;  /* accumulate force on i */
              force[j*DIM + k] -= dEidr*Rij[k]/R;  /* accumulate force on j */
            }
          }
        }  /* if Rsqij */
      }  /* loop on jj */
    }  /* if particleContributing */
  }  /* infinite while loop (terminated by break statements above) */

  /* everything is great */
  ier = FALSE;

  return ier;
}

/* Initialization function */
int model_driver_init(KIM_Simulator * const simulator,
                      char const * const parameterFileNames,
                      int const nameStringLength,
                      int const numberOfParameterFiles)
{
  /* KIM variables */
  char const * paramfile1name;

  /* Local variables */
  FILE* fid;
  double cutoff;
  double epsilon;
  double C;
  double Rzero;
  int ier;
  double dummy;
  struct model_buffer* buffer;

  /* set paramfile1name */
  if (numberOfParameterFiles != 1)
  {
    ier = TRUE;
    KIM_report_error(__LINE__, __FILE__,
                     "Incorrect number of parameter files.", ier);
    return ier;
  }
  paramfile1name = parameterFileNames;

  /* store pointer to functions in KIM object */
  KIM_Simulator_set_destroy(simulator, KIM_LANGUAGE_NAME_C, (func *) destroy);
  KIM_Simulator_set_compute_func(simulator, KIM_LANGUAGE_NAME_C,
                                 (func *) compute);

  /* Read in model parameters from parameter file */
  fid = fopen(paramfile1name, "r");
  if (fid == NULL)
  {
    ier = TRUE;
    KIM_report_error(
        __LINE__, __FILE__,
        "Unable to open parameter file for Morse parameters", ier);
    return ier;
  }

  ier = fscanf(fid, "%lf \n%lf \n%lf \n%lf",
               &cutoff,  /* cutoff distance in angstroms */
               &epsilon,  /* Morse epsilon in eV */
               &C,  /* Morse C in 1/Angstroms */
               &Rzero  /* Morse Rzero in Angstroms */
               );
  fclose(fid);

  /* check that we read the right number of parameters */
  if (4 != ier)
  {
    ier = TRUE;
    KIM_report_error(__LINE__, __FILE__, "Unable to read all parameters", ier);
    return ier;
  }

  /* allocate buffer */
  buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));
  if (NULL == buffer)
  {
    ier = TRUE;
    KIM_report_error(__LINE__, __FILE__, "malloc", ier);
    return ier;
  }

  /* setup buffer */
  /* set value of parameters */
  buffer->influenceDistance = cutoff;
  buffer->cutsq = (cutoff)*(cutoff);
  buffer->epsilon = epsilon;
  buffer->C = C;
  buffer->Rzero = Rzero;
  /* set value of parameter shift */
  dummy = 0.0;
  /* call calc_phi with r=cutoff and shift=0.0 */
  calc_phi(&(buffer->epsilon),
           &(buffer->C),
           &(buffer->Rzero),
           &dummy,
           cutoff, cutoff, &(buffer->shift));
  /* set shift to -shift */
  buffer->shift = -buffer->shift;

  /* end setup buffer */

  /* store in model buffer */
  KIM_Simulator_set_model_buffer(simulator, (void*) buffer);

  /* store model cutoff in KIM object */
  KIM_Simulator_set_influence_distance(simulator, &(buffer->influenceDistance));
  KIM_Simulator_set_cutoffs(simulator, 1, &(buffer->influenceDistance));

  return FALSE;
}

/* destroy function */
static int destroy(KIM_Simulator * const simulator)
{
  /* Local variables */
  struct model_buffer* buffer;
  int ier;

  /* get model buffer from KIM object */
  KIM_Simulator_get_model_buffer(simulator, (void **) &buffer);

  /* free the buffer */
  free(buffer);

  ier = FALSE;
  return ier;
}
