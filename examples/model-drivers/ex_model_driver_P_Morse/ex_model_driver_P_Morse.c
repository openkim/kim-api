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
/* Copyright (c) 2013--2018, Regents of the University of Minnesota.          */
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
/******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "KIM_ModelDriverHeaders.h"

#define TRUE 1
#define FALSE 0

/******************************************************************************/
/* Below are the definitions for some constants                               */
/******************************************************************************/
#define DIM 3  /* dimensionality of space */
#define SPECCODE 1  /* internal species code */


/* Define prototype for Model Driver init */
int model_driver_create(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LengthUnit const requestedLengthUnit,
    KIM_EnergyUnit const requestedEnergyUnit,
    KIM_ChargeUnit const requestedChargeUnit,
    KIM_TemperatureUnit const requestedTemperatureUnit,
    KIM_TimeUnit const requestedTimeUnit);

/* Define prototypes for destroy */
/* defined as static to avoid namespace clashes with other codes */
static int destroy(KIM_ModelDestroy * const modelDestroy);

/* Define prototype for compute routine */
static int compute(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArguments const * const modelComputeArguments);
static int compute_arguments_create(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);
static int compute_arguments_destroy(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

/* Define prototype for refresh routine */
static int refresh(KIM_ModelRefresh * const modelRefresh);

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
  double cutoff;
  double cutsq;
  int modelWillNotRequestNeighborsOfNoncontributingParticles;
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
#include "KIM_ModelComputeLogMacros.h"
static int compute(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArguments const * const modelComputeArguments)
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
  int* particleSpeciesCodes;
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
  KIM_ModelCompute_GetModelBufferPointer(modelCompute, (void **) &buffer);

  /* unpack info from the buffer */
  cutoff = buffer->influenceDistance;
  cutsq = &(buffer->cutsq);
  epsilon = &(buffer->epsilon);
  C = &(buffer->C);
  Rzero = &(buffer->Rzero);
  shift = &(buffer->shift);

  ier =
      KIM_ModelComputeArguments_GetArgumentPointerInteger(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles,
          &nParts)
      ||
      KIM_ModelComputeArguments_GetArgumentPointerInteger(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes,
          &particleSpeciesCodes)
      ||
      KIM_ModelComputeArguments_GetArgumentPointerInteger(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_particleContributing,
          &particleContributing)
      ||
      KIM_ModelComputeArguments_GetArgumentPointerDouble(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_coordinates,
          &coords)
      ||
      KIM_ModelComputeArguments_GetArgumentPointerDouble(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_partialEnergy,
          &energy)
      ||
      KIM_ModelComputeArguments_GetArgumentPointerDouble(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_partialForces,
          &force)
      ||
      KIM_ModelComputeArguments_GetArgumentPointerDouble(
          modelComputeArguments,
          KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy,
          &particleEnergy);
  if (ier)
  {
    LOG_ERROR("GetArgumentPointer");
    return ier;
  }

  comp_energy = (energy != NULL);
  comp_force = (force != NULL);
  comp_particleEnergy = (particleEnergy != NULL);

  /* Check to be sure that the species are correct */
  /**/
  ier = TRUE; /* assume an error */
  for (i = 0; i < *nParts; ++i)
  {
    if ( SPECCODE != particleSpeciesCodes[i])
    {
      LOG_ERROR("Unexpected species code detected");
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
      ier = KIM_ModelComputeArguments_GetNeighborList(
          modelComputeArguments,
          0, i, &numOfPartNeigh, &neighListOfCurrentPart);
      if (ier)
      {
        /* some sort of problem, exit */
        LOG_ERROR("KIM_get_neigh");
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

/* Create function */
#include "KIM_ModelDriverCreateLogMacros.h"
int model_driver_create(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LengthUnit const requestedLengthUnit,
    KIM_EnergyUnit const requestedEnergyUnit,
    KIM_ChargeUnit const requestedChargeUnit,
    KIM_TemperatureUnit const requestedTemperatureUnit,
    KIM_TimeUnit const requestedTimeUnit)
{
  /* KIM variables */
  int numberOfParameterFiles;
  char const * paramfile1name;

  /* Local variables */
  FILE* fid;
  char speciesNameString[100];
  KIM_SpeciesName speciesName;
  double cutoff;
  double epsilon;
  double C;
  double Rzero;
  int ier;
  double dummy;
  struct model_buffer* buffer;

  (void)requestedLengthUnit;  /* avoid unused parameter warnings */
  (void)requestedEnergyUnit;
  (void)requestedChargeUnit;
  (void)requestedTemperatureUnit;
  (void)requestedTimeUnit;


  /* using fixed units */
  ier = KIM_ModelDriverCreate_SetUnits(modelDriverCreate,
                                       KIM_LENGTH_UNIT_A,
                                       KIM_ENERGY_UNIT_eV,
                                       KIM_CHARGE_UNIT_unused,
                                       KIM_TEMPERATURE_UNIT_unused,
                                       KIM_TIME_UNIT_unused);
  if (ier == TRUE)
  {
    LOG_ERROR("Problem setting units");
    return ier;
  }

  ier = KIM_ModelDriverCreate_SetModelNumbering(modelDriverCreate,
                                                KIM_NUMBERING_zeroBased);
  if (ier == TRUE)
  {
    LOG_ERROR("Unable to set numbering");
    return ier;
  }

  /* store pointer to functions in KIM object */
  KIM_ModelDriverCreate_SetDestroyPointer(modelDriverCreate,
                                          KIM_LANGUAGE_NAME_c,
                                          (func *) destroy);
  KIM_ModelDriverCreate_SetComputeArgumentsCreatePointer(modelDriverCreate,
                                          KIM_LANGUAGE_NAME_c,
                                          (func *) compute_arguments_create);
  KIM_ModelDriverCreate_SetComputeArgumentsDestroyPointer(modelDriverCreate,
                                          KIM_LANGUAGE_NAME_c,
                                          (func *) compute_arguments_destroy);
  KIM_ModelDriverCreate_SetComputePointer(modelDriverCreate,
                                          KIM_LANGUAGE_NAME_c,
                                          (func *) compute);
  KIM_ModelDriverCreate_SetRefreshPointer(
      modelDriverCreate, KIM_LANGUAGE_NAME_c, (func *) refresh);

  /* get number of parameter files */
  KIM_ModelDriverCreate_GetNumberOfParameterFiles(
      modelDriverCreate, &numberOfParameterFiles);
  /* set paramfile1name */
  if (numberOfParameterFiles != 1)
  {
    ier = TRUE;
    LOG_ERROR("Incorrect number of parameter files.");
    return ier;
  }
  ier = KIM_ModelDriverCreate_GetParameterFileName(
      modelDriverCreate,
      0,
      &paramfile1name);
  if (ier == TRUE)
  {
    LOG_ERROR("Unable to get parameter file name.");
    return ier;
  }

  /* Read in model parameters from parameter file */
  fid = fopen(paramfile1name, "r");
  if (fid == NULL)
  {
    ier = TRUE;
    LOG_ERROR("Unable to open parameter file for Morse parameters");
    return ier;
  }

  ier = fscanf(fid, "%s \n%lf \n%lf \n%lf \n%lf",
               speciesNameString, /* element symbol */
               &cutoff,  /* cutoff distance in angstroms */
               &epsilon,  /* Morse epsilon in eV */
               &C,  /* Morse C in 1/Angstroms */
               &Rzero  /* Morse Rzero in Angstroms */
               );
  fclose(fid);

  /* check that we read the right number of parameters */
  if (5 != ier)
  {
    ier = TRUE;
    LOG_ERROR("Unable to read all parameters");
    return ier;
  }

  /* register species */
  speciesName = KIM_SpeciesName_FromString(speciesNameString);
  ier = KIM_ModelDriverCreate_SetSpeciesCode(
      modelDriverCreate,
      speciesName,
      SPECCODE);
  if (ier == TRUE)
  {
    LOG_ERROR("Unable to set species code for Ar.");
    return ier;
  }


  /* allocate buffer */
  buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));
  if (NULL == buffer)
  {
    ier = TRUE;
    LOG_ERROR("malloc");
    return ier;
  }

  /* setup buffer */
  /* set value of parameters */
  buffer->influenceDistance = cutoff;
  buffer->cutoff = cutoff;
  buffer->cutsq = (cutoff)*(cutoff);
  buffer->modelWillNotRequestNeighborsOfNoncontributingParticles = 1;
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
  KIM_ModelDriverCreate_SetModelBufferPointer(modelDriverCreate,
                                              (void*) buffer);

  /* store model cutoff in KIM object */
  KIM_ModelDriverCreate_SetInfluenceDistancePointer(
      modelDriverCreate,
      &(buffer->influenceDistance));
  KIM_ModelDriverCreate_SetNeighborListPointers(
      modelDriverCreate,
      1,
      &(buffer->cutoff),
      &(buffer->modelWillNotRequestNeighborsOfNoncontributingParticles));

  return FALSE;
}

/* refresh function */
static int refresh(KIM_ModelRefresh * const modelRefresh)
{
  /* Local variables */
  struct model_buffer* buffer;

  /* get model buffer from KIM object */
  KIM_ModelRefresh_GetModelBufferPointer(modelRefresh, (void **) &buffer);

  KIM_ModelRefresh_SetInfluenceDistancePointer(
      modelRefresh, &(buffer->influenceDistance));
  KIM_ModelRefresh_SetNeighborListPointers(
      modelRefresh,
      1,
      &(buffer->cutoff),
      &(buffer->modelWillNotRequestNeighborsOfNoncontributingParticles));

  return FALSE;
}

/* destroy function */
static int destroy(KIM_ModelDestroy * const modelDestroy)
{
  /* Local variables */
  struct model_buffer* buffer;
  int ier;

  /* get model buffer from KIM object */
  KIM_ModelDestroy_GetModelBufferPointer(modelDestroy, (void **) &buffer);

  /* free the buffer */
  free(buffer);

  ier = FALSE;
  return ier;
}

/* compute arguments create routine */
#include "KIM_ModelComputeArgumentsCreateLogMacros.h"
static int compute_arguments_create(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
{
  int ier;

  (void)modelCompute;  /* avoid unused parameter warning */

  /* register arguments */
  ier = KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
      modelComputeArgumentsCreate,
      KIM_COMPUTE_ARGUMENT_NAME_partialEnergy,
      KIM_SUPPORT_STATUS_optional)
      ||
      KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
          modelComputeArgumentsCreate,
          KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy,
          KIM_SUPPORT_STATUS_optional)
      ||
      KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
          modelComputeArgumentsCreate,
          KIM_COMPUTE_ARGUMENT_NAME_partialForces,
          KIM_SUPPORT_STATUS_optional);
  if (ier == TRUE)
  {
    LOG_ERROR("Unable to set argument supportStatus.");
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}

/* compue arguments destroy routine */
#include "KIM_ModelComputeArgumentsDestroyLogMacros.h"
static int compute_arguments_destroy(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
{

  (void)modelCompute;  /* avoid unused parameter warning */
  (void)modelComputeArgumentsDestroy;

  /* nothing to be done */

  return FALSE;
}
