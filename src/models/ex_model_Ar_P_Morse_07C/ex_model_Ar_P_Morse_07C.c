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
/* ex_model_Ar_P_Morse_07C  pair potential KIM Model                          */
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
#include "KIM_Numbering.h"
#include "KIM_LanguageName.h"
#include "KIM_SpeciesName.h"
#include "KIM_Attribute.h"
#include "KIM_ArgumentName.h"
#include "KIM_CallBackName.h"
#include "KIM_LogVerbosity.h"
#include "KIM_UnitSystem.h"
#include "KIM_ModelInitialization.h"
#include "KIM_ModelReinitialization.h"
#include "KIM_ModelCompute.h"
#include "KIM_ModelDestroy.h"

#define TRUE 1
#define FALSE 0

/******************************************************************************/
/* Below are the definitions and values of all Model parameters               */
/******************************************************************************/
#define DIM 3  /* dimensionality of space */
#define SPECCODE 1  /* internal species code */
#define CUTOFF 8.15  /* Angstroms */
#define EPSILON -0.0134783698072604  /* eV */
#define PARAM_C 1.545  /* 1/Angstroms */
#define RZERO   3.786  /* Angstroms */


/* Define prototype for Model init */
int model_init(KIM_ModelInitialization * const modelInitialization,
               KIM_LengthUnit const requestedLengthUnit,
               KIM_EnergyUnit const requestedEnergyUnit,
               KIM_ChargeUnit const requestedChargeUnit,
               KIM_TemperatureUnit const requestedTemperatureUnit,
               KIM_TimeUnit const requestedTimeUnit);

/* Define prototype for compute routine */
static int compute(KIM_ModelCompute const * const modelCompute);
static int model_reinit(
    KIM_ModelReinitialization * const modelReinitialization);
static int model_destroy(KIM_ModelDestroy * const modelDestroy);

/* Define prototypes for pair potential calculations */
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
                           double* cutoff, double r, double* phi, double* dphi,
                           double* d2phi);

/* Calculate pair potential phi(r) */
static void calc_phi(double* epsilon,
                     double* C,
                     double* Rzero,
                     double* shift,
                     double* cutoff, double r, double* phi) {
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C)*(r-*Rzero));
  ep2 = ep*ep;

  if (r > *cutoff) {
    /* Argument exceeds cutoff radius */
    *phi = 0.0; }
  else {
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift; }

  return; }

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double* epsilon,
                          double* C,
                          double* Rzero,
                          double* shift,
                          double* cutoff, double r, double* phi, double* dphi) {
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C)*(r-*Rzero));
  ep2 = ep*ep;

  if (r > *cutoff) {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
    *dphi = 0.0; }
  else {
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
    *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 ); }

  return; }

/* Calculate pair potential phi(r) and its 1st & 2nd derivatives dphi(r), */
/* d2phi(r) */
static void calc_phi_d2phi(double* epsilon,
                           double* C,
                           double* Rzero,
                           double* shift,
                           double* cutoff, double r, double* phi, double* dphi,
                           double* d2phi) {
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C)*(r-*Rzero));
  ep2 = ep*ep;

  if (r > *cutoff) {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
    *dphi = 0.0;
    *d2phi = 0.0; }
  else {
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;
    *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 );
    *d2phi = 2.0*(*epsilon)*(*C)*(*C)*(ep - 2.0*ep2); }

  return; }

/* compute function */
#include "KIM_ModelComputeLogMacros.h"
static int compute(KIM_ModelCompute const * const modelCompute)
{
  /* local variables */
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
  double const * pRij_pairs = &(Rij_pairs[0][0]);
  int ier;
  int i;
  int i_pairs[2];
  int *pi_pairs = &(i_pairs[0]);
  int j;
  int j_pairs[2];
  int *pj_pairs = &(j_pairs[0]);
  int jj;
  int k;
  int const * neighListOfCurrentPart;
  int comp_energy;
  int comp_force;
  int comp_particleEnergy;
  int comp_process_dEdr;
  int comp_process_d2Edr2;

  int* nParts;
  int* particleSpecies;
  int* particleContributing;
  double* cutoff;
  double cutsq;
  double epsilon;
  double C;
  double Rzero;
  double shift;
  double* coords;
  double* energy;
  double* force;
  double* particleEnergy;
  int numOfPartNeigh;
  double dummy;

  /* check to see if we have been asked to compute the forces, */
  /* particleEnergy, and d1Edr */
  LOG_INFORMATION("Checking if call backs are present.");
  KIM_ModelCompute_is_call_back_present(modelCompute,
                                        KIM_CALL_BACK_NAME_process_dEdr,
                                        &comp_process_dEdr);
  KIM_ModelCompute_is_call_back_present(modelCompute,
                                        KIM_CALL_BACK_NAME_process_d2Edr2,
                                        &comp_process_d2Edr2);

  LOG_INFORMATION("Getting data pointers");
  ier =
      KIM_ModelCompute_get_data_int(modelCompute,
                                    KIM_ARGUMENT_NAME_numberOfParticles,
                                    &nParts)
      ||
      KIM_ModelCompute_get_data_int(modelCompute,
                                    KIM_ARGUMENT_NAME_particleSpecies,
                                    &particleSpecies)
      ||
      KIM_ModelCompute_get_data_int(modelCompute,
                                    KIM_ARGUMENT_NAME_particleContributing,
                                    &particleContributing)
      ||
      KIM_ModelCompute_get_data_double(modelCompute,
                                       KIM_ARGUMENT_NAME_coordinates,
                                       &coords)
      ||
      KIM_ModelCompute_get_data_double(modelCompute, KIM_ARGUMENT_NAME_energy,
                                       &energy)
      ||
      KIM_ModelCompute_get_data_double(modelCompute, KIM_ARGUMENT_NAME_forces,
                                       &force)
      ||
      KIM_ModelCompute_get_data_double(modelCompute,
                                       KIM_ARGUMENT_NAME_particleEnergy,
                                       &particleEnergy);
  if (ier) {
    LOG_ERROR("get data pointers failed");
    return ier; }

  comp_energy = (energy != 0);
  comp_force = (force != 0);
  comp_particleEnergy = (particleEnergy != 0);

  /* set value of parameters */
  KIM_ModelCompute_get_model_buffer(modelCompute, (void**) &cutoff);
  cutsq = (*cutoff)*(*cutoff);
  epsilon = EPSILON;
  C = PARAM_C;
  Rzero = RZERO;
  /* set value of parameter shift */
  dummy = 0.0;
  /* call calc_phi with r=cutoff and shift=0.0 */
  calc_phi(&epsilon, &C, &Rzero, &dummy, cutoff, *cutoff, &shift);
  /* set shift to -shift */
  shift = -(shift);

  /* Check to be sure that the species are correct */
  /**/
  ier = TRUE; /* assume an error */
  for (i = 0; i < *nParts; ++i) {
    if ( SPECCODE != particleSpecies[i]) {
      LOG_ERROR("Unexpected species detected");
      return ier; } }
  ier = FALSE;  /* everything is ok */

  /* initialize potential energies, forces, and virial term */
  LOG_INFORMATION("Initializing data");
  if (comp_particleEnergy) {
    for (i = 0; i < *nParts; ++i) {
      particleEnergy[i] = 0.0; } }
  if (comp_energy) {
    *energy = 0.0; }

  if (comp_force) {
    for (i = 0; i < *nParts; ++i) {
      for (k = 0; k < DIM; ++k) {
        force[i*DIM + k] = 0.0; } } }

  /* Compute energy and forces */

  /* loop over particles and compute enregy and forces */
  LOG_INFORMATION("Starting main compute loop");
  for (i = 0; i< *nParts; ++i) {
    if (particleContributing[i])
    {
      ier = KIM_ModelCompute_get_neigh(
          modelCompute, 0, i, &numOfPartNeigh, &neighListOfCurrentPart);
      if (ier) {
        /* some sort of problem, exit */
        LOG_ERROR("GetNeighborList failed");
        ier = TRUE;
        return ier; }

      /* loop over the neighbors of particle i */
      for (jj = 0; jj < numOfPartNeigh; ++ jj) {
        j = neighListOfCurrentPart[jj];  /* get neighbor ID */

        /* compute relative position vector and squared distance */
        Rsqij = 0.0;
        for (k = 0; k < DIM; ++k) {
          Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];

          /* compute squared distance */
          Rsqij += Rij[k]*Rij[k]; }

        /* compute energy and force */
        if (Rsqij < cutsq) {
          /* particles are interacting ? */
          R = sqrt(Rsqij);
          if (comp_process_d2Edr2) {
            /* compute pair potential and its derivatives */
            calc_phi_d2phi(&epsilon,
                           &C,
                           &Rzero,
                           &shift,
                           cutoff, R, &phi, &dphi, &d2phi);

            /* compute dEidr */
            /* Full mode -- regular contribution */
            dEidr = 0.5*dphi;
            d2Eidr = 0.5*d2phi; }
          else if (comp_force || comp_process_dEdr) {
            /* compute pair potential and its derivative */
            calc_phi_dphi(&epsilon,
                          &C,
                          &Rzero,
                          &shift,
                          cutoff, R, &phi, &dphi);

            /* compute dEidr */
            /* Full mode -- regular contribution */
            dEidr = 0.5*dphi; }
          else {
            /* compute just pair potential */
            calc_phi(&epsilon,
                     &C,
                     &Rzero,
                     &shift,
                     cutoff, R, &phi); }

          /* contribution to energy */
          if (comp_particleEnergy) {
            particleEnergy[i] += 0.5*phi; }
          if (comp_energy) {
            /* Full mode -- add half v to total energy */
            *energy += 0.5*phi; }

          /* contribution to process_dEdr */
          if (comp_process_dEdr) {
            ier = KIM_ModelCompute_process_dEdr(
                modelCompute, dEidr, R, pRij, i, j); }

          /* contribution to process_d2Edr2 */
          if (comp_process_d2Edr2) {
            R_pairs[0] = R_pairs[1] = R;
            Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];
            Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];
            Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];
            i_pairs[0] = i_pairs[1] = i;
            j_pairs[0] = j_pairs[1] = j;

            ier = KIM_ModelCompute_process_d2Edr2(
                modelCompute, d2Eidr, pR_pairs, pRij_pairs, pi_pairs, pj_pairs);
          }

          /* contribution to forces */
          if (comp_force) {
            for (k = 0; k < DIM; ++k) {
              force[i*DIM + k] += dEidr*Rij[k]/R;  /* accumulate force on i */
              force[j*DIM + k] -= dEidr*Rij[k]/R;  /* accumulate force on j */ } } }
      }  /* loop on jj */
    } /* if contributing */
  }  /* loop on i */
  LOG_INFORMATION("Finished compute loop");

  /* everything is great */
  ier = FALSE;

  return ier; }

/* Initialization function */
#include "KIM_ModelInitializationLogMacros.h"
int model_init(KIM_ModelInitialization * const modelInitialization,
               KIM_LengthUnit const requestedLengthUnit,
               KIM_EnergyUnit const requestedEnergyUnit,
               KIM_ChargeUnit const requestedChargeUnit,
               KIM_TemperatureUnit const requestedTemperatureUnit,
               KIM_TimeUnit const requestedTimeUnit)
{
  double* model_cutoff;
  int error;

  /* register numbering */
  LOG_INFORMATION("Setting model numbering");
  error = KIM_ModelInitialization_set_model_numbering(modelInitialization,
                                                      KIM_NUMBERING_zeroBased);

  /* register species */
  LOG_INFORMATION("Setting species code");
  error = error ||
      KIM_ModelInitialization_set_species_code(modelInitialization,
                                               KIM_SPECIES_NAME_Ar, SPECCODE);

  /* register arguments */
  LOG_INFORMATION("Register argument attributes");
  error = error ||
      KIM_ModelInitialization_set_argument_attribute(
          modelInitialization,
          KIM_ARGUMENT_NAME_energy, KIM_ATTRIBUTE_optional);
  error = error ||
      KIM_ModelInitialization_set_argument_attribute(
          modelInitialization,
          KIM_ARGUMENT_NAME_forces, KIM_ATTRIBUTE_optional);
  error = error ||
      KIM_ModelInitialization_set_argument_attribute(
          modelInitialization,
          KIM_ARGUMENT_NAME_particleEnergy, KIM_ATTRIBUTE_optional);

  /* register call backs */
  LOG_INFORMATION("Register call back attributes");
  error = error ||
      KIM_ModelInitialization_set_call_back_attribute(
          modelInitialization,
          KIM_CALL_BACK_NAME_process_dEdr, KIM_ATTRIBUTE_optional);
  error = error ||
      KIM_ModelInitialization_set_call_back_attribute(
          modelInitialization,
          KIM_CALL_BACK_NAME_process_d2Edr2, KIM_ATTRIBUTE_optional);

  /* register function pointers */
  LOG_INFORMATION("Register model function pointers");
  error = error ||
      KIM_ModelInitialization_set_compute_func(modelInitialization,
                                               KIM_LANGUAGE_NAME_C,
                                               (func *) &compute);
  error = error ||
      KIM_ModelInitialization_set_destroy(modelInitialization,
                                          KIM_LANGUAGE_NAME_C,
                                          (func *) &model_destroy);
  error = error ||
      KIM_ModelInitialization_set_reinit(modelInitialization,
                                         KIM_LANGUAGE_NAME_C,
                                         (func *) &model_reinit);

  /* set units */
  LOG_INFORMATION("Set model units");
  error = error || KIM_ModelInitialization_set_units(
      modelInitialization, /* ignoring requested units */
      KIM_LENGTH_UNIT_A,
      KIM_ENERGY_UNIT_eV,
      KIM_CHARGE_UNIT_any,
      KIM_TEMPERATURE_UNIT_any,
      KIM_TIME_UNIT_any);

  /* store model cutoff in KIM object */
  LOG_INFORMATION("Set influence distance and cutoffs");
  model_cutoff = (double*) malloc(sizeof(double));
  *model_cutoff = CUTOFF;
  KIM_ModelInitialization_set_model_buffer(modelInitialization, model_cutoff);

  /* register influence distance and cutoffs */
  KIM_ModelInitialization_set_influence_distance(modelInitialization,
                                                 model_cutoff);
  KIM_ModelInitialization_set_cutoffs(modelInitialization, 1, model_cutoff);

  if (error)
  {
    LOG_ERROR("Unable to successfully initialize model");
    return TRUE;
  }
  else
    return FALSE;
}

/* reinitialization function */
#include "KIM_ModelReinitializationLogMacros.h"
static int model_reinit(KIM_ModelReinitialization * const modelReinitialization)
{
  /* Local variables */
  double * model_cutoff;

  /* get model buffer from KIM object */
  LOG_INFORMATION("Getting model buffer");
  KIM_ModelReinitialization_get_model_buffer(modelReinitialization,
                                             (void **) &model_cutoff);

  LOG_INFORMATION("Resetting influence distance and cutoffs");
  KIM_ModelReinitialization_set_influence_distance(
      modelReinitialization, model_cutoff);
  KIM_ModelReinitialization_set_cutoffs(modelReinitialization, 1, model_cutoff);

  return FALSE;
}

/* Initialization function */
#include "KIM_ModelDestroyLogMacros.h"
int model_destroy(KIM_ModelDestroy * const modelDestroy) {
  double* model_cutoff;

  LOG_INFORMATION("Getting buffer");
  KIM_ModelDestroy_get_model_buffer(modelDestroy, (void **) &model_cutoff);
  LOG_INFORMATION("Freeing model memory");
  free(model_cutoff);

  return FALSE; }
