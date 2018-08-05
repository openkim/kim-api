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
/* ex_model_Ar_P_Morse_07C  pair potential KIM Model                          */
/* shifted to have zero energy at the cutoff radius                           */
/*                                                                            */
/* Language: C                                                                */
/*                                                                            */
/******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "KIM_ModelHeaders.h"

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

/* Model buffer definition */
struct buffer
{
  double influenceDistance;
  double cutoff;
  int paddingNeighborHint;
  int halfListHint;
};
typedef struct buffer buffer;

/* Define prototype for Model create */
int model_create(KIM_ModelCreate * const modelCreate,
                 KIM_LengthUnit const requestedLengthUnit,
                 KIM_EnergyUnit const requestedEnergyUnit,
                 KIM_ChargeUnit const requestedChargeUnit,
                 KIM_TemperatureUnit const requestedTemperatureUnit,
                 KIM_TimeUnit const requestedTimeUnit);

/* Define prototype for compute routine */
static int compute(void* km);

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

///* compute function */
//static int compute(void* km) {
//  /* local variables */
//  intptr_t* pkim = *((intptr_t**) km);
//  double R;
//  double R_pairs[2];
//  double *pR_pairs = &(R_pairs[0]);
//  double Rsqij;
//  double phi;
//  double dphi;
//  double d2phi;
//  double dEidr;
//  double d2Eidr;
//  double Rij[DIM];
//  double *pRij = &(Rij[0]);
//  double Rij_pairs[2][3];
//  double *pRij_pairs = &(Rij_pairs[0][0]);
//  int ier;
//  int i;
//  int i_pairs[2];
//  int *pi_pairs = &(i_pairs[0]);
//  int j;
//  int j_pairs[2];
//  int *pj_pairs = &(j_pairs[0]);
//  int jj;
//  int k;
//  int currentPart;
//  int* neighListOfCurrentPart;
//  int comp_energy;
//  int comp_force;
//  int comp_particleEnergy;
//  int comp_process_dEdr;
//  int comp_process_d2Edr2;
//  int one = 1;
//  int request;
//
//  int* nParts;
//  int* particleSpecies;
//  double* cutoff;
//  double cutsq;
//  double epsilon;
//  double C;
//  double Rzero;
//  double shift;
//  double* Rij_list;
//  double* coords;
//  double* energy;
//  double* force;
//  double* particleEnergy;
//  int numOfPartNeigh;
//  double dummy;
//
//  /* check to see if we have been asked to compute the forces, */
//  /* particleEnergy, and d1Edr */
//  KIM_API_getm_compute(pkim, &ier, 5*3,
//                       "energy",         &comp_energy,         1,
//                       "forces",         &comp_force,          1,
//                       "particleEnergy", &comp_particleEnergy, 1,
//                       "process_dEdr",   &comp_process_dEdr,   1,
//                       "process_d2Edr2", &comp_process_d2Edr2, 1
//                       );
//  if (KIM_STATUS_OK > ier) {
//    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute", ier);
//    return ier; }
//
//  KIM_API_getm_data(
//      pkim, &ier, 7*3,
//      "cutoff",                      &cutoff,         1,
//      "numberOfParticles",           &nParts,         1,
//      "particleSpecies",             &particleSpecies,1,
//      "coordinates",                 &coords,         1,
//      "energy",                      &energy,         comp_energy,
//      "forces",                      &force,          comp_force,
//      "particleEnergy",              &particleEnergy, comp_particleEnergy
//                    );
//  if (KIM_STATUS_OK > ier) {
//    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);
//    return ier; }
//
//  /* set value of parameters */
//  cutsq = (*cutoff)*(*cutoff);
//  epsilon = EPSILON;
//  C = PARAM_C;
//  Rzero = RZERO;
//  /* set value of parameter shift */
//  dummy = 0.0;
//  /* call calc_phi with r=cutoff and shift=0.0 */
//  calc_phi(&epsilon, &C, &Rzero, &dummy, cutoff, *cutoff, &shift);
//  /* set shift to -shift */
//  shift = -(shift);
//
//  /* Check to be sure that the species are correct */
//  /**/
//  ier = KIM_STATUS_FAIL; /* assume an error */
//  for (i = 0; i < *nParts; ++i) {
//    if ( SPECCODE != particleSpecies[i]) {
//      KIM_API_report_error(__LINE__, __FILE__,
//                           "Unexpected species detected", ier);
//      return ier; } }
//  ier = KIM_STATUS_OK;  /* everything is ok */
//
//  /* initialize potential energies, forces, and virial term */
//  if (comp_particleEnergy) {
//    for (i = 0; i < *nParts; ++i) {
//      particleEnergy[i] = 0.0; } }
//  if (comp_energy) {
//    *energy = 0.0; }
//
//  if (comp_force) {
//    for (i = 0; i < *nParts; ++i) {
//      for (k = 0; k < DIM; ++k) {
//        force[i*DIM + k] = 0.0; } } }
//
//  /* Compute energy and forces */
//
//  /* loop over particles and compute enregy and forces */
//  for (i = 0; i < *nParts; ++i) {
//    request = i;
//    ier = KIM_API_get_neigh(pkim, one, request, &currentPart,
//                            &numOfPartNeigh, &neighListOfCurrentPart,
//                            &Rij_list);
//    if (KIM_STATUS_OK != ier) {
//      /* some sort of problem, exit */
//      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);
//      ier = KIM_STATUS_FAIL;
//      return ier; }
//
//    /* loop over the neighbors of particle i */
//    for (jj = 0; jj < numOfPartNeigh; ++ jj) {
//      j = neighListOfCurrentPart[jj];  /* get neighbor ID */
//
//      /* compute relative position vector and squared distance */
//      Rsqij = 0.0;
//      for (k = 0; k < DIM; ++k) {
//        Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];
//
//        /* compute squared distance */
//        Rsqij += Rij[k]*Rij[k]; }
//
//      /* compute energy and force */
//      if (Rsqij < cutsq) {
//        /* particles are interacting ? */
//        R = sqrt(Rsqij);
//        if (comp_process_d2Edr2) {
//          /* compute pair potential and its derivatives */
//          calc_phi_d2phi(&epsilon,
//                         &C,
//                         &Rzero,
//                         &shift,
//                         cutoff, R, &phi, &dphi, &d2phi);
//
//          /* compute dEidr */
//          dEidr = 0.5*dphi;
//          d2Eidr = 0.5*d2phi; }
//        else if (comp_force || comp_process_dEdr) {
//          /* compute pair potential and its derivative */
//          calc_phi_dphi(&epsilon,
//                        &C,
//                        &Rzero,
//                        &shift,
//                        cutoff, R, &phi, &dphi);
//
//          /* compute dEidr */
//          dEidr = 0.5*dphi; }
//        else {
//          /* compute just pair potential */
//          calc_phi(&epsilon,
//                   &C,
//                   &Rzero,
//                   &shift,
//                   cutoff, R, &phi); }
//
//        /* contribution to energy */
//        if (comp_particleEnergy) {
//          particleEnergy[i] += 0.5*phi; }
//        if (comp_energy) {
//          *energy += 0.5*phi; }
//
//        /* contribution to process_dEdr */
//        if (comp_process_dEdr) {
//          ier = KIM_API_process_dEdr(km, &dEidr, &R, &pRij, &i, &j); }
//
//        /* contribution to process_d2Edr2 */
//        if (comp_process_d2Edr2) {
//          R_pairs[0] = R_pairs[1] = R;
//          Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];
//          Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];
//          Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];
//          i_pairs[0] = i_pairs[1] = i;
//          j_pairs[0] = j_pairs[1] = j;
//
//          ier = KIM_API_process_d2Edr2(km, &d2Eidr, &pR_pairs, &pRij_pairs,
//                                       &pi_pairs, &pj_pairs); }
//
//        /* contribution to forces */
//        if (comp_force) {
//          for (k = 0; k < DIM; ++k) {
//            force[i*DIM + k] += dEidr*Rij[k]/R;  /* accumulate force on i */
//            force[j*DIM + k] -= dEidr*Rij[k]/R;  /* accumulate force on j */ } } }
//    }  /* loop on jj */
//  }  /* loop on i */
//
//  /* everything is great */
//  ier = KIM_STATUS_OK;
//
//  return ier; }

/* Create function */
#include "KIM_ModelCreateLogMacros.h"
int model_create(KIM_ModelCreate * const modelCreate,
                 KIM_LengthUnit const requestedLengthUnit,
                 KIM_EnergyUnit const requestedEnergyUnit,
                 KIM_ChargeUnit const requestedChargeUnit,
                 KIM_TemperatureUnit const requestedTemperatureUnit,
                 KIM_TimeUnit const requestedTimeUnit)
{
  buffer * bufferPointer;
  int error;

  /* set units */
  LOG_INFORMATION("Set model units");
  error = KIM_ModelCreate_SetUnits(
      modelCreate, /* ignoring requested units */
      KIM_LENGTH_UNIT_A,
      KIM_ENERGY_UNIT_eV,
      KIM_CHARGE_UNIT_unused,
      KIM_TEMPERATURE_UNIT_unused,
      KIM_TIME_UNIT_unused);

  /* register species */
  LOG_INFORMATION("Setting species code");
  error = error ||
      KIM_ModelCreate_SetSpeciesCode(modelCreate,
                                     KIM_SPECIES_NAME_Ar, SPECCODE);

  /* register numbering */
  LOG_INFORMATION("Setting model numbering");
  error = error || KIM_ModelCreate_SetModelNumbering(modelCreate,
                                                     KIM_NUMBERING_zeroBased);

  /* register function pointers */
  LOG_INFORMATION("Register model function pointers");
  error = error ||
      KIM_ModelCreate_SetComputePointer(
          modelCreate,
          KIM_LANGUAGE_NAME_c,
          (func *) 2);
  error = error ||
      KIM_ModelCreate_SetComputeArgumentsCreatePointer(
          modelCreate,
          KIM_LANGUAGE_NAME_c,
          (func *) 2);
  error = error ||
      KIM_ModelCreate_SetComputeArgumentsDestroyPointer(
          modelCreate,
          KIM_LANGUAGE_NAME_c,
          (func *) 2);
  error = error ||
      KIM_ModelCreate_SetDestroyPointer(
          modelCreate,
          KIM_LANGUAGE_NAME_c,
          (func *) 2);
  error = error ||
      KIM_ModelCreate_SetRefreshPointer(
          modelCreate,
          KIM_LANGUAGE_NAME_c,
          (func *) 2);

  /* allocate buffer */
  bufferPointer = (buffer *) malloc(sizeof(buffer));

  /* store model buffer in KIM object */
  LOG_INFORMATION("Set influence distance and cutoffs");
  KIM_ModelCreate_SetModelBufferPointer(modelCreate,
                                        bufferPointer);

  /* set buffer values */
  bufferPointer->influenceDistance = CUTOFF;
  bufferPointer->cutoff = CUTOFF;
  bufferPointer->paddingNeighborHint = 1;
  bufferPointer->halfListHint = 0;

  /* register influence distance */
  KIM_ModelCreate_SetInfluenceDistancePointer(
      modelCreate,
      &(bufferPointer->influenceDistance));

  /* register cutoff */
  KIM_ModelCreate_SetNeighborListPointers(
      modelCreate,
      1,
      &(bufferPointer->cutoff),
      &(bufferPointer->paddingNeighborHint),
      &(bufferPointer->halfListHint));

  if (error)
  {
    free(bufferPointer);
    LOG_ERROR("Unable to successfully initialize model");
    return TRUE;
  }
  else
  {
    printf("%s\n",KIM_ModelCreate_String(modelCreate));
    exit(0);
    return FALSE;
  }
}
