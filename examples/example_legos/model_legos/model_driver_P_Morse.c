/*                                                                            *//*123456789012345*/
/* CDDL HEADER START                                                          *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* The contents of this file are subject to the terms of the Common           *//*123456789012345*/
/* Development and Distribution License Version 1.0 (the "License").          *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* You can obtain a copy of the license at                                    *//*123456789012345*/
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      *//*123456789012345*/
/* specific language governing permissions and limitations under the License. *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* When distributing Covered Code, include this CDDL HEADER in each file and  *//*123456789012345*/
/* include the License file in a prominent location with the name             *//*123456789012345*/
/* LICENSE.CDDL.  If applicable, add the following below this CDDL HEADER,    *//*123456789012345*/
/* with the fields enclosed by brackets "[]" replaced with your own           *//*123456789012345*/
/* identifying information:                                                   *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   *//*123456789012345*/
/* All rights reserved.                                                       *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* CDDL HEADER END                                                            *//*123456789012345*/
/*                                                                            *//*123456789012345*/
                                                                                /*123456789012345*/
/*                                                                            *//*123456789012345*/
/* Copyright (c) 2013--2014, Regents of the University of Minnesota.          *//*123456789012345*/
/* All rights reserved.                                                       *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* Contributors:                                                              *//*123456789012345*/
/*    Ryan S. Elliott                                                         *//*123456789012345*/
/*    Ellad B. Tadmor                                                         *//*123456789012345*/
/*    Valeriu Smirichinski                                                    *//*123456789012345*/
/*    Stephen M. Whalen                                                       *//*123456789012345*/
/*                                                                            *//*123456789012345*/
                                                                                /*123456789012345*/
/******************************************************************************//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* Morse pair potential KIM Model Driver                                      *//*123456789012345*/
/* shifted to have zero energy at the cutoff radius                           *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* Language: C                                                                *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/* Release: This file is part of the kim-api.git repository.                  *//*123456789012345*/
/*                                                                            *//*123456789012345*/
/******************************************************************************//*123456789012345*/
                                                                                /*123456789012345*/
                                                                                /*123456789012345*/
#include <stdlib.h>                                                             /*123456789012345*/
#include <stdio.h>                                                              /*123456789012345*/
#include <string.h>                                                             /*123456789012345*/
#include <math.h>                                                               /*123456789012345*/
#include "KIM_API_C.h"                                                          /*123456789012345*/
#include "KIM_API_status.h"                                                     /*123456789012345*/
                                                                                /*123456789012345*/
/******************************************************************************//*123456789012345*/
/* Below are the definitions and values of all Model parameters                 /*123456789012345*/
/******************************************************************************//*123456789012345*/
#define DIM 3       /* dimensionality of space */                               /*123456789012345*/
#define SPECCODE 1  /* internal species code */                                 /*123456789012345*/
                                                                                /*123456789012345*/
                                                                                /*123456789012345*/
/* Define prototypes for Model Driver init */                                   /*123456789012345*/
/**/                                                                            /*123456789012345*/
int model_driver_init(void* km, char* paramfile_names, int* nmstrlen,           /*123456789012345*/
                      int* numparamfiles);                                      /*123456789012345*/
                                                                                /*123456789012345*/
/* Define prototypes for Model (Driver) reinit, compute, and destroy */         /*123456789012345*/
/* defined as static to avoid namespace clashes with other Models    */         /*123456789012345*/
/**/                                                                            /*123456789012345*/
static int reinit(void* km);                                                    /*123456789012345*/
static int destroy(void* km);                                                   /*123456789012345*/
static int compute(void* km);                                                   /*123456789012345*/
/**/                                                                            /*123456789012345*/
static void calc_phi(double* epsilon,                                           /*123456789012345*/
                     double* C,                                                 /*123456789012345*/
                     double* Rzero,                                             /*123456789012345*/
                     double* shift,                                             /*123456789012345*/
                     double* cutoff, double r, double* phi);                    /*123456789012345*/
static void calc_phi_dphi(double* epsilon,                                      /*123456789012345*/
                          double* C,                                            /*123456789012345*/
                          double* Rzero,                                        /*123456789012345*/
                          double* shift,                                        /*123456789012345*/
                          double* cutoff, double r, double* phi, double* dphi); /*123456789012345*/
                                                                                /*123456789012345*/
static void calc_phi_d2phi(double* epsilon,                                     /*123456789012345*/
                           double* C,                                           /*123456789012345*/
                           double* Rzero,                                       /*123456789012345*/
                           double* shift,                                       /*123456789012345*/
                           double* cutoff, double r, double* phi, double* dphi, /*123456789012345*/
                           double* d2phi);                                      /*123456789012345*/
                                                                                /*123456789012345*/
/* Define model_buffer structure */                                             /*123456789012345*/
struct model_buffer                                                             /*123456789012345*/
{                                                                               /*123456789012345*/
  int NBC;                                                                      /*123456789012345*/
  int HalfOrFull;                                                               /*123456789012345*/
  int IterOrLoca;                                                               /*123456789012345*/
  int energy_ind;                                                               /*123456789012345*/
  int forces_ind;                                                               /*123456789012345*/
  int particleEnergy_ind;                                                       /*123456789012345*/
  int process_dEdr_ind;                                                         /*123456789012345*/
  int process_d2Edr2_ind;                                                       /*123456789012345*/
  int model_index_shift;                                                        /*123456789012345*/
  int numberOfParticles_ind;                                                    /*123456789012345*/
  int particleSpecies_ind;                                                      /*123456789012345*/
  int coordinates_ind;                                                          /*123456789012345*/
  int numberContribParticles_ind;                                               /*123456789012345*/
  int boxSideLengths_ind;                                                       /*123456789012345*/
  int get_neigh_ind;                                                            /*123456789012345*/
  int cutoff_ind;                                                               /*123456789012345*/
                                                                                /*123456789012345*/
  double Pcutoff;                                                               /*123456789012345*/
  double cutsq;                                                                 /*123456789012345*/
  double epsilon;                                                               /*123456789012345*/
  double C;                                                                     /*123456789012345*/
  double Rzero;                                                                 /*123456789012345*/
  double shift;                                                                 /*123456789012345*/
};                                                                              /*123456789012345*/
                                                                                /*123456789012345*/
                                                                                /*123456789012345*/
/* Calculate pair potential phi(r) */                                           /*123456789012345*/
static void calc_phi(double* epsilon,                                           /*123456789012345*/
                     double* C,                                                 /*123456789012345*/
                     double* Rzero,                                             /*123456789012345*/
                     double* shift,                                             /*123456789012345*/
                     double* cutoff, double r, double* phi)                     /*123456789012345*/
{                                                                               /*123456789012345*/
  /* local variables */                                                         /*123456789012345*/
  double ep;                                                                    /*123456789012345*/
  double ep2;                                                                   /*123456789012345*/
                                                                                /*123456789012345*/
  ep  = exp(-(*C)*(r-*Rzero));                                                  /*123456789012345*/
  ep2 = ep*ep;                                                                  /*123456789012345*/
                                                                                /*123456789012345*/
  if (r > *cutoff)                                                              /*123456789012345*/
  {                                                                             /*123456789012345*/
    /* Argument exceeds cutoff radius */                                        /*123456789012345*/
    *phi = 0.0;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    *phi   = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;                             /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  return;                                                                       /*123456789012345*/
}                                                                               /*123456789012345*/
                                                                                /*123456789012345*/
/* Calculate pair potential phi(r) and its derivative dphi(r) */                /*123456789012345*/
static void calc_phi_dphi(double* epsilon,                                      /*123456789012345*/
                          double* C,                                            /*123456789012345*/
                          double* Rzero,                                        /*123456789012345*/
                          double* shift,                                        /*123456789012345*/
                          double* cutoff, double r, double* phi, double* dphi)  /*123456789012345*/
{                                                                               /*123456789012345*/
  /* local variables */                                                         /*123456789012345*/
  double ep;                                                                    /*123456789012345*/
  double ep2;                                                                   /*123456789012345*/
                                                                                /*123456789012345*/
  ep  = exp(-(*C)*(r-*Rzero));                                                  /*123456789012345*/
  ep2 = ep*ep;                                                                  /*123456789012345*/
                                                                                /*123456789012345*/
  if (r > *cutoff)                                                              /*123456789012345*/
  {                                                                             /*123456789012345*/
    /* Argument exceeds cutoff radius */                                        /*123456789012345*/
    *phi  = 0.0;                                                                /*123456789012345*/
    *dphi = 0.0;                                                                /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    *phi  = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;                              /*123456789012345*/
    *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 );                                  /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  return;                                                                       /*123456789012345*/
}                                                                               /*123456789012345*/
                                                                                /*123456789012345*/
/* Calculate pair potential phi(r) and its 1st & 2nd derivatives dphi(r), */    /*123456789012345*/
/* d2phi(r) */                                                                  /*123456789012345*/
static void calc_phi_d2phi(double* epsilon,                                     /*123456789012345*/
                           double* C,                                           /*123456789012345*/
                           double* Rzero,                                       /*123456789012345*/
                           double* shift,                                       /*123456789012345*/
                           double* cutoff, double r, double* phi,               /*123456789012345*/
                           double* dphi, double* d2phi)                         /*123456789012345*/
{                                                                               /*123456789012345*/
  /* local variables */                                                         /*123456789012345*/
  double ep;                                                                    /*123456789012345*/
  double ep2;                                                                   /*123456789012345*/
                                                                                /*123456789012345*/
  ep  = exp(-(*C)*(r-*Rzero));                                                  /*123456789012345*/
  ep2 = ep*ep;                                                                  /*123456789012345*/
                                                                                /*123456789012345*/
  if (r > *cutoff)                                                              /*123456789012345*/
  {                                                                             /*123456789012345*/
    /* Argument exceeds cutoff radius */                                        /*123456789012345*/
    *phi   = 0.0;                                                               /*123456789012345*/
    *dphi  = 0.0;                                                               /*123456789012345*/
    *d2phi = 0.0;                                                               /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    *phi   = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;                             /*123456789012345*/
    *dphi  = 2.0*(*epsilon)*(*C)*( -ep + ep2 );                                 /*123456789012345*/
    *d2phi = 2.0*(*epsilon)*(*C)*(*C)*(ep - 2.0*ep2);                           /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  return;                                                                       /*123456789012345*/
}                                                                               /*123456789012345*/
                                                                                /*123456789012345*/
/* compute function */                                                          /*123456789012345*/
static int compute(void* km)                                                    /*123456789012345*/
{                                                                               /*123456789012345*/
  /* local variables */                                                         /*123456789012345*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*123456789012345*/
  double R;                                                                     /*123456789012345*/
  double R_pairs[2];                                                            /*123456789012345*/
  double *pR_pairs = &(R_pairs[0]);                                             /*123456789012345*/
  double Rsqij;                                                                 /*123456789012345*/
  double phi;                                                                   /*123456789012345*/
  double dphi;                                                                  /*123456789012345*/
  double d2phi;                                                                 /*123456789012345*/
  double dEidr;                                                                 /*123456789012345*/
  double d2Eidr;                                                                /*123456789012345*/
  double Rij[DIM];                                                              /*123456789012345*/
  double *pRij = &(Rij[0]);                                                     /*123456789012345*/
  double Rij_pairs[2][3];                                                       /*123456789012345*/
  double *pRij_pairs = &(Rij_pairs[0][0]);                                      /*123456789012345*/
  int ier;                                                                      /*123456789012345*/
  int i;                                                                        /*123456789012345*/
  int i_pairs[2];                                                               /*123456789012345*/
  int *pi_pairs = &(i_pairs[0]);                                                /*123456789012345*/
  int j;                                                                        /*123456789012345*/
  int j_pairs[2];                                                               /*123456789012345*/
  int *pj_pairs = &(j_pairs[0]);                                                /*123456789012345*/
  int jj;                                                                       /*123456789012345*/
  int k;                                                                        /*123456789012345*/
  int currentPart;                                                              /*123456789012345*/
  int* neighListOfCurrentPart;                                                  /*123456789012345*/
  struct model_buffer* buffer;                                                  /*123456789012345*/
  int comp_energy;                                                              /*123456789012345*/
  int comp_force;                                                               /*123456789012345*/
  int comp_particleEnergy;                                                      /*123456789012345*/
  int comp_process_dEdr;                                                        /*123456789012345*/
  int comp_process_d2Edr2;                                                      /*123456789012345*/
  int NBC;                                                                      /*123456789012345*/
  int HalfOrFull;                                                               /*123456789012345*/
  int IterOrLoca;                                                               /*123456789012345*/
  int model_index_shift;                                                        /*123456789012345*/
  int zero = 0;                                                                 /*123456789012345*/
  int one = 1;                                                                  /*123456789012345*/
  int request;                                                                  /*123456789012345*/
                                                                                /*123456789012345*/
  int* nParts;                                                                  /*123456789012345*/
  int* particleSpecies;                                                         /*123456789012345*/
  double* cutoff;                                                               /*123456789012345*/
  double* cutsq;                                                                /*123456789012345*/
  double* epsilon;                                                              /*123456789012345*/
  double* C;                                                                    /*123456789012345*/
  double* Rzero;                                                                /*123456789012345*/
  double* shift;                                                                /*123456789012345*/
  double* Rij_list;                                                             /*123456789012345*/
  double* coords;                                                               /*123456789012345*/
  double* energy;                                                               /*123456789012345*/
  double* force;                                                                /*123456789012345*/
  double* particleEnergy;                                                       /*123456789012345*/
  double* boxSideLengths;                                                       /*123456789012345*/
  int* numContrib;                                                              /*123456789012345*/
  int numberContrib;                                                            /*123456789012345*/
  int numOfPartNeigh;                                                           /*123456789012345*/
  typedef int (*get_neigh_ptr)(void *,int *,int *,int *, int *, int **,         /*123456789012345*/
                               double **);                                      /*123456789012345*/
  get_neigh_ptr get_neigh = NULL;                                               /*123456789012345*/
                                                                                /*123456789012345*/
                                                                                /*123456789012345*/
  /* get buffer from KIM object */                                              /*123456789012345*/
  buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);         /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);  /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* unpack info from the buffer */                                             /*123456789012345*/
  NBC = buffer->NBC;                                                            /*123456789012345*/
  HalfOrFull = buffer->HalfOrFull;                                              /*123456789012345*/
  IterOrLoca = buffer->IterOrLoca;                                              /*123456789012345*/
  model_index_shift = buffer->model_index_shift;                                /*123456789012345*/
  cutsq = &(buffer->cutsq);                                                     /*123456789012345*/
  epsilon = &(buffer->epsilon);                                                 /*123456789012345*/
  C = &(buffer->C);                                                             /*123456789012345*/
  Rzero = &(buffer->Rzero);                                                     /*123456789012345*/
  shift = &(buffer->shift);                                                     /*123456789012345*/
                                                                                /*123456789012345*/
  /* check to see if we have been asked to compute the forces, */               /*123456789012345*/
  /* particleEnergy, and d1Edr */                                               /*123456789012345*/
  KIM_API_getm_compute_by_index(                                                /*123456789012345*/
      pkim, &ier, 5*3,                                                          /*123456789012345*/
      buffer->energy_ind,         &comp_energy,         1,                      /*123456789012345*/
      buffer->forces_ind,         &comp_force,          1,                      /*123456789012345*/
      buffer->particleEnergy_ind, &comp_particleEnergy, 1,                      /*123456789012345*/
      buffer->process_dEdr_ind,   &comp_process_dEdr,   1,                      /*123456789012345*/
      buffer->process_d2Edr2_ind, &comp_process_d2Edr2, 1);                     /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "KIM_API_getm_compute_by_index", ier);                 /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  KIM_API_getm_data_by_index(                                                   /*123456789012345*/
      pkim, &ier, 9*3,                                                          /*123456789012345*/
      buffer->cutoff_ind,                 &cutoff,         1,                   /*123456789012345*/
      buffer->numberOfParticles_ind,      &nParts,         1,                   /*123456789012345*/
      buffer->particleSpecies_ind,        &particleSpecies,1,                   /*123456789012345*/
      buffer->coordinates_ind,            &coords,         1,                   /*123456789012345*/
      buffer->numberContribParticles_ind, &numContrib,     (HalfOrFull==1),     /*123456789012345*/
      buffer->boxSideLengths_ind,         &boxSideLengths, (NBC==2),            /*123456789012345*/
      buffer->energy_ind,                 &energy,         comp_energy,         /*123456789012345*/
      buffer->forces_ind,                 &force,          comp_force,          /*123456789012345*/
      buffer->particleEnergy_ind,         &particleEnergy, comp_particleEnergy);/*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);/*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  if (NBC!=3)                                                                   /*123456789012345*/
  {                                                                             /*123456789012345*/
    get_neigh = (get_neigh_ptr)                                                 /*123456789012345*/
        KIM_API_get_method_by_index(pkim, buffer->get_neigh_ind, &ier);         /*123456789012345*/
    if (KIM_STATUS_OK > ier)                                                    /*123456789012345*/
    {                                                                           /*123456789012345*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*123456789012345*/
                           "KIM_API_get_method_by_index", ier);                 /*123456789012345*/
      return ier;                                                               /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  if (HalfOrFull == 1)                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    if (3 != NBC)                                                               /*123456789012345*/
    {                                                                           /*123456789012345*/
      /* non-CLUSTER cases */                                                   /*123456789012345*/
      numberContrib = *numContrib;                                              /*123456789012345*/
    }                                                                           /*123456789012345*/
    else                                                                        /*123456789012345*/
    {                                                                           /*123456789012345*/
      /* CLUSTER cases */                                                       /*123456789012345*/
      numberContrib = *nParts;                                                  /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    /* provide initialization even if not used */                               /*123456789012345*/
    numberContrib = *nParts;                                                    /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* Check to be sure that the species are correct */                           /*123456789012345*/
  /**/                                                                          /*123456789012345*/
  ier = KIM_STATUS_FAIL; /* assume an error */                                  /*123456789012345*/
  for (i = 0; i < *nParts; ++i)                                                 /*123456789012345*/
  {                                                                             /*123456789012345*/
    if ( SPECCODE != particleSpecies[i])                                        /*123456789012345*/
    {                                                                           /*123456789012345*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*123456789012345*/
                           "Unexpected species detected", ier);                 /*123456789012345*/
      return ier;                                                               /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
  ier = KIM_STATUS_OK;  /* everything is ok */                                  /*123456789012345*/
                                                                                /*123456789012345*/
  /* initialize potential energies, forces, and virial term */                  /*123456789012345*/
  if (comp_particleEnergy)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    for (i = 0; i < *nParts; ++i)                                               /*123456789012345*/
    {                                                                           /*123456789012345*/
      particleEnergy[i] = 0.0;                                                  /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
  if (comp_energy)                                                              /*123456789012345*/
  {                                                                             /*123456789012345*/
    *energy = 0.0;                                                              /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  if (comp_force)                                                               /*123456789012345*/
  {                                                                             /*123456789012345*/
    for (i = 0; i < *nParts; ++i)                                               /*123456789012345*/
    {                                                                           /*123456789012345*/
      for (k = 0; k < DIM; ++k)                                                 /*123456789012345*/
      {                                                                         /*123456789012345*/
        force[i*DIM + k] = 0.0;                                                 /*123456789012345*/
      }                                                                         /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* Initialize neighbor handling for CLUSTER NBC */                            /*123456789012345*/
  if (3 == NBC)                                                                 /*123456789012345*/
  {                                                                             /*123456789012345*/
    /* CLUSTER */                                                               /*123456789012345*/
    neighListOfCurrentPart = (int *) malloc((*nParts)*sizeof(int));             /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* Initialize neighbor handling for Iterator mode */                          /*123456789012345*/
                                                                                /*123456789012345*/
  if (1 == IterOrLoca)                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    ier = (*get_neigh)(&pkim, &zero, &zero, &currentPart, &numOfPartNeigh,      /*123456789012345*/
                       &neighListOfCurrentPart, &Rij_list);                     /*123456789012345*/
    /* check for successful initialization */                                   /*123456789012345*/
    if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)                                   /*123456789012345*/
    {                                                                           /*123456789012345*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);       /*123456789012345*/
      ier = KIM_STATUS_FAIL;                                                    /*123456789012345*/
      return ier;                                                               /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* Compute energy and forces */                                               /*123456789012345*/
                                                                                /*123456789012345*/
  /* loop over particles and compute enregy and forces */                       /*123456789012345*/
  i = -1;                                                                       /*123456789012345*/
  while( 1 )                                                                    /*123456789012345*/
  {                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
    /* Set up neighbor list for next part for all NBC methods */                /*123456789012345*/
    if (1 == IterOrLoca)                                                        /*123456789012345*/
    {                                                                           /*123456789012345*/
      /* ITERATOR mode */                                                       /*123456789012345*/
      ier = (*get_neigh)(&pkim, &zero, &one, &currentPart, &numOfPartNeigh,     /*123456789012345*/
                         &neighListOfCurrentPart, &Rij_list);                   /*123456789012345*/
      if (KIM_STATUS_NEIGH_ITER_PAST_END == ier)                                /*123456789012345*/
      {                                                                         /*123456789012345*/
        /* the end of the list, terminate loop */                               /*123456789012345*/
        break;                                                                  /*123456789012345*/
      }                                                                         /*123456789012345*/
      if (KIM_STATUS_OK > ier)                                                  /*123456789012345*/
      {                                                                         /*123456789012345*/
        /* some sort of problem, exit */                                        /*123456789012345*/
        KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);     /*123456789012345*/
        return ier;                                                             /*123456789012345*/
      }                                                                         /*123456789012345*/
                                                                                /*123456789012345*/
      i = currentPart + model_index_shift;                                      /*123456789012345*/
    }                                                                           /*123456789012345*/
    else                                                                        /*123456789012345*/
    {                                                                           /*123456789012345*/
      i++;                                                                      /*123456789012345*/
      if (*nParts <= i)                                                         /*123456789012345*/
      {                                                                         /*123456789012345*/
        /* incremented past end of list, terminate loop */                      /*123456789012345*/
        break;                                                                  /*123456789012345*/
      }                                                                         /*123456789012345*/
                                                                                /*123456789012345*/
      if (3 == NBC)                                                             /*123456789012345*/
      {                                                                         /*123456789012345*/
        /* CLUSTER NBC method */                                                /*123456789012345*/
        numOfPartNeigh = *nParts - (i + 1);                                     /*123456789012345*/
        for (k = 0; k < numOfPartNeigh; ++k)                                    /*123456789012345*/
        {                                                                       /*123456789012345*/
          neighListOfCurrentPart[k] = i + k + 1 - model_index_shift;            /*123456789012345*/
        }                                                                       /*123456789012345*/
        ier = KIM_STATUS_OK;                                                    /*123456789012345*/
      }                                                                         /*123456789012345*/
      else                                                                      /*123456789012345*/
      {                                                                         /*123456789012345*/
        request = i - model_index_shift;                                        /*123456789012345*/
        ier = (*get_neigh)(&pkim, &one, &request,                               /*123456789012345*/
                           &currentPart, &numOfPartNeigh,                       /*123456789012345*/
                           &neighListOfCurrentPart, &Rij_list);                 /*123456789012345*/
        if (KIM_STATUS_OK != ier)                                               /*123456789012345*/
        {                                                                       /*123456789012345*/
          /* some sort of problem, exit */                                      /*123456789012345*/
          KIM_API_report_error(__LINE__, __FILE__, "get_neigh", ier);           /*123456789012345*/
          ier = KIM_STATUS_FAIL;                                                /*123456789012345*/
          return ier;                                                           /*123456789012345*/
        }                                                                       /*123456789012345*/
      }                                                                         /*123456789012345*/
    }                                                                           /*123456789012345*/
                                                                                /*123456789012345*/
    /* loop over the neighbors of particle i */                                 /*123456789012345*/
    for (jj = 0; jj < numOfPartNeigh; ++ jj)                                    /*123456789012345*/
    {                                                                           /*123456789012345*/
      j = neighListOfCurrentPart[jj] + model_index_shift;  /* get neighbor ID *//*123456789012345*/
                                                                                /*123456789012345*/
      /* compute relative position vector and squared distance */               /*123456789012345*/
      Rsqij = 0.0;                                                              /*123456789012345*/
      for (k = 0; k < DIM; ++k)                                                 /*123456789012345*/
      {                                                                         /*123456789012345*/
        if (0 != NBC)                                                           /*123456789012345*/
        {                                                                       /*123456789012345*/
          /* all methods except NEIGH_RVEC */                                   /*123456789012345*/
          Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];                       /*123456789012345*/
        }                                                                       /*123456789012345*/
        else                                                                    /*123456789012345*/
        {                                                                       /*123456789012345*/
          /* NEIGH_RVEC_F method */                                             /*123456789012345*/
          Rij[k] = Rij_list[jj*DIM + k];                                        /*123456789012345*/
        }                                                                       /*123456789012345*/
                                                                                /*123456789012345*/
        /* apply periodic boundary conditions if required */                    /*123456789012345*/
        if (2 == NBC)                                                           /*123456789012345*/
        {                                                                       /*123456789012345*/
          if (abs(Rij[k]) > 0.5*boxSideLengths[k])                              /*123456789012345*/
          {                                                                     /*123456789012345*/
            Rij[k] -= (Rij[k]/fabs(Rij[k]))*boxSideLengths[k];                  /*123456789012345*/
          }                                                                     /*123456789012345*/
        }                                                                       /*123456789012345*/
                                                                                /*123456789012345*/
        /* compute squared distance */                                          /*123456789012345*/
        Rsqij += Rij[k]*Rij[k];                                                 /*123456789012345*/
      }                                                                         /*123456789012345*/
                                                                                /*123456789012345*/
      /* compute energy and force */                                            /*123456789012345*/
      if (Rsqij < *cutsq)                                                       /*123456789012345*/
      {                                                                         /*123456789012345*/
        /* particles are interacting ? */                                       /*123456789012345*/
        R = sqrt(Rsqij);                                                        /*123456789012345*/
        if (comp_process_d2Edr2)                                                /*123456789012345*/
        {                                                                       /*123456789012345*/
          /* compute pair potential and its derivatives */                      /*123456789012345*/
          calc_phi_d2phi(epsilon,                                               /*123456789012345*/
                         C,                                                     /*123456789012345*/
                         Rzero,                                                 /*123456789012345*/
                         shift,                                                 /*123456789012345*/
                         cutoff, R, &phi, &dphi, &d2phi);                       /*123456789012345*/
                                                                                /*123456789012345*/
          /* compute dEidr */                                                   /*123456789012345*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*123456789012345*/
          {                                                                     /*123456789012345*/
            /* Half mode -- double contribution */                              /*123456789012345*/
            dEidr = dphi;                                                       /*123456789012345*/
            d2Eidr = d2phi;                                                     /*123456789012345*/
          }                                                                     /*123456789012345*/
          else                                                                  /*123456789012345*/
          {                                                                     /*123456789012345*/
            /* Full mode -- regular contribution */                             /*123456789012345*/
            dEidr = 0.5*dphi;                                                   /*123456789012345*/
            d2Eidr = 0.5*d2phi;                                                 /*123456789012345*/
          }                                                                     /*123456789012345*/
        }                                                                       /*123456789012345*/
        else if (comp_force || comp_process_dEdr)                               /*123456789012345*/
        {                                                                       /*123456789012345*/
          /* compute pair potential and its derivative */                       /*123456789012345*/
          calc_phi_dphi(epsilon,                                                /*123456789012345*/
                        C,                                                      /*123456789012345*/
                        Rzero,                                                  /*123456789012345*/
                        shift,                                                  /*123456789012345*/
                        cutoff, R, &phi, &dphi);                                /*123456789012345*/
                                                                                /*123456789012345*/
          /* compute dEidr */                                                   /*123456789012345*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*123456789012345*/
          {                                                                     /*123456789012345*/
            /* Half mode -- double contribution */                              /*123456789012345*/
            dEidr = dphi;                                                       /*123456789012345*/
          }                                                                     /*123456789012345*/
          else                                                                  /*123456789012345*/
          {                                                                     /*123456789012345*/
            /* Full mode -- regular contribution */                             /*123456789012345*/
            dEidr = 0.5*dphi;                                                   /*123456789012345*/
          }                                                                     /*123456789012345*/
        }                                                                       /*123456789012345*/
        else                                                                    /*123456789012345*/
        {                                                                       /*123456789012345*/
          /* compute just pair potential */                                     /*123456789012345*/
          calc_phi(epsilon,                                                     /*123456789012345*/
                   C,                                                           /*123456789012345*/
                   Rzero,                                                       /*123456789012345*/
                   shift,                                                       /*123456789012345*/
                   cutoff, R, &phi);                                            /*123456789012345*/
        }                                                                       /*123456789012345*/
                                                                                /*123456789012345*/
        /* contribution to energy */                                            /*123456789012345*/
        if (comp_particleEnergy)                                                /*123456789012345*/
        {                                                                       /*123456789012345*/
          particleEnergy[i] += 0.5*phi;                                         /*123456789012345*/
          /* if half list add energy for the other particle in the pair */      /*123456789012345*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*123456789012345*/
          {                                                                     /*123456789012345*/
            particleEnergy[j] += 0.5*phi;                                       /*123456789012345*/
          }                                                                     /*123456789012345*/
        }                                                                       /*123456789012345*/
        if (comp_energy)                                                        /*123456789012345*/
        {                                                                       /*123456789012345*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*123456789012345*/
          {                                                                     /*123456789012345*/
            /* Half mode -- add v to total energy */                            /*123456789012345*/
            *energy += phi;                                                     /*123456789012345*/
          }                                                                     /*123456789012345*/
          else                                                                  /*123456789012345*/
          {                                                                     /*123456789012345*/
            /* Full mode -- add half v to total energy */                       /*123456789012345*/
            *energy += 0.5*phi;                                                 /*123456789012345*/
          }                                                                     /*123456789012345*/
        }                                                                       /*123456789012345*/
                                                                                /*123456789012345*/
        /* contribution to process_dEdr */                                      /*123456789012345*/
        if (comp_process_dEdr)                                                  /*123456789012345*/
        {                                                                       /*123456789012345*/
          ier = KIM_API_process_dEdr(km, &dEidr, &R, &pRij, &i, &j);            /*123456789012345*/
        }                                                                       /*123456789012345*/
                                                                                /*123456789012345*/
        /* contribution to process_d2Edr2 */                                    /*123456789012345*/
        if (comp_process_d2Edr2)                                                /*123456789012345*/
        {                                                                       /*123456789012345*/
          R_pairs[0] = R_pairs[1] = R;                                          /*123456789012345*/
          Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];                           /*123456789012345*/
          Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];                           /*123456789012345*/
          Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];                           /*123456789012345*/
          i_pairs[0] = i_pairs[1] = i;                                          /*123456789012345*/
          j_pairs[0] = j_pairs[1] = j;                                          /*123456789012345*/
                                                                                /*123456789012345*/
          ier = KIM_API_process_d2Edr2(km, &d2Eidr, &pR_pairs, &pRij_pairs,     /*123456789012345*/
                                       &pi_pairs, &pj_pairs);                   /*123456789012345*/
        }                                                                       /*123456789012345*/
                                                                                /*123456789012345*/
        /* contribution to forces */                                            /*123456789012345*/
        if (comp_force)                                                         /*123456789012345*/
        {                                                                       /*123456789012345*/
          for (k = 0; k < DIM; ++k)                                             /*123456789012345*/
          {                                                                     /*123456789012345*/
            force[i*DIM + k] += dEidr*Rij[k]/R;  /* accumulate force on i */    /*123456789012345*/
            force[j*DIM + k] -= dEidr*Rij[k]/R;  /* accumulate force on j */    /*123456789012345*/
          }                                                                     /*123456789012345*/
        }                                                                       /*123456789012345*/
      }                                                                         /*123456789012345*/
    }  /* loop on jj */                                                         /*123456789012345*/
  }  /* infinite while loop (terminated by break statements above */            /*123456789012345*/
                                                                                /*123456789012345*/
  /* Free temporary storage */                                                  /*123456789012345*/
  if (3 == NBC)                                                                 /*123456789012345*/
  {                                                                             /*123456789012345*/
    free(neighListOfCurrentPart);                                               /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* everything is great */                                                     /*123456789012345*/
  ier = KIM_STATUS_OK;                                                          /*123456789012345*/
                                                                                /*123456789012345*/
  return ier;                                                                   /*123456789012345*/
}                                                                               /*123456789012345*/
                                                                                /*123456789012345*/
/* Initialization function */                                                   /*123456789012345*/
int model_driver_init(void *km, char* paramfile_names, int* nmstrlen,           /*123456789012345*/
                      int* numparamfiles)                                       /*123456789012345*/
{                                                                               /*123456789012345*/
  /* KIM variables */                                                           /*123456789012345*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*123456789012345*/
  char* paramfile1name;                                                         /*123456789012345*/
                                                                                /*123456789012345*/
  /* Local variables */                                                         /*123456789012345*/
  FILE* fid;                                                                    /*123456789012345*/
  double cutoff;                                                                /*123456789012345*/
  double epsilon;                                                               /*123456789012345*/
  double C;                                                                     /*123456789012345*/
  double Rzero;                                                                 /*123456789012345*/
  double* model_cutoff;                                                         /*123456789012345*/
  int ier;                                                                      /*123456789012345*/
  double dummy;                                                                 /*123456789012345*/
  struct model_buffer* buffer;                                                  /*123456789012345*/
  const char* NBCstr;                                                           /*123456789012345*/
                                                                                /*123456789012345*/
  /* set paramfile1name */                                                      /*123456789012345*/
  if (*numparamfiles != 1)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    ier = KIM_STATUS_FAIL;                                                      /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "Incorrect number of parameter files.", ier);          /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  paramfile1name = paramfile_names;                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* store pointer to functions in KIM object */                                /*123456789012345*/
  KIM_API_setm_method(pkim, &ier, 3*4,                                          /*123456789012345*/
                      "compute", 1, &compute, 1,                                /*123456789012345*/
                      "reinit",  1, &reinit,  1,                                /*123456789012345*/
                      "destroy", 1, &destroy, 1);                               /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);         /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* Read in model parameters from parameter file */                            /*123456789012345*/
  fid = fopen(paramfile1name, "r");                                             /*123456789012345*/
  if (fid == NULL)                                                              /*123456789012345*/
  {                                                                             /*123456789012345*/
    ier = KIM_STATUS_FAIL;                                                      /*123456789012345*/
    KIM_API_report_error(                                                       /*123456789012345*/
        __LINE__, __FILE__,                                                     /*123456789012345*/
        "Unable to open parameter file for Morse parameters", ier);             /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  ier = fscanf(fid, "%lf \n%lf \n%lf \n%lf",                                    /*123456789012345*/
               &cutoff,  /* cutoff distance in angstroms */                     /*123456789012345*/
               &epsilon,  /* Morse epsilon in eV */                             /*123456789012345*/
               &C,  /* Morse C in 1/Angstroms */                                /*123456789012345*/
               &Rzero);  /* Morse Rzero in Angstroms */                         /*123456789012345*/
  fclose(fid);                                                                  /*123456789012345*/
                                                                                /*123456789012345*/
  /* check that we read the right number of parameters */                       /*123456789012345*/
  if (4 != ier)                                                                 /*123456789012345*/
  {                                                                             /*123456789012345*/
    ier = KIM_STATUS_FAIL;                                                      /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "Unable to read all Morse parameters", ier);           /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* convert to appropriate units */                                            /*123456789012345*/
  cutoff *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",        /*123456789012345*/
                                        1.0, 0.0,  0.0, 0.0, 0.0, &ier);        /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "KIM_API_convert_to_act_unit", ier);                   /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  epsilon *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",       /*123456789012345*/
                                         0.0, 1.0,  0.0, 0.0, 0.0, &ier);       /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "KIM_API_convert_to_act_unit", ier);                   /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  C *= KIM_API_convert_to_act_unit(pkim, "A",  "eV", "e", "K", "ps",            /*123456789012345*/
                                   -1.0, 0.0,  0.0, 0.0, 0.0, &ier);            /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "KIM_API_convert_to_act_unit", ier);                   /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  Rzero *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",         /*123456789012345*/
                                       1.0, 0.0,  0.0, 0.0, 0.0, &ier);         /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*123456789012345*/
                         "KIM_API_convert_to_act_unit", ier);                   /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* store model cutoff in KIM object */                                        /*123456789012345*/
  model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);              /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);          /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  *model_cutoff = cutoff;                                                       /*123456789012345*/
                                                                                /*123456789012345*/
  /* allocate buffer */                                                         /*123456789012345*/
  buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));          /*123456789012345*/
  if (NULL == buffer)                                                           /*123456789012345*/
  {                                                                             /*123456789012345*/
    ier = KIM_STATUS_FAIL;                                                      /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* setup buffer */                                                            /*123456789012345*/
  /* set value of parameters */                                                 /*123456789012345*/
  buffer->Pcutoff = *model_cutoff;                                              /*123456789012345*/
  buffer->cutsq = (*model_cutoff)*(*model_cutoff);                              /*123456789012345*/
  buffer->epsilon = epsilon;                                                    /*123456789012345*/
  buffer->C = C;                                                                /*123456789012345*/
  buffer->Rzero = Rzero;                                                        /*123456789012345*/
  /* set value of parameter shift */                                            /*123456789012345*/
  dummy = 0.0;                                                                  /*123456789012345*/
  /* call calc_phi with r=cutoff and shift=0.0 */                               /*123456789012345*/
  calc_phi(&(buffer->epsilon),                                                  /*123456789012345*/
           &(buffer->C),                                                        /*123456789012345*/
           &(buffer->Rzero),                                                    /*123456789012345*/
           &dummy,                                                              /*123456789012345*/
           model_cutoff, *model_cutoff, &(buffer->shift));                      /*123456789012345*/
  /* set shift to -shift */                                                     /*123456789012345*/
  buffer->shift = -buffer->shift;                                               /*123456789012345*/
                                                                                /*123456789012345*/
  /* Determine neighbor list boundary condition (NBC) */                        /*123456789012345*/
  ier = KIM_API_get_NBC_method(pkim, &NBCstr);                                  /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);    /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  if ((!strcmp("NEIGH_RVEC_H",NBCstr)) || (!strcmp("NEIGH_RVEC_F",NBCstr)))     /*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->NBC = 0;                                                            /*123456789012345*/
  }                                                                             /*123456789012345*/
  else if ((!strcmp("NEIGH_PURE_H",NBCstr)) || (!strcmp("NEIGH_PURE_F",NBCstr)))/*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->NBC = 1;                                                            /*123456789012345*/
  }                                                                             /*123456789012345*/
  else if ((!strcmp("MI_OPBC_H",NBCstr)) || (!strcmp("MI_OPBC_F",NBCstr)))      /*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->NBC = 2;                                                            /*123456789012345*/
  }                                                                             /*123456789012345*/
  else if (!strcmp("CLUSTER",NBCstr))                                           /*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->NBC = 3;                                                            /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    ier = KIM_STATUS_FAIL;                                                      /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);        /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* Determine if Half or Full neighbor lists are being used */                 /*123456789012345*/
  /*****************************/                                               /*123456789012345*/
  /* HalfOrFull = 1 -- Half    */                                               /*123456789012345*/
  /*            = 2 -- Full    */                                               /*123456789012345*/
  /*****************************/                                               /*123456789012345*/
  if (KIM_API_is_half_neighbors(pkim, &ier))                                    /*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->HalfOrFull = 1;                                                     /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->HalfOrFull = 2;                                                     /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* determine neighbor list handling mode */                                   /*123456789012345*/
  if (buffer->NBC != 3)                                                         /*123456789012345*/
  {                                                                             /*123456789012345*/
    /******************************/                                            /*123456789012345*/
    /* IterOrLoca = 1 -- Iterator */                                            /*123456789012345*/
    /*            = 2 -- Locator  */                                            /*123456789012345*/
    /******************************/                                            /*123456789012345*/
    buffer->IterOrLoca = KIM_API_get_neigh_mode(pkim, &ier);                    /*123456789012345*/
    if (KIM_STATUS_OK > ier)                                                    /*123456789012345*/
    {                                                                           /*123456789012345*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh_mode", ier);  /*123456789012345*/
      return ier;                                                               /*123456789012345*/
    }                                                                           /*123456789012345*/
    if ((buffer->IterOrLoca != 1) && (buffer->IterOrLoca != 2))                 /*123456789012345*/
    {                                                                           /*123456789012345*/
      ier = KIM_STATUS_FAIL;                                                    /*123456789012345*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*123456789012345*/
                           "Unsupported IterOrLoca mode", ier);                 /*123456789012345*/
      return ier;                                                               /*123456789012345*/
    }                                                                           /*123456789012345*/
  }                                                                             /*123456789012345*/
  else                                                                          /*123456789012345*/
  {                                                                             /*123456789012345*/
    buffer->IterOrLoca = 2;  /* for CLUSTER NBC */                              /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  buffer->model_index_shift = KIM_API_get_model_index_shift(pkim);              /*123456789012345*/
                                                                                /*123456789012345*/
  KIM_API_getm_index(                                                           /*123456789012345*/
      pkim, &ier, 12*3,                                                         /*123456789012345*/
      "cutoff",                      &(buffer->cutoff_ind),                 1,  /*123456789012345*/
      "numberOfParticles",           &(buffer->numberOfParticles_ind),      1,  /*123456789012345*/
      "particleSpecies",             &(buffer->particleSpecies_ind),        1,  /*123456789012345*/
      "numberContributingParticles", &(buffer->numberContribParticles_ind), 1,  /*123456789012345*/
      "coordinates",                 &(buffer->coordinates_ind),            1,  /*123456789012345*/
      "get_neigh",                   &(buffer->get_neigh_ind),              1,  /*123456789012345*/
      "boxSideLengths",              &(buffer->boxSideLengths_ind),         1,  /*123456789012345*/
      "energy",                      &(buffer->energy_ind),                 1,  /*123456789012345*/
      "forces",                      &(buffer->forces_ind),                 1,  /*123456789012345*/
      "particleEnergy",              &(buffer->particleEnergy_ind),         1,  /*123456789012345*/
      "process_dEdr",                &(buffer->process_dEdr_ind),           1,  /*123456789012345*/
      "process_d2Edr2",              &(buffer->process_d2Edr2_ind),         1   /*123456789012345*/
                     );                                                         /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_index", ier);        /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  /* end setup buffer */                                                        /*123456789012345*/
                                                                                /*123456789012345*/
  /* store in model buffer */                                                   /*123456789012345*/
  KIM_API_set_model_buffer(pkim, (void*) buffer, &ier);                         /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_model_buffer", ier);  /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* set pointers to parameters in KIM object */                                /*123456789012345*/
  KIM_API_setm_data(pkim, &ier, 6*4,                                            /*123456789012345*/
                    "PARAM_FREE_cutoff",  1, &(buffer->Pcutoff), 1,             /*123456789012345*/
                    "PARAM_FIXED_cutsq",  1, &(buffer->cutsq),   1,             /*123456789012345*/
                    "PARAM_FREE_epsilon", 1, &(buffer->epsilon), 1,             /*123456789012345*/
                    "PARAM_FREE_C",       1, &(buffer->C),       1,             /*123456789012345*/
                    "PARAM_FREE_Rzero",   1, &(buffer->Rzero),   1,             /*123456789012345*/
                    "PARAM_FIXED_shift",  1, &(buffer->shift),   1              /*123456789012345*/
                    );                                                          /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);         /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  return KIM_STATUS_OK;                                                         /*123456789012345*/
}                                                                               /*123456789012345*/
                                                                                /*123456789012345*/
/* Reinitialization function */                                                 /*123456789012345*/
static int reinit(void *km)                                                     /*123456789012345*/
{                                                                               /*123456789012345*/
  /* Local variables */                                                         /*123456789012345*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*123456789012345*/
  int ier;                                                                      /*123456789012345*/
  double *cutoff;                                                               /*123456789012345*/
  double dummy;                                                                 /*123456789012345*/
  struct model_buffer* buffer;                                                  /*123456789012345*/
                                                                                /*123456789012345*/
  /* get buffer from KIM object */                                              /*123456789012345*/
  buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);         /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);  /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* set new values in KIM object     */                                        /*123456789012345*/
  /*                                  */                                        /*123456789012345*/
  /* store model cutoff in KIM object */                                        /*123456789012345*/
  cutoff = KIM_API_get_data(pkim, "cutoff", &ier);                              /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);          /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
  *cutoff = buffer->Pcutoff;                                                    /*123456789012345*/
                                                                                /*123456789012345*/
  /* set value of parameter cutsq */                                            /*123456789012345*/
  buffer->cutsq = (*cutoff)*(*cutoff);                                          /*123456789012345*/
                                                                                /*123456789012345*/
  /* set value of parameter shift */                                            /*123456789012345*/
  dummy = 0.0;                                                                  /*123456789012345*/
  /* call calc_phi with r=cutoff and shift=0.0 */                               /*123456789012345*/
  calc_phi(&(buffer->epsilon),                                                  /*123456789012345*/
           &(buffer->C),                                                        /*123456789012345*/
           &(buffer->Rzero),                                                    /*123456789012345*/
           &dummy,                                                              /*123456789012345*/
           cutoff, *cutoff, &(buffer->shift));                                  /*123456789012345*/
  /* set shift to -shift */                                                     /*123456789012345*/
  buffer->shift = -buffer->shift;                                               /*123456789012345*/
                                                                                /*123456789012345*/
  ier = KIM_STATUS_OK;                                                          /*123456789012345*/
  return ier;                                                                   /*123456789012345*/
}                                                                               /*123456789012345*/
                                                                                /*123456789012345*/
/* destroy function */                                                          /*123456789012345*/
static int destroy(void *km)                                                    /*123456789012345*/
{                                                                               /*123456789012345*/
  /* Local variables */                                                         /*123456789012345*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*123456789012345*/
  struct model_buffer* buffer;                                                  /*123456789012345*/
  int ier;                                                                      /*123456789012345*/
                                                                                /*123456789012345*/
  /* get model buffer from KIM object */                                        /*123456789012345*/
  buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);         /*123456789012345*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789012345*/
  {                                                                             /*123456789012345*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);  /*123456789012345*/
    return ier;                                                                 /*123456789012345*/
  }                                                                             /*123456789012345*/
                                                                                /*123456789012345*/
  /* destroy the buffer */                                                      /*123456789012345*/
  free(buffer);                                                                 /*123456789012345*/
                                                                                /*123456789012345*/
  ier = KIM_STATUS_OK;                                                          /*123456789012345*/
  return ier;                                                                   /*123456789012345*/
}                                                                               /*123456789012345*/
