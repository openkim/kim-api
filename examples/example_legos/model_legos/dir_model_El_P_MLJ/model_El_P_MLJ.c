/* Markup tag documentation:                                                  *//*..*/
/*    1 - ex_model_Ar_P_MLJ_C.c                                               *//*..*/
/*    2 - model_El_P_Template.c                                               *//*..*/
/*                                                                            *//*..*/
/*                                                                            *//*12*/
/* CDDL HEADER START                                                          *//*12*/
/*                                                                            *//*12*/
/* The contents of this file are subject to the terms of the Common           *//*12*/
/* Development and Distribution License Version 1.0 (the "License").          *//*12*/
/*                                                                            *//*12*/
/* You can obtain a copy of the license at                                    *//*12*/
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      *//*12*/
/* specific language governing permissions and limitations under the License. *//*12*/
/*                                                                            *//*12*/
/* When distributing Covered Code, include this CDDL HEADER in each file and  *//*12*/
/* include the License file in a prominent location with the name             *//*12*/
/* LICENSE.CDDL. If applicable, add the following below this CDDL HEADER,     *//*12*/
/* with the fields enclosed by brackets "[]" replaced with your own           *//*12*/
/* identifying information:                                                   *//*12*/
/*                                                                            *//*12*/
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   *//*12*/
/* All rights reserved.                                                       *//*12*/
/*                                                                            *//*12*/
/* CDDL HEADER END                                                            *//*12*/
/*                                                                            *//*12*/
                                                                                /*12*/
/*                                                                            *//*12*/
/* Copyright (c) 2013--2016, Regents of the University of Minnesota.          *//*12*/
/* All rights reserved.                                                       *//*12*/
/*                                                                            *//*12*/
/* Contributors:                                                              *//*12*/
/*    Ryan S. Elliott                                                         *//*12*/
/*    Ellad B. Tadmor                                                         *//*12*/
/*    Valeriu Smirichinski                                                    *//*12*/
/*    Stephen M. Whalen                                                       *//*12*/
/*                                                                            *//*12*/
                                                                                /*.2*/
/*                                                                            *//*.2*/
/* Portions Copyright (c) <FILL_year>, <FILL_copyright_holder>.               *//*.2*/
/* All rights reserved.                                                       *//*.2*/
/*                                                                            *//*.2*/
/* Contributors:                                                              *//*.2*/
/*    <FILL_name>                                                             *//*.2*/
/*                                                                            *//*.2*/
                                                                                /*12*/
/******************************************************************************//*12*/
/*                                                                            *//*12*/
/*  <FILL_model_name>                                                         *//*12*/
/*                                                                            *//*12*/
/*  Modified Lennard-Jones pair potential (with smooth cutoff) Model for Ar   *//*1.*/
/*  <FILL_model_class_name> pair potential model for <FILL_element_name>      *//*.2*/
/*                                                                            *//*12*/
/*  Reference: <FILL_model_reference>                                         *//*12*/
/*                                                                            *//*12*/
/*  Language: C                                                               *//*12*/
/*                                                                            *//*12*/
/*  Release: This file is part of the kim-api.git repository.                 *//*12*/
/*                                                                            *//*12*/
/******************************************************************************//*12*/
                                                                                /*12*/
                                                                                /*12*/
#include <stdlib.h>                                                             /*12*/
#include <stdio.h>                                                              /*12*/
#include <string.h>                                                             /*12*/
#include <math.h>                                                               /*12*/
#include "KIM_API_C.h"                                                          /*12*/
#include "KIM_API_status.h"                                                     /*12*/
                                                                                /*12*/
/******************************************************************************//*12*/
/* Below are the definitions and values of all Model parameters               *//*12*/
/******************************************************************************//*12*/
#define DIM 3       /* dimensionality of space */                               /*12*/
#define SPECCODE 1  /* internal species code */                                 /*12*/
#define MODEL_CUTOFF 8.15 /* cutoff radius in angstroms */                      /*1.*/
#define MODEL_CUTOFF <FILL_cutoff> /* cutoff radius in angstroms */             /*.2*/
#define MODEL_CUTSQ  (MODEL_CUTOFF * MODEL_CUTOFF)                              /*12*/
#define SIGMA 3.40                                                              /*1.*/
#define EPSILON 0.0104                                                          /*1.*/
#define CUTNORM (MODEL_CUTOFF/SIGMA)                                            /*1.*/
#define CUTNORM3 (CUTNORM*CUTNORM*CUTNORM)                                      /*1.*/
#define CUTNORM6 (CUTNORM3*CUTNORM3)                                            /*1.*/
#define CUTNORM14 (CUTNORM6*CUTNORM6*CUTNORM*CUTNORM)                           /*1.*/
#define APARAM (12.0*EPSILON*(-26.0 + 7.0*CUTNORM6)/(CUTNORM14*SIGMA*SIGMA))    /*1.*/
#define BPARAM (96.0*EPSILON*(7.0 - 2.0*CUTNORM6)/(CUTNORM6*CUTNORM6*CUTNORM*SIGMA))/*1.*/
#define CPARAM (28.0*EPSILON*(-13.0 + 4.0*CUTNORM6)/(CUTNORM6*CUTNORM6))        /*1.*/
#define <FILL_aprameter_name> <FILL_parameter_value>                            /*.2*/
                                                                                /*12*/
                                                                                /*12*/
/* Define prototypes for model init */                                          /*12*/
/**/                                                                            /*12*/
int model_init(void* km);                                                       /*12*/
                                                                                /*12*/
/* Define prototypes for model reinit, compute, and destroy */                  /*12*/
/* defined as static to avoid namespace clashes with other Models */            /*12*/
/**/                                                                            /*12*/
static int compute(void* km);                                                   /*12*/
/**/                                                                            /*12*/
static void calc_phi(double r, double* phi);                                    /*12*/
static void calc_phi_dphi(double r, double* phi, double* dphi);                 /*12*/
                                                                                /*12*/
                                                                                /*12*/
/* Calculate pair potential phi(r) */                                           /*12*/
static void calc_phi(double r, double* phi)                                     /*12*/
{                                                                               /*12*/
  /* local variables */                                                         /*12*/
  double rsq   = r*r;                                                           /*1.*/
  double sor   = SIGMA/r;                                                       /*1.*/
  double sor6  = sor*sor*sor*sor*sor*sor;                                       /*1.*/
  double sor12 = sor6*sor6;                                                     /*1.*/
  /* <FILL_any_local_variable_definitions_here> */                              /*.2*/
                                                                                /*12*/
  if (r > MODEL_CUTOFF)                                                         /*12*/
  {                                                                             /*12*/
    /* Argument exceeds cutoff radius */                                        /*12*/
    *phi = 0.0;                                                                 /*12*/
  }                                                                             /*12*/
  else                                                                          /*12*/
  {                                                                             /*12*/
    *phi = 4.0*EPSILON*(sor12 - sor6) + APARAM*rsq + BPARAM*r + CPARAM;         /*1.*/
    *phi = <FILL_functional_form_of_phi(r)>;                                    /*.2*/
  }                                                                             /*12*/
                                                                                /*12*/
  return;                                                                       /*12*/
}                                                                               /*12*/
                                                                                /*12*/
/* Calculate pair potential phi(r) and its derivative dphi(r) */                /*12*/
static void calc_phi_dphi(double r, double* phi, double* dphi)                  /*12*/
{                                                                               /*12*/
  /* local variables */                                                         /*12*/
  double rsq   = r*r;                                                           /*1.*/
  double sor   = SIGMA/r;                                                       /*1.*/
  double sor6  = sor*sor*sor*sor*sor*sor;                                       /*1.*/
  double sor12 = sor6*sor6;                                                     /*1.*/
  /* <FILL_any_local_variable_definitions_here> */                              /*.2*/
                                                                                /*12*/
  if (r > MODEL_CUTOFF)                                                         /*12*/
  {                                                                             /*12*/
    /* Argument exceeds cutoff radius */                                        /*12*/
    *phi  = 0.0;                                                                /*12*/
    *dphi = 0.0;                                                                /*12*/
  }                                                                             /*12*/
  else                                                                          /*12*/
  {                                                                             /*12*/
    *phi  = 4.0*EPSILON*(sor12 - sor6) + APARAM*rsq + BPARAM*r + CPARAM;        /*1.*/
    *dphi = 24.0*EPSILON*(-2.0*sor12 + sor6)/r + 2.0*APARAM*r + BPARAM;         /*1.*/
    *phi  = <FILL_functional_form_of_phi(r)>;                                   /*.2*/
    *dphi = <FILL_functional_form_of_dphi(r)>;                                  /*.2*/
  }                                                                             /*12*/
                                                                                /*12*/
  return;                                                                       /*12*/
}                                                                               /*12*/
                                                                                /*12*/
/* compute function */                                                          /*12*/
static int compute(void* km)                                                    /*12*/
{                                                                               /*12*/
  /* local variables */                                                         /*12*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*12*/
  double R;                                                                     /*12*/
  double Rsqij;                                                                 /*12*/
  double phi;                                                                   /*12*/
  double dphi;                                                                  /*12*/
  double dEidr = 0.0;                                                           /*12*/
  double Rij[DIM];                                                              /*12*/
  int ier;                                                                      /*12*/
  int i;                                                                        /*12*/
  int j;                                                                        /*12*/
  int jj;                                                                       /*12*/
  int k;                                                                        /*12*/
  int numOfPartNeigh;                                                           /*12*/
  int currentPart;                                                              /*12*/
  int comp_energy;                                                              /*12*/
  int comp_force;                                                               /*12*/
  int comp_particleEnergy;                                                      /*12*/
  int comp_virial;                                                              /*12*/
  int IterOrLoca;                                                               /*12*/
  int HalfOrFull;                                                               /*12*/
  int NBC;                                                                      /*12*/
  const char* NBCstr;                                                           /*12*/
  int numberContrib;                                                            /*12*/
                                                                                /*12*/
  int* nParts;                                                                  /*12*/
  int* particleSpecies;                                                         /*12*/
  double* Rij_list;                                                             /*12*/
  double* coords;                                                               /*12*/
  double* energy;                                                               /*12*/
  double* force;                                                                /*12*/
  double* particleEnergy;                                                       /*12*/
  double* virial;                                                               /*12*/
  int* neighListOfCurrentPart;                                                  /*12*/
  double* boxSideLengths;                                                       /*12*/
  int* numContrib;                                                              /*12*/
                                                                                /*12*/
                                                                                /*12*/
  /* Determine neighbor list boundary condition (NBC) */                        /*12*/
  /* and half versus full mode: */                                              /*12*/
  /*****************************                                                /*12*/
   * HalfOrFull = 1 -- Half                                                     /*12*/
   *            = 2 -- Full                                                     /*12*/
   *****************************/                                               /*12*/
  ier = KIM_API_get_NBC_method(pkim, &NBCstr);                                  /*12*/
  if (KIM_STATUS_OK > ier)                                                      /*12*/
  {                                                                             /*12*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);    /*12*/
    return ier;                                                                 /*12*/
  }                                                                             /*12*/
  if (!strcmp("NEIGH_RVEC_H",NBCstr))                                           /*12*/
  {                                                                             /*12*/
    NBC = 0;                                                                    /*12*/
    HalfOrFull = 1;                                                             /*12*/
  }                                                                             /*12*/
  else if (!strcmp("NEIGH_PURE_H",NBCstr))                                      /*12*/
  {                                                                             /*12*/
    NBC = 1;                                                                    /*12*/
    HalfOrFull = 1;                                                             /*12*/
  }                                                                             /*12*/
  else if (!strcmp("NEIGH_RVEC_F",NBCstr))                                      /*12*/
  {                                                                             /*12*/
    NBC = 0;                                                                    /*12*/
    HalfOrFull = 2;                                                             /*12*/
  }                                                                             /*12*/
  else if (!strcmp("NEIGH_PURE_F",NBCstr))                                      /*12*/
  {                                                                             /*12*/
    NBC = 1;                                                                    /*12*/
    HalfOrFull = 2;                                                             /*12*/
  }                                                                             /*12*/
  else if (!strcmp("MI_OPBC_H",NBCstr))                                         /*12*/
  {                                                                             /*12*/
    NBC = 2;                                                                    /*12*/
    HalfOrFull = 1;                                                             /*12*/
  }                                                                             /*12*/
  else if (!strcmp("MI_OPBC_F",NBCstr))                                         /*12*/
  {                                                                             /*12*/
    NBC = 2;                                                                    /*12*/
    HalfOrFull = 2;                                                             /*12*/
  }                                                                             /*12*/
  else if (!strcmp("CLUSTER",NBCstr))                                           /*12*/
  {                                                                             /*12*/
    NBC = 3;                                                                    /*12*/
    HalfOrFull = 1;                                                             /*12*/
  }                                                                             /*12*/
  else                                                                          /*12*/
  {                                                                             /*12*/
    ier = KIM_STATUS_FAIL;                                                      /*12*/
    KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);        /*12*/
    return ier;                                                                 /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* determine neighbor list handling mode */                                   /*12*/
  if (NBC != 3)                                                                 /*12*/
  {                                                                             /*12*/
    /*****************************                                              /*12*/
     * IterOrLoca = 1 -- Iterator                                               /*12*/
     *            = 2 -- Locator                                                /*12*/
     *****************************/                                             /*12*/
    IterOrLoca = KIM_API_get_neigh_mode(pkim, &ier);                            /*12*/
    if (KIM_STATUS_OK > ier)                                                    /*12*/
    {                                                                           /*12*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh_mode", ier);  /*12*/
      return ier;                                                               /*12*/
    }                                                                           /*12*/
    if ((IterOrLoca != 1) && (IterOrLoca != 2))                                 /*12*/
    {                                                                           /*12*/
      printf("* ERROR: Unsupported IterOrLoca mode = %i\n", IterOrLoca);        /*12*/
      return KIM_STATUS_FAIL;                                                   /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
  else                                                                          /*12*/
  {                                                                             /*12*/
    IterOrLoca = 2;   /* for CLUSTER NBC */                                     /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* check to see if we have been asked to compute the forces, */               /*12*/
  /* particleEnergy, energy and virial */                                       /*12*/
  KIM_API_getm_compute(pkim, &ier, 4*3,                                         /*12*/
                       "energy",         &comp_energy,         1,               /*12*/
                       "forces",         &comp_force,          1,               /*12*/
                       "particleEnergy", &comp_particleEnergy, 1,               /*12*/
                       "virial",         &comp_virial,         1);              /*12*/
  if (KIM_STATUS_OK > ier)                                                      /*12*/
  {                                                                             /*12*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute", ier);      /*12*/
    return ier;                                                                 /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* unpack data from KIM object */                                             /*12*/
  KIM_API_getm_data(                                                            /*12*/
      pkim, &ier, 9*3,                                                          /*12*/
      "numberOfParticles",           &nParts,         1,                        /*12*/
      "particleSpecies",             &particleSpecies,1,                        /*12*/
      "coordinates",                 &coords,         1,                        /*12*/
      "numberContributingParticles", &numContrib,     (HalfOrFull==1),          /*12*/
      "boxSideLengths",              &boxSideLengths, (NBC==2),                 /*12*/
      "energy",                      &energy,         (comp_energy==1),         /*12*/
      "forces",                      &force,          (comp_force==1),          /*12*/
      "particleEnergy",              &particleEnergy, (comp_particleEnergy==1), /*12*/
      "virial",                      &virial,         (comp_virial==1));        /*12*/
  if (KIM_STATUS_OK > ier)                                                      /*12*/
  {                                                                             /*12*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);         /*12*/
    return ier;                                                                 /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  if (HalfOrFull == 1)                                                          /*12*/
  {                                                                             /*12*/
    if (3 != NBC) /* non-CLUSTER cases */                                       /*12*/
    {                                                                           /*12*/
      numberContrib = *numContrib;                                              /*12*/
    }                                                                           /*12*/
    else                                                                        /*12*/
    {                                                                           /*12*/
      numberContrib = *nParts;                                                  /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
  else                                                                          /*12*/
  { /* provide initialization even if not used */                               /*12*/
    numberContrib = *nParts;                                                    /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* Check to be sure that the species are correct */                           /*12*/
  /**/                                                                          /*12*/
  ier = KIM_STATUS_FAIL; /* assume an error */                                  /*12*/
  for (i = 0; i < *nParts; ++i)                                                 /*12*/
  {                                                                             /*12*/
    if ( SPECCODE != particleSpecies[i])                                        /*12*/
    {                                                                           /*12*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*12*/
                           "Unexpected species detected", ier);                 /*12*/
      return ier;                                                               /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
  ier = KIM_STATUS_OK; /* everything is ok */                                   /*12*/
                                                                                /*12*/
  /* initialize potential energies, forces, and virial term */                  /*12*/
  if (comp_particleEnergy)                                                      /*12*/
  {                                                                             /*12*/
    for (i = 0; i < *nParts; ++i)                                               /*12*/
    {                                                                           /*12*/
      particleEnergy[i] = 0.0;                                                  /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
  if (comp_energy)                                                              /*12*/
  {                                                                             /*12*/
    *energy = 0.0;                                                              /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  if (comp_force)                                                               /*12*/
  {                                                                             /*12*/
    for (i = 0; i < *nParts; ++i)                                               /*12*/
    {                                                                           /*12*/
      for (k = 0; k < DIM; ++k)                                                 /*12*/
      {                                                                         /*12*/
        force[i*DIM + k] = 0.0;                                                 /*12*/
      }                                                                         /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  if (comp_virial)                                                              /*12*/
  {                                                                             /*12*/
    for (i = 0; i < 6; ++i)                                                     /*12*/
    {                                                                           /*12*/
      virial[i] = 0.0;                                                          /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* Initialize neighbor handling for CLUSTER NBC */                            /*12*/
  if (3 == NBC) /* CLUSTER */                                                   /*12*/
  {                                                                             /*12*/
    neighListOfCurrentPart = (int *) malloc((*nParts)*sizeof(int));             /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* Initialize neighbor handling for Iterator mode */                          /*12*/
                                                                                /*12*/
  if (1 == IterOrLoca)                                                          /*12*/
  {                                                                             /*12*/
    ier = KIM_API_get_neigh(pkim, 0, 0, &currentPart, &numOfPartNeigh,          /*12*/
                            &neighListOfCurrentPart, &Rij_list);                /*12*/
    /* check for successful initialization */                                   /*12*/
    if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)                                   /*12*/
    {                                                                           /*12*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);       /*12*/
      ier = KIM_STATUS_FAIL;                                                    /*12*/
      return ier;                                                               /*12*/
    }                                                                           /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* Compute energy and forces */                                               /*12*/
                                                                                /*12*/
  /* loop over particles and compute enregy and forces */                       /*12*/
  i = -1;                                                                       /*12*/
  while( 1 )                                                                    /*12*/
  {                                                                             /*12*/
                                                                                /*12*/
    /* Set up neighbor list for next particle for all NBC methods */            /*12*/
    if (1 == IterOrLoca) /* ITERATOR mode */                                    /*12*/
    {                                                                           /*12*/
      ier = KIM_API_get_neigh(pkim, 0, 1, &currentPart, &numOfPartNeigh,        /*12*/
                              &neighListOfCurrentPart, &Rij_list);              /*12*/
      if (KIM_STATUS_NEIGH_ITER_PAST_END == ier)                                /*12*/
      {  /* the end of the list, terminate loop */                              /*12*/
        break;                                                                  /*12*/
      }                                                                         /*12*/
      if (KIM_STATUS_OK > ier) /* some sort of problem, return */               /*12*/
      {                                                                         /*12*/
        KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);     /*12*/
        return ier;                                                             /*12*/
      }                                                                         /*12*/
                                                                                /*12*/
      i = currentPart;                                                          /*12*/
    }                                                                           /*12*/
    else                                                                        /*12*/
    {                                                                           /*12*/
      i++;                                                                      /*12*/
      if (*nParts <= i) /* incremented past end of list, terminate loop */      /*12*/
      {                                                                         /*12*/
        break;                                                                  /*12*/
      }                                                                         /*12*/
                                                                                /*12*/
      if (3 == NBC)     /* CLUSTER NBC method */                                /*12*/
      {                                                                         /*12*/
        numOfPartNeigh = *nParts - (i + 1);                                     /*12*/
        for (k = 0; k < numOfPartNeigh; ++k)                                    /*12*/
        {                                                                       /*12*/
          neighListOfCurrentPart[k] = i + k + 1;                                /*12*/
        }                                                                       /*12*/
        ier = KIM_STATUS_OK;                                                    /*12*/
      }                                                                         /*12*/
      else              /* All other NBCs */                                    /*12*/
      {                                                                         /*12*/
        ier = KIM_API_get_neigh(pkim, 1, i, &currentPart, &numOfPartNeigh,      /*12*/
                                &neighListOfCurrentPart, &Rij_list);            /*12*/
        if (KIM_STATUS_OK != ier) /* some sort of problem, return */            /*12*/
        {                                                                       /*12*/
          KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);   /*12*/
          ier = KIM_STATUS_FAIL;                                                /*12*/
          return ier;                                                           /*12*/
        }                                                                       /*12*/
      }                                                                         /*12*/
    }                                                                           /*12*/
                                                                                /*12*/
    /* loop over the neighbors of particle i */                                 /*12*/
    for (jj = 0; jj < numOfPartNeigh; ++ jj)                                    /*12*/
    {                                                                           /*12*/
                                                                                /*12*/
      j = neighListOfCurrentPart[jj]; /* get neighbor ID */                     /*12*/
                                                                                /*12*/
      /* compute relative position vector and squared distance */               /*12*/
      Rsqij = 0.0;                                                              /*12*/
      for (k = 0; k < DIM; ++k)                                                 /*12*/
      {                                                                         /*12*/
        if (0 != NBC) /* all methods except NEIGH_RVEC */                       /*12*/
        {                                                                       /*12*/
          Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];                       /*12*/
        }                                                                       /*12*/
        else          /* NEIGH_RVEC_F method */                                 /*12*/
        {                                                                       /*12*/
          Rij[k] = Rij_list[jj*DIM + k];                                        /*12*/
        }                                                                       /*12*/
                                                                                /*12*/
        /* apply periodic boundary conditions if required */                    /*12*/
        if (2 == NBC)                                                           /*12*/
        {                                                                       /*12*/
          if (abs(Rij[k]) > 0.5*boxSideLengths[k])                              /*12*/
          {                                                                     /*12*/
            Rij[k] -= (Rij[k]/fabs(Rij[k]))*boxSideLengths[k];                  /*12*/
          }                                                                     /*12*/
        }                                                                       /*12*/
                                                                                /*12*/
        /* compute squared distance */                                          /*12*/
        Rsqij += Rij[k]*Rij[k];                                                 /*12*/
      }                                                                         /*12*/
                                                                                /*12*/
      /* compute energy and force */                                            /*12*/
      if (Rsqij < MODEL_CUTSQ) /* particles are interacting ? */                /*12*/
      {                                                                         /*12*/
        R = sqrt(Rsqij);                                                        /*12*/
        if (comp_force || comp_virial)                                          /*12*/
        {                                                                       /*12*/
          /* compute pair potential and its derivative */                       /*12*/
          calc_phi_dphi(R, &phi, &dphi);                                        /*12*/
                                                                                /*12*/
          /* compute dEidr */                                                   /*12*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*12*/
          {                                                                     /*12*/
            /* HALF mode -- double contribution */                              /*12*/
            dEidr = dphi;                                                       /*12*/
          }                                                                     /*12*/
          else                                                                  /*12*/
          {                                                                     /*12*/
            /* FULL mode -- regular contribution */                             /*12*/
            dEidr = 0.5*dphi;                                                   /*12*/
          }                                                                     /*12*/
        }                                                                       /*12*/
        else                                                                    /*12*/
        {                                                                       /*12*/
          /* compute just pair potential */                                     /*12*/
          calc_phi(R, &phi);                                                    /*12*/
        }                                                                       /*12*/
                                                                                /*12*/
        /* contribution to energy */                                            /*12*/
        if (comp_particleEnergy)                                                /*12*/
        {                                                                       /*12*/
          particleEnergy[i] += 0.5*phi;                                         /*12*/
          /* if half list add energy for the other particle in the pair */      /*12*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*12*/
          {                                                                     /*12*/
            particleEnergy[j] += 0.5*phi;                                       /*12*/
          }                                                                     /*12*/
        }                                                                       /*12*/
        if (comp_energy)                                                        /*12*/
        {                                                                       /*12*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*12*/
          {                                                                     /*12*/
            /* Half mode -- add v to total energy */                            /*12*/
            *energy += phi;                                                     /*12*/
          }                                                                     /*12*/
          else                                                                  /*12*/
          {                                                                     /*12*/
            /* Full mode -- add half v to total energy */                       /*12*/
            *energy += 0.5*phi;                                                 /*12*/
          }                                                                     /*12*/
        }                                                                       /*12*/
                                                                                /*12*/
        /* contribution to virial tensor */                                     /*12*/
        if (comp_virial)                                                        /*12*/
        {                                                                       /*12*/
          /* virial(i,j) = r(i)*r(j)*(dV/dr)/r */                               /*12*/
          virial[0] += Rij[0]*Rij[0]*dEidr/R;                                   /*12*/
          virial[1] += Rij[1]*Rij[1]*dEidr/R;                                   /*12*/
          virial[2] += Rij[2]*Rij[2]*dEidr/R;                                   /*12*/
          virial[3] += Rij[1]*Rij[2]*dEidr/R;                                   /*12*/
          virial[4] += Rij[0]*Rij[2]*dEidr/R;                                   /*12*/
          virial[5] += Rij[0]*Rij[1]*dEidr/R;                                   /*12*/
        }                                                                       /*12*/
                                                                                /*12*/
        /* contribution to forces */                                            /*12*/
        if (comp_force)                                                         /*12*/
        {                                                                       /*12*/
          for (k = 0; k < DIM; ++k)                                             /*12*/
          {                                                                     /*12*/
            force[i*DIM + k] += dEidr*Rij[k]/R; /* accumulate force on i */     /*12*/
            force[j*DIM + k] -= dEidr*Rij[k]/R; /* accumulate force on j */     /*12*/
          }                                                                     /*12*/
        }                                                                       /*12*/
      }                                                                         /*12*/
    } /* loop on jj */                                                          /*12*/
  }    /* infinite while loop (terminated by break statements above */          /*12*/
                                                                                /*12*/
                                                                                /*12*/
  /* Free temporary storage */                                                  /*12*/
  if (3 == NBC)                                                                 /*12*/
  {                                                                             /*12*/
    free(neighListOfCurrentPart);                                               /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* everything is great */                                                     /*12*/
  ier = KIM_STATUS_OK;                                                          /*12*/
  return ier;                                                                   /*12*/
}                                                                               /*12*/
                                                                                /*12*/
/* Initialization function */                                                   /*12*/
int model_init(void *km)                                                        /*12*/
{                                                                               /*12*/
  /* Local variables */                                                         /*12*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*12*/
  double* model_cutoff;                                                         /*12*/
  int ier;                                                                      /*12*/
                                                                                /*12*/
  /* store pointer to compute function in KIM object */                         /*12*/
  ier = KIM_API_set_method(pkim, "compute", 1, (func_ptr) &compute);            /*12*/
  if (KIM_STATUS_OK > ier)                                                      /*12*/
  {                                                                             /*12*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_method", ier);        /*12*/
    return ier;                                                                 /*12*/
  }                                                                             /*12*/
                                                                                /*12*/
  /* store model cutoff in KIM object */                                        /*12*/
  model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);              /*12*/
  if (KIM_STATUS_OK > ier)                                                      /*12*/
  {                                                                             /*12*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);          /*12*/
    return ier;                                                                 /*12*/
  }                                                                             /*12*/
  *model_cutoff = MODEL_CUTOFF; /* cutoff distance in angstroms */              /*12*/
                                                                                /*12*/
  ier = KIM_STATUS_OK;                                                          /*12*/
  return ier;                                                                   /*12*/
}                                                                               /*12*/
