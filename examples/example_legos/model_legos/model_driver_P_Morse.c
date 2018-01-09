/* Markup tag documentation:                                                  *//*..............*/
/*    1 - ex_model_Ar_P_Morse_01C.c = CLUSTER                                 *//*..............*/
/*    2 - ex_model_Ar_P_Morse_02C.c + optional energy, force, particleEnergy  *//*..............*/
/*    3 - ex_model_Ar_P_Morse_03C.c + virial, particleVirial, hessian         *//*..............*/
/*    4 - ex_model_Ar_P_Morse_04C.c / process_dEdr & process_d2Edr2           *//*..............*/
/*    5 - ex_model_Ar_P_Morse_05C.c + NEIGH_PURE_H                            *//*..............*/
/*    6 - ex_model_Ar_P_Morse_06C.c + iterator mode                           *//*..............*/
/*    7 - ex_model_Ar_P_Morse_07C.c + NEIGH_PURE_F                            *//*..............*/
/*    8 - ex_model_Ar_P_Morse_08C.c + NEIGH_RVEC_*                            *//*..............*/
/*    9 - ex_model_Ar_P_Morse_09C.c + NEIGH_MIOPBC_*                          *//*..............*/
/* (1)0 - ex_model_Ar_P_Morse_10C.c + publish parameters                      *//*..............*/
/* (1)1 - ex_model_Ar_P_Morse_11C.c + flexible units                          *//*..............*/
/* (1)2 - ex_model_Ar_P_Morse_12C.c + Model Buffer                            *//*..............*/
/* (1)3 - ex_model_driver_P_Morse_13C.c & ex_model_driver_P_Morse.c           *//*..............*/
/* (1)4 - model_driver_P_Template.c                                           *//*..............*/
/*                                                                            *//*..............*/
/*                                                                            *//*12345678901234*/
/* CDDL HEADER START                                                          *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* The contents of this file are subject to the terms of the Common           *//*12345678901234*/
/* Development and Distribution License Version 1.0 (the "License").          *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* You can obtain a copy of the license at                                    *//*12345678901234*/
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      *//*12345678901234*/
/* specific language governing permissions and limitations under the License. *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* When distributing Covered Code, include this CDDL HEADER in each file and  *//*12345678901234*/
/* include the License file in a prominent location with the name             *//*12345678901234*/
/* LICENSE.CDDL.  If applicable, add the following below this CDDL HEADER,    *//*12345678901234*/
/* with the fields enclosed by brackets "[]" replaced with your own           *//*12345678901234*/
/* identifying information:                                                   *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   *//*12345678901234*/
/* All rights reserved.                                                       *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* CDDL HEADER END                                                            *//*12345678901234*/
/*                                                                            *//*12345678901234*/
                                                                                /*12345678901234*/
/*                                                                            *//*12345678901234*/
/* Copyright (c) 2013--2018, Regents of the University of Minnesota.          *//*12345678901234*/
/* All rights reserved.                                                       *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* Contributors:                                                              *//*12345678901234*/
/*    Ryan S. Elliott                                                         *//*12345678901234*/
/*    Ellad B. Tadmor                                                         *//*12345678901234*/
/*    Stephen M. Whalen                                                       *//*12345678901234*/
/*                                                                            *//*12345678901234*/
                                                                                /*.............4*/
/*                                                                            *//*.............4*/
/* Portions Copyright (c) <FILL_year>, <FILL_copyright_holder>.               *//*.............4*/
/* All rights reserved.                                                       *//*.............4*/
/*                                                                            *//*.............4*/
/* Contributors:                                                              *//*.............4*/
/*    <FILL_name>                                                             *//*.............4*/
/*                                                                            *//*.............4*/
                                                                                /*12345678901234*/
/******************************************************************************//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* <FILL_model_driver_name> pair potential KIM Model Driver                   *//*............34*/
/* <FILL_model_driver_name> pair potential KIM Model                          *//*123456789012..*/
/* shifted to have zero energy at the cutoff radius                           *//*1234567890123.*/
/*                                                                            *//*.............4*/
/* Reference: <FILL>                                                          *//*.............4*/
/*                                                                            *//*12345678901234*/
/* Language: C                                                                *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/* Release: This file is part of the kim-api.git repository.                  *//*12345678901234*/
/*                                                                            *//*12345678901234*/
/******************************************************************************//*12345678901234*/
                                                                                /*12345678901234*/
                                                                                /*12345678901234*/
#include <stdlib.h>                                                             /*12345678901234*/
#include <stdio.h>                                                              /*12345678901234*/
#include <string.h>                                                             /*12345678901234*/
#include <math.h>                                                               /*12345678901234*/
#include "KIM_API_C.h"                                                          /*12345678901234*/
#include "KIM_API_status.h"                                                     /*12345678901234*/
                                                                                /*12345678901234*/
/******************************************************************************//*12345678901234*/
/* Below are the definitions for some constants                               *//*.........01234*/
/* Below are the definitions and values of all Model parameters               *//*123456789.....*/
/******************************************************************************//*12345678901234*/
#define DIM 3  /* dimensionality of space */                                    /*12345678901234*/
#define SPECCODE 1  /* internal species code */                                 /*12345678901234*/
#define CUTOFF 8.15  /* Angstroms */                                            /*123456789.....*/
#define EPSILON -0.0134783698072604  /* eV */                                   /*123456789.....*/
#define PARAM_C 1.545  /* 1/Angstroms */                                        /*123456789.....*/
#define RZERO   3.786  /* Angstroms */                                          /*123456789.....*/
                                                                                /*12345678901234*/
                                                                                /*12345678901234*/
/* Define prototype for Model Driver init */                                    /*............34*/
int model_driver_init(void* km, char* paramfile_names, int* nmstrlen,           /*............34*/
                      int* numparamfiles);                                      /*............34*/
                                                                                /*..............*/
/* Define prototype for Model init */                                           /*123456789012..*/
int model_init(void* km);                                                       /*123456789012..*/
                                                                                /*.........01234*/
/* Define prototypes for reinit and destroy */                                  /*.........01234*/
/* defined as static to avoid namespace clashes with other codes */             /*.........01234*/
static int reinit(void* km);                                                    /*.........01234*/
static int destroy(void* km);                                                   /*.........01234*/
                                                                                /*12345678901234*/
/* Define prototype for compute routine */                                      /*12345678901234*/
static int compute(void* km);                                                   /*12345678901234*/
                                                                                /*12345678901234*/
/* Define prototypes for pair potential calculations */                         /*12345678901234*/
static void calc_phi(double* <FILL_parameter_1>,                                /*12345678901234*/
                     double* <FILL_parameter_2>,                                /*12345678901234*/
                     double* <FILL_parameter_3>,                                /*12345678901234*/
                     double* <FILL_parameter_4>,                                /*12345678901234*/
                     /* FILL as many parameters as needed */                    /*.............4*/
                     double* cutoff, double r, double* phi);                    /*12345678901234*/
                                                                                /*.2345678901234*/
static void calc_phi_dphi(double* <FILL_parameter_1>,                           /*.2345678901234*/
                          double* <FILL_parameter_2>,                           /*.2345678901234*/
                          double* <FILL_parameter_3>,                           /*.2345678901234*/
                          double* <FILL_parameter_4>,                           /*.2345678901234*/
                          /* FILL as many parameters as needed */               /*.............4*/
                          double* cutoff, double r, double* phi, double* dphi); /*.2345678901234*/
                                                                                /*..345678901234*/
static void calc_phi_d2phi(double* <FILL_parameter_1>,                          /*..345678901234*/
                           double* <FILL_parameter_2>,                          /*..345678901234*/
                           double* <FILL_parameter_3>,                          /*..345678901234*/
                           double* <FILL_parameter_4>,                          /*..345678901234*/
                           /* FILL as many parameters as needed */              /*.............4*/
                           double* cutoff, double r, double* phi, double* dphi, /*..345678901234*/
                           double* d2phi);                                      /*..345678901234*/
                                                                                /*12345678901234*/
/* Define model_buffer structure */                                             /*...........234*/
struct model_buffer                                                             /*...........234*/
{                                                                               /*...........234*/
  int NBC;                                                                      /*...........234*/
  int HalfOrFull;                                                               /*...........234*/
  int IterOrLoca;                                                               /*...........234*/
  int energy_ind;                                                               /*...........234*/
  int forces_ind;                                                               /*...........234*/
  int particleEnergy_ind;                                                       /*...........234*/
  int process_dEdr_ind;                                                         /*...........234*/
  int process_d2Edr2_ind;                                                       /*...........234*/
  int model_index_shift;                                                        /*...........234*/
  int numberOfParticles_ind;                                                    /*...........234*/
  int particleSpecies_ind;                                                      /*...........234*/
  int coordinates_ind;                                                          /*...........234*/
  int numberContribParticles_ind;                                               /*...........234*/
  int boxSideLengths_ind;                                                       /*...........234*/
  int get_neigh_ind;                                                            /*...........234*/
  int cutoff_ind;                                                               /*...........234*/
                                                                                /*...........234*/
  double Pcutoff;                                                               /*...........234*/
  double cutsq;                                                                 /*...........234*/
  double <FILL_parameter_1>;                                                    /*...........234*/
  double <FILL_parameter_2>;                                                    /*...........234*/
  double <FILL_parameter_3>;                                                    /*...........234*/
  double <FILL_parameter_4>;                                                    /*...........234*/
  /* FILL as many parameters as needed */                                       /*.............4*/
};                                                                              /*...........234*/
                                                                                /*...........234*/
                                                                                /*...........234*/
/* Calculate pair potential phi(r) */                                           /*12345678901234*/
static void calc_phi(double* <FILL_parameter_1>,                                /*12345678901234*/
                     double* <FILL_parameter_2>,                                /*12345678901234*/
                     double* <FILL_parameter_3>,                                /*12345678901234*/
                     double* <FILL_parameter_4>,                                /*12345678901234*/
                     /* FILL as many parameters as needed */                    /*.............4*/
                     double* cutoff, double r, double* phi)                     /*12345678901234*/
{                                                                               /*12345678901234*/
  /* local variables */                                                         /*12345678901234*/
  double ep;                                                                    /*1234567890123.*/
  double ep2;                                                                   /*1234567890123.*/
  /* FILL: place any local variable definitions here */                         /*.............4*/
                                                                                /*12345678901234*/
  ep = exp(-(*C)*(r-*Rzero));                                                   /*1234567890123.*/
  ep2 = ep*ep;                                                                  /*1234567890123.*/
                                                                                /*1234567890123.*/
  if (r > *cutoff)                                                              /*12345678901234*/
  {                                                                             /*12345678901234*/
    /* Argument exceeds cutoff radius */                                        /*12345678901234*/
    *phi = 0.0;                                                                 /*12345678901234*/
  }                                                                             /*12345678901234*/
  else                                                                          /*12345678901234*/
  {                                                                             /*12345678901234*/
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;                               /*1234567890123.*/
    *phi = <FILL_functional_form_of_phi(r)>;                                    /*.............4*/
  }                                                                             /*12345678901234*/
                                                                                /*12345678901234*/
  return;                                                                       /*12345678901234*/
}                                                                               /*12345678901234*/
                                                                                /*.2345678901234*/
/* Calculate pair potential phi(r) and its derivative dphi(r) */                /*.2345678901234*/
static void calc_phi_dphi(double* <FILL_parameter_1>,                           /*.2345678901234*/
                          double* <FILL_parameter_2>,                           /*.2345678901234*/
                          double* <FILL_parameter_3>,                           /*.2345678901234*/
                          double* <FILL_parameter_4>,                           /*.2345678901234*/
                          /* FILL as many parameters as needed */               /*.............4*/
                          double* cutoff, double r, double* phi, double* dphi)  /*.2345678901234*/
{                                                                               /*.2345678901234*/
  /* local variables */                                                         /*.2345678901234*/
  double ep;                                                                    /*.234567890123.*/
  double ep2;                                                                   /*.234567890123.*/
  /* FILL: place any local variable definitions here */                         /*.............4*/
                                                                                /*.2345678901234*/
  ep = exp(-(*C)*(r-*Rzero));                                                   /*.234567890123.*/
  ep2 = ep*ep;                                                                  /*.234567890123.*/
                                                                                /*.234567890123.*/
  if (r > *cutoff)                                                              /*.2345678901234*/
  {                                                                             /*.2345678901234*/
    /* Argument exceeds cutoff radius */                                        /*.2345678901234*/
    *phi = 0.0;                                                                 /*.2345678901234*/
    *dphi = 0.0;                                                                /*.2345678901234*/
  }                                                                             /*.2345678901234*/
  else                                                                          /*.2345678901234*/
  {                                                                             /*.2345678901234*/
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;                               /*.234567890123.*/
    *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 );                                  /*.234567890123.*/
    *phi = <FILL functional form of phi(r)>;                                    /*.............4*/
    *dphi = <FILL functional form of dphi(r)>;                                  /*.............4*/
  }                                                                             /*.2345678901234*/
                                                                                /*.2345678901234*/
  return;                                                                       /*.2345678901234*/
}                                                                               /*.2345678901234*/
                                                                                /*..345678901234*/
/* Calculate pair potential phi(r) and its 1st & 2nd derivatives dphi(r), */    /*..345678901234*/
/* d2phi(r) */                                                                  /*..345678901234*/
static void calc_phi_d2phi(double* <FILL_parameter_1>,                          /*..345678901234*/
                           double* <FILL_parameter_2>,                          /*..345678901234*/
                           double* <FILL_parameter_3>,                          /*..345678901234*/
                           double* <FILL_parameter_4>,                          /*..345678901234*/
                           /* FILL as many parameters as needed */              /*.............4*/
                           double* cutoff, double r, double* phi, double* dphi, /*..345678901234*/
                           double* d2phi)                                       /*..345678901234*/
{                                                                               /*..345678901234*/
  /* local variables */                                                         /*..345678901234*/
  double ep;                                                                    /*..34567890123.*/
  double ep2;                                                                   /*..34567890123.*/
  /* FILL: place any local variable definitions here */                         /*.............4*/
                                                                                /*..345678901234*/
  ep = exp(-(*C)*(r-*Rzero));                                                   /*..34567890123.*/
  ep2 = ep*ep;                                                                  /*..34567890123.*/
                                                                                /*..34567890123.*/
  if (r > *cutoff)                                                              /*..345678901234*/
  {                                                                             /*..345678901234*/
    /* Argument exceeds cutoff radius */                                        /*..345678901234*/
    *phi = 0.0;                                                                 /*..345678901234*/
    *dphi = 0.0;                                                                /*..345678901234*/
    *d2phi = 0.0;                                                               /*..345678901234*/
  }                                                                             /*..345678901234*/
  else                                                                          /*..345678901234*/
  {                                                                             /*..345678901234*/
    *phi = (*epsilon)*( -ep2 + 2.0*ep ) + *shift;                               /*..34567890123.*/
    *dphi = 2.0*(*epsilon)*(*C)*( -ep + ep2 );                                  /*..34567890123.*/
    *d2phi = 2.0*(*epsilon)*(*C)*(*C)*(ep - 2.0*ep2);                           /*..34567890123.*/
    *phi = <FILL functional form of phi(r)>;                                    /*.............4*/
    *dphi = <FILL functional form of dphi(r)>;                                  /*.............4*/
    *d2phi = <FILL functional form of d2phi(r)>;                                /*.............4*/
  }                                                                             /*..345678901234*/
                                                                                /*..345678901234*/
  return;                                                                       /*..345678901234*/
}                                                                               /*..345678901234*/
                                                                                /*12345678901234*/
/* compute function */                                                          /*12345678901234*/
static int compute(void* km)                                                    /*12345678901234*/
{                                                                               /*12345678901234*/
  /* local variables */                                                         /*12345678901234*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*12345678901234*/
  double R;                                                                     /*12345678901234*/
  double R_pairs[2];                                                            /*...45678901234*/
  double *pR_pairs = &(R_pairs[0]);                                             /*...45678901234*/
  double Rsqij;                                                                 /*12345678901234*/
  double phi;                                                                   /*12345678901234*/
  double dphi;                                                                  /*.2345678901234*/
  double d2phi;                                                                 /*..345678901234*/
  double dEidr;                                                                 /*.2345678901234*/
  double d2Eidr;                                                                /*..345678901234*/
  double Rij[DIM];                                                              /*12345678901234*/
  double *pRij = &(Rij[0]);                                                     /*...45678901234*/
  double Rij_pairs[2][3];                                                       /*...45678901234*/
  double *pRij_pairs = &(Rij_pairs[0][0]);                                      /*...45678901234*/
  int ier;                                                                      /*12345678901234*/
  int i;                                                                        /*12345678901234*/
  int i_pairs[2];                                                               /*...45678901234*/
  int *pi_pairs = &(i_pairs[0]);                                                /*...45678901234*/
  int j;                                                                        /*12345678901234*/
  int j_pairs[2];                                                               /*...45678901234*/
  int *pj_pairs = &(j_pairs[0]);                                                /*...45678901234*/
  int jj;                                                                       /*....5678901234*/
  int k;                                                                        /*12345678901234*/
  int m;                                                                        /*..3...........*/
  int currentPart;                                                              /*....5678901234*/
  int* neighListOfCurrentPart;                                                  /*....5678901234*/
  struct model_buffer* buffer;                                                  /*...........234*/
  int comp_energy;                                                              /*.2345678901234*/
  int comp_force;                                                               /*.2345678901234*/
  int comp_particleEnergy;                                                      /*.2345678901234*/
  int comp_process_dEdr;                                                        /*...45678901234*/
  int comp_process_d2Edr2;                                                      /*...45678901234*/
  int comp_virial;                                                              /*..3...........*/
  int comp_particleVirial;                                                      /*..3...........*/
  int comp_hessian;                                                             /*..3...........*/
  int NBC;                                                                      /*....5678901234*/
  int HalfOrFull;                                                               /*......78901234*/
  int IterOrLoca;                                                               /*.....678901234*/
  int model_index_shift;                                                        /*...........234*/
  int zero = 0;                                                                 /*.....678901234*/
  int one = 1;                                                                  /*....5678901234*/
  int request;                                                                  /*....5678901234*/
                                                                                /*12345678901234*/
  int* nParts;                                                                  /*12345678901234*/
  int* particleSpecies;                                                         /*12345678901234*/
  double* cutoff;                                                               /*12345678901234*/
  double cutsq;                                                                 /*123456789.....*/
  double <FILL_parameter_1>;                                                    /*123456789.....*/
  double <FILL_parameter_2>;                                                    /*123456789.....*/
  double <FILL_parameter_3>;                                                    /*123456789.....*/
  double <FILL_parameter_4>;                                                    /*123456789.....*/
  double* cutsq;                                                                /*.........01234*/
  double* <FILL_parameter_1>;                                                   /*.........01234*/
  double* <FILL_parameter_2>;                                                   /*.........01234*/
  double* <FILL_parameter_3>;                                                   /*.........01234*/
  double* <FILL_parameter_4>;                                                   /*.........01234*/
  /* FILL as many parameters as needed */                                       /*.............4*/
  double* Rij_list;                                                             /*....5678901234*/
  double* coords;                                                               /*12345678901234*/
  double* energy;                                                               /*12345678901234*/
  double* force;                                                                /*.2345678901234*/
  double* particleEnergy;                                                       /*.2345678901234*/
  double* virial;                                                               /*..3...........*/
  double* particleVirial;                                                       /*..3...........*/
  double* hessian;                                                              /*..3...........*/
  double* boxSideLengths;                                                       /*........901234*/
  int* numContrib;                                                              /*....5678901234*/
  int numberContrib;                                                            /*....5678901234*/
  int numOfPartNeigh;                                                           /*....5678901234*/
  typedef int (*get_neigh_ptr)(void *,int *,int *,int *, int *, int **,         /*...........234*/
                               double **);                                      /*...........234*/
  get_neigh_ptr get_neigh = NULL;                                               /*...........234*/
  const char* NBCstr;                                                           /*....5678901...*/
  double fac;                                                                   /*..3...........*/
  double fac1;                                                                  /*..3...........*/
  double fac2;                                                                  /*..3...........*/
  double fac3;                                                                  /*..3...........*/
  double stiff[DIM][DIM];                                                       /*..3...........*/
  double dummy;                                                                 /*123456789.....*/
                                                                                /*12345678901234*/
  /* get buffer from KIM object */                                              /*...........234*/
  buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);         /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);  /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  /* unpack info from the buffer */                                             /*...........234*/
  NBC = buffer->NBC;                                                            /*...........234*/
  HalfOrFull = buffer->HalfOrFull;                                              /*...........234*/
  IterOrLoca = buffer->IterOrLoca;                                              /*...........234*/
  model_index_shift = buffer->model_index_shift;                                /*...........234*/
  cutsq = &(buffer->cutsq);                                                     /*...........234*/
  <FILL_parameter_1> = &(buffer-><FILL_parameter_1>);                           /*...........234*/
  <FILL_parameter_2> = &(buffer-><FILL_parameter_2>);                           /*...........234*/
  <FILL_parameter_3> = &(buffer-><FILL_parameter_3>);                           /*...........234*/
  <FILL_parameter_4> = &(buffer-><FILL_parameter_4>);                           /*...........234*/
  /* also FILL additional parameters here if there are any ... */               /*.............4*/
                                                                                /*..............*/
  /* Determine neighbor list boundary condition (NBC) */                        /*....5678901...*/
  ier = KIM_API_get_NBC_method(pkim, &NBCstr);                                  /*....5678901...*/
  if (KIM_STATUS_OK > ier)                                                      /*....5678901...*/
  {                                                                             /*....5678901...*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);    /*....5678901...*/
    return ier;                                                                 /*....5678901...*/
  }                                                                             /*....5678901...*/
  if ((!strcmp("NEIGH_RVEC_H",NBCstr)) || (!strcmp("NEIGH_RVEC_F",NBCstr)))     /*.......8901...*/
  {                                                                             /*.......8901...*/
    NBC = 0;                                                                    /*.......8901...*/
  }                                                                             /*.......8901...*/
  else if ((!strcmp("NEIGH_PURE_H",NBCstr)) || (!strcmp("NEIGH_PURE_F",NBCstr)))/*.......8901...*/
  if ((!strcmp("NEIGH_PURE_H",NBCstr)) || (!strcmp("NEIGH_PURE_F",NBCstr)))     /*......7.......*/
  if (!strcmp("NEIGH_PURE_H",NBCstr))                                           /*....56........*/
  {                                                                             /*....5678901...*/
    NBC = 1;                                                                    /*....5678901...*/
  }                                                                             /*....5678901...*/
  else if ((!strcmp("MI_OPBC_H",NBCstr)) || (!strcmp("MI_OPBC_F",NBCstr)))      /*........901...*/
  {                                                                             /*........901...*/
    NBC = 2;                                                                    /*........901...*/
  }                                                                             /*........901...*/
  else if (!strcmp("CLUSTER",NBCstr))                                           /*....5678901...*/
  {                                                                             /*....5678901...*/
    NBC = 3;                                                                    /*....5678901...*/
  }                                                                             /*....5678901...*/
  else                                                                          /*....5678901...*/
  {                                                                             /*....5678901...*/
    ier = KIM_STATUS_FAIL;                                                      /*....5678901...*/
    KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);        /*....5678901...*/
    return ier;                                                                 /*....5678901...*/
  }                                                                             /*....5678901...*/
                                                                                /*......78901...*/
  /* Determine if Half or Full neighbor lists are being used */                 /*......78901...*/
  /*****************************/                                               /*......78901...*/
  /* HalfOrFull = 1 -- Half    */                                               /*......78901...*/
  /*            = 2 -- Full    */                                               /*......78901...*/
  /*****************************/                                               /*......78901...*/
  if (KIM_API_is_half_neighbors(pkim, &ier))                                    /*......78901...*/
  {                                                                             /*......78901...*/
    HalfOrFull = 1;                                                             /*......78901...*/
  }                                                                             /*......78901...*/
  else                                                                          /*......78901...*/
  {                                                                             /*......78901...*/
    HalfOrFull = 2;                                                             /*......78901...*/
  }                                                                             /*......78901...*/
                                                                                /*.....678901...*/
  /* determine neighbor list handling mode */                                   /*.....678901...*/
  if (NBC != 3)                                                                 /*.....678901...*/
  {                                                                             /*.....678901...*/
    /******************************/                                            /*.....678901...*/
    /* IterOrLoca = 1 -- Iterator */                                            /*.....678901...*/
    /*            = 2 -- Locator  */                                            /*.....678901...*/
    /******************************/                                            /*.....678901...*/
    IterOrLoca = KIM_API_get_neigh_mode(pkim, &ier);                            /*.....678901...*/
    if (KIM_STATUS_OK > ier)                                                    /*.....678901...*/
    {                                                                           /*.....678901...*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh_mode", ier);  /*.....678901...*/
      return ier;                                                               /*.....678901...*/
    }                                                                           /*.....678901...*/
    if ((IterOrLoca != 1) && (IterOrLoca != 2))                                 /*.....678901...*/
    {                                                                           /*.....678901...*/
      ier = KIM_STATUS_FAIL;                                                    /*.....678901...*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*.....678901...*/
                           "Unsupported IterOrLoca mode", ier);                 /*.....678901...*/
      return ier;                                                               /*.....678901...*/
    }                                                                           /*.....678901...*/
  }                                                                             /*.....678901...*/
  else                                                                          /*.....678901...*/
  {                                                                             /*.....678901...*/
    IterOrLoca = 2;  /* for CLUSTER NBC */                                      /*.....678901...*/
  }                                                                             /*.....678901...*/
                                                                                /*.........01...*/
  /* get parameters from KIM object */                                          /*.........01...*/
  KIM_API_getm_data(pkim, &ier, 5*3,                                            /*.........01...*/
                    "PARAM_FIXED_cutsq",  &cutsq,   1,                          /*.........01...*/
                    "PARAM_FREE_epsilon", &epsilon, 1,                          /*.........01...*/
                    "PARAM_FREE_C",       &C,       1,                          /*.........01...*/
                    "PARAM_FREE_Rzero",   &Rzero,   1,                          /*.........01...*/
                    "PARAM_FIXED_shift",  &shift,   1                           /*.........01...*/
                    );                                                          /*.........01...*/
  if (KIM_STATUS_OK > ier)                                                      /*.........01...*/
  {                                                                             /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);         /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
                                                                                /*....5678901234*/
  /* check to see if we have been asked to compute the forces, */               /*...........234*/
  /* particleEnergy, and d1Edr */                                               /*...........234*/
  KIM_API_getm_compute_by_index(                                                /*...........234*/
      pkim, &ier, 5*3,                                                          /*...........234*/
      buffer->energy_ind,         &comp_energy,         1,                      /*...........234*/
      buffer->forces_ind,         &comp_force,          1,                      /*...........234*/
      buffer->particleEnergy_ind, &comp_particleEnergy, 1,                      /*...........234*/
      buffer->process_dEdr_ind,   &comp_process_dEdr,   1,                      /*...........234*/
      buffer->process_d2Edr2_ind, &comp_process_d2Edr2, 1);                     /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*...........234*/
                         "KIM_API_getm_compute_by_index", ier);                 /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  KIM_API_getm_data_by_index(                                                   /*...........234*/
      pkim, &ier, 9*3,                                                          /*...........234*/
      buffer->cutoff_ind,                 &cutoff,         1,                   /*...........234*/
      buffer->numberOfParticles_ind,      &nParts,         1,                   /*...........234*/
      buffer->particleSpecies_ind,        &particleSpecies,1,                   /*...........234*/
      buffer->coordinates_ind,            &coords,         1,                   /*...........234*/
      buffer->numberContribParticles_ind, &numContrib,     (HalfOrFull==1),     /*...........234*/
      buffer->boxSideLengths_ind,         &boxSideLengths, (NBC==2),            /*...........234*/
      buffer->energy_ind,                 &energy,         comp_energy,         /*...........234*/
      buffer->forces_ind,                 &force,          comp_force,          /*...........234*/
      buffer->particleEnergy_ind,         &particleEnergy, comp_particleEnergy);/*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data_by_index", ier);/*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
  if (NBC!=3)                                                                   /*...........234*/
  {                                                                             /*...........234*/
    get_neigh = (get_neigh_ptr)                                                 /*...........234*/
        KIM_API_get_method_by_index(pkim, buffer->get_neigh_ind, &ier);         /*...........234*/
    if (KIM_STATUS_OK > ier)                                                    /*...........234*/
    {                                                                           /*...........234*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*...........234*/
                           "KIM_API_get_method_by_index", ier);                 /*...........234*/
      return ier;                                                               /*...........234*/
    }                                                                           /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*..............*/
  /* check to see if we have been asked to compute the forces, */               /*.2345678901...*/
  /* particleEnergy, and d1Edr */                                               /*.2345678901...*/
  KIM_API_getm_compute(pkim, &ier, 5*3,                                         /*...45678901...*/
  KIM_API_getm_compute(pkim, &ier, 6*3,                                         /*..3...........*/
  KIM_API_getm_compute(pkim, &ier, 3*3,                                         /*.2............*/
                       "energy",         &comp_energy,         1,               /*.2345678901...*/
                       "forces",         &comp_force,          1,               /*.2345678901...*/
                       "particleEnergy", &comp_particleEnergy, 1,               /*..345678901...*/
                       "particleEnergy", &comp_particleEnergy, 1                /*.2............*/
                       "process_dEdr",   &comp_process_dEdr,   1,               /*...45678901...*/
                       "process_d2Edr2", &comp_process_d2Edr2, 1                /*...45678901...*/
                       "virial",         &comp_virial,         1,               /*..3...........*/
                       "particleVirial", &comp_particleVirial, 1,               /*..3...........*/
                       "hessian",        &comp_hessian,        1                /*..3...........*/
                       );                                                       /*.2345678901...*/
  if (KIM_STATUS_OK > ier)                                                      /*.2345678901...*/
  {                                                                             /*.2345678901...*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_compute", ier);      /*.2345678901...*/
    return ier;                                                                 /*.2345678901...*/
  }                                                                             /*.2345678901...*/
                                                                                /*.2345678901...*/
  KIM_API_getm_data(                                                            /*12345678901...*/
      pkim, &ier, 9*3,                                                          /*........901...*/
      pkim, &ier, 8*3,                                                          /*....5678......*/
      pkim, &ier, 7*3,                                                          /*.2.4..........*/
      pkim, &ier, 10*3,                                                         /*..3...........*/
      pkim, &ier, 5*3,                                                          /*1.............*/
      "cutoff",                      &cutoff,         1,                        /*12345678901...*/
      "numberOfParticles",           &nParts,         1,                        /*12345678901...*/
      "particleSpecies",             &particleSpecies,1,                        /*12345678901...*/
      "coordinates",                 &coords,         1,                        /*12345678901...*/
      "numberContributingParticles", &numContrib,     (HalfOrFull==1),          /*......78901...*/
      "numberContributingParticles", &numContrib,     1,                        /*....56........*/
      "boxSideLengths",              &boxSideLengths, (NBC==2),                 /*........901...*/
      "energy",                      &energy,         comp_energy,              /*.2345678901...*/
      "energy",                      &energy,         1                         /*1.............*/
      "forces",                      &force,          comp_force,               /*.2345678901...*/
      "particleEnergy",              &particleEnergy, comp_particleEnergy       /*.2.45678901...*/
      "particleEnergy",              &particleEnergy, comp_particleEnergy,      /*..3...........*/
      "virial",                      &virial,         comp_virial,              /*..3...........*/
      "particleVirial",              &particleVirial, comp_particleVirial,      /*..3...........*/
      "hessian",                     &hessian,        comp_hessian              /*..3...........*/
                    );                                                          /*12345678901...*/
  if (KIM_STATUS_OK > ier)                                                      /*12345678901...*/
  {                                                                             /*12345678901...*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);         /*12345678901...*/
    return ier;                                                                 /*12345678901...*/
  }                                                                             /*12345678901...*/
                                                                                /*12345678901234*/
  /* set value of parameters */                                                 /*123456789.....*/
  cutsq = (*cutoff)*(*cutoff);                                                  /*123456789.....*/
  epsilon = EPSILON;                                                            /*123456789.....*/
  C = PARAM_C;                                                                  /*123456789.....*/
  Rzero = RZERO;                                                                /*123456789.....*/
  /* set value of parameter shift */                                            /*123456789.....*/
  dummy = 0.0;                                                                  /*123456789.....*/
  /* call calc_phi with r=cutoff and shift=0.0 */                               /*123456789.....*/
  calc_phi(&epsilon, &C, &Rzero, &dummy, cutoff, *cutoff, &shift);              /*123456789.....*/
  /* set shift to -shift */                                                     /*123456789.....*/
  shift = -(shift);                                                             /*123456789.....*/
                                                                                /*123456789.....*/
  if (HalfOrFull == 1)                                                          /*......78901234*/
  {                                                                             /*......78901234*/
    if (3 != NBC)                                                               /*......78901234*/
    {                                                                           /*......78901234*/
      /* non-CLUSTER cases */                                                   /*......78901234*/
      numberContrib = *numContrib;                                              /*......78901234*/
    }                                                                           /*......78901234*/
    else                                                                        /*......78901234*/
    {                                                                           /*......78901234*/
      /* CLUSTER cases */                                                       /*......78901234*/
      numberContrib = *nParts;                                                  /*......78901234*/
    }                                                                           /*......78901234*/
  }                                                                             /*......78901234*/
  else                                                                          /*......78901234*/
  {                                                                             /*......78901234*/
    /* provide initialization even if not used */                               /*......78901234*/
    numberContrib = *nParts;                                                    /*......78901234*/
  }                                                                             /*......78901234*/
                                                                                /*..............*/
  if (3 != NBC)                                                                 /*....56........*/
  {                                                                             /*....56........*/
    /* non-CLUSTER cases */                                                     /*....56........*/
    numberContrib = *numContrib;                                                /*....56........*/
  }                                                                             /*....56........*/
  else                                                                          /*....56........*/
  {                                                                             /*....56........*/
    /* CLUSTER cases */                                                         /*....56........*/
    numberContrib = *nParts;                                                    /*....56........*/
  }                                                                             /*....56........*/
                                                                                /*....5678901234*/
  /* Check to be sure that the species are correct */                           /*12345678901234*/
  /**/                                                                          /*12345678901234*/
  ier = KIM_STATUS_FAIL; /* assume an error */                                  /*12345678901234*/
  for (i = 0; i < *nParts; ++i)                                                 /*12345678901234*/
  {                                                                             /*12345678901234*/
    if ( SPECCODE != particleSpecies[i])                                        /*12345678901234*/
    {                                                                           /*12345678901234*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*12345678901234*/
                           "Unexpected species detected", ier);                 /*12345678901234*/
      return ier;                                                               /*12345678901234*/
    }                                                                           /*12345678901234*/
  }                                                                             /*12345678901234*/
  ier = KIM_STATUS_OK;  /* everything is ok */                                  /*12345678901234*/
                                                                                /*12345678901234*/
  /* initialize potential energies, forces */                                   /*.2............*/
  /* initialize potential energies, forces, and virial term */                  /*..345678901234*/
  if (comp_particleEnergy)                                                      /*.2345678901234*/
  {                                                                             /*.2345678901234*/
    for (i = 0; i < *nParts; ++i)                                               /*.2345678901234*/
    {                                                                           /*.2345678901234*/
      particleEnergy[i] = 0.0;                                                  /*.2345678901234*/
    }                                                                           /*.2345678901234*/
  }                                                                             /*.2345678901234*/
  if (comp_energy)                                                              /*.2345678901234*/
  {                                                                             /*.2345678901234*/
    *energy = 0.0;                                                              /*.2345678901234*/
  }                                                                             /*.2345678901234*/
                                                                                /*.2345678901234*/
  if (comp_force)                                                               /*.2345678901234*/
  {                                                                             /*.2345678901234*/
    for (i = 0; i < *nParts; ++i)                                               /*.2345678901234*/
    {                                                                           /*.2345678901234*/
      for (k = 0; k < DIM; ++k)                                                 /*.2345678901234*/
      {                                                                         /*.2345678901234*/
        force[i*DIM + k] = 0.0;                                                 /*.2345678901234*/
      }                                                                         /*.2345678901234*/
    }                                                                           /*.2345678901234*/
  }                                                                             /*.2345678901234*/
                                                                                /*..............*/
  *energy = 0.0;                                                                /*1.............*/
                                                                                /*..3...........*/
  if (comp_virial)                                                              /*..3...........*/
  {                                                                             /*..3...........*/
    for (i = 0; i < 6; ++i)                                                     /*..3...........*/
    {                                                                           /*..3...........*/
      virial[i] = 0.0;                                                          /*..3...........*/
    }                                                                           /*..3...........*/
  }                                                                             /*..3...........*/
                                                                                /*..3...........*/
  if (comp_particleVirial)                                                      /*..3...........*/
  {                                                                             /*..3...........*/
    for (i = 0; i < *nParts; ++i)                                               /*..3...........*/
    {                                                                           /*..3...........*/
      for (j = 0; j < 6; ++j)                                                   /*..3...........*/
      {                                                                         /*..3...........*/
        particleVirial[i*6 + j] = 0.0;                                          /*..3...........*/
      }                                                                         /*..3...........*/
    }                                                                           /*..3...........*/
  }                                                                             /*..3...........*/
                                                                                /*..3...........*/
  if (comp_hessian)                                                             /*..3...........*/
  {                                                                             /*..3...........*/
    for (i = 0; i < *nParts; ++i)                                               /*..3...........*/
    {                                                                           /*..3...........*/
      for (j = 0; j < *nParts; ++j)                                             /*..3...........*/
      {                                                                         /*..3...........*/
        for (k = 0; k < DIM; ++k)                                               /*..3...........*/
        {                                                                       /*..3...........*/
          for (m = 0; m < DIM; ++m)                                             /*..3...........*/
          {                                                                     /*..3...........*/
            hessian[i*(*nParts)*DIM*DIM + j*DIM*DIM + k*DIM + m] = 0.0;         /*..3...........*/
          }                                                                     /*..3...........*/
        }                                                                       /*..3...........*/
      }                                                                         /*..3...........*/
    }                                                                           /*..3...........*/
  }                                                                             /*..3...........*/
                                                                                /*....5678901234*/
  /* Initialize neighbor handling for CLUSTER NBC */                            /*....5678901234*/
  if (3 == NBC)                                                                 /*....5678901234*/
  {                                                                             /*....5678901234*/
    /* CLUSTER */                                                               /*....5678901234*/
    neighListOfCurrentPart = (int *) malloc((*nParts)*sizeof(int));             /*....5678901234*/
  }                                                                             /*....5678901234*/
                                                                                /*.....678901234*/
  /* Initialize neighbor handling for Iterator mode */                          /*.....678901234*/
                                                                                /*.....678901234*/
  if (1 == IterOrLoca)                                                          /*.....678901234*/
  {                                                                             /*.....678901234*/
    ier = (*get_neigh)(&pkim, &zero, &zero, &currentPart, &numOfPartNeigh,      /*...........234*/
                       &neighListOfCurrentPart, &Rij_list);                     /*...........234*/
    ier = KIM_API_get_neigh(pkim, zero, zero, &currentPart, &numOfPartNeigh,    /*.....678901...*/
                            &neighListOfCurrentPart, &Rij_list);                /*.....678901...*/
    /* check for successful initialization */                                   /*.....678901234*/
    if (KIM_STATUS_NEIGH_ITER_INIT_OK != ier)                                   /*.....678901234*/
    {                                                                           /*.....678901234*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);       /*.....678901234*/
      ier = KIM_STATUS_FAIL;                                                    /*.....678901234*/
      return ier;                                                               /*.....678901234*/
    }                                                                           /*.....678901234*/
  }                                                                             /*.....678901234*/
                                                                                /*12345678901234*/
  /* Compute energy and forces */                                               /*12345678901234*/
                                                                                /*12345678901234*/
  /* loop over particles and compute enregy and forces */                       /*12345678901234*/
  i = -1;                                                                       /*....5678901234*/
  while( 1 )                                                                    /*....5678901234*/
  {                                                                             /*....5678901234*/
    /* Set up neighbor list for next part for all NBC methods */                /*....5678901234*/
    if (1 == IterOrLoca)                                                        /*.....678901234*/
    {                                                                           /*.....678901234*/
      /* ITERATOR mode */                                                       /*.....678901234*/
      ier = (*get_neigh)(&pkim, &zero, &one, &currentPart, &numOfPartNeigh,     /*...........234*/
                         &neighListOfCurrentPart, &Rij_list);                   /*...........234*/
      ier = KIM_API_get_neigh(pkim, zero, one, &currentPart, &numOfPartNeigh,   /*.....678901...*/
                              &neighListOfCurrentPart, &Rij_list);              /*.....678901...*/
      if (KIM_STATUS_NEIGH_ITER_PAST_END == ier)                                /*.....678901234*/
      {                                                                         /*.....678901234*/
        /* the end of the list, terminate loop */                               /*.....678901234*/
        break;                                                                  /*.....678901234*/
      }                                                                         /*.....678901234*/
      if (KIM_STATUS_OK > ier)                                                  /*.....678901234*/
      {                                                                         /*.....678901234*/
        /* some sort of problem, exit */                                        /*.....678901234*/
        KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);     /*.....678901234*/
        return ier;                                                             /*.....678901234*/
      }                                                                         /*.....678901234*/
                                                                                /*.....678901234*/
      i = currentPart + model_index_shift;                                      /*...........234*/
      i = currentPart;                                                          /*.....678901...*/
    }                                                                           /*.....678901234*/
    else                                                                        /*.....678901234*/
    {                                                                           /*.....678901234*/
      i++;                                                                      /*.....678901234*/
      if (*nParts <= i)                                                         /*.....678901234*/
      {                                                                         /*.....678901234*/
        /* incremented past end of list, terminate loop */                      /*.....678901234*/
        break;                                                                  /*.....678901234*/
      }                                                                         /*.....678901234*/
                                                                                /*.....678901234*/
      if (3 == NBC)                                                             /*.....678901234*/
      {                                                                         /*.....678901234*/
        /* CLUSTER NBC method */                                                /*.....678901234*/
        numOfPartNeigh = *nParts - (i + 1);                                     /*.....678901234*/
        for (k = 0; k < numOfPartNeigh; ++k)                                    /*.....678901234*/
        {                                                                       /*.....678901234*/
          neighListOfCurrentPart[k] = i + k + 1 - model_index_shift;            /*...........234*/
          neighListOfCurrentPart[k] = i + k + 1;                                /*.....678901...*/
        }                                                                       /*.....678901234*/
        ier = KIM_STATUS_OK;                                                    /*.....678901234*/
      }                                                                         /*.....678901234*/
      else                                                                      /*.....678901234*/
      {                                                                         /*.....678901234*/
        request = i - model_index_shift;                                        /*...........234*/
        ier = (*get_neigh)(&pkim, &one, &request,                               /*...........234*/
                           &currentPart, &numOfPartNeigh,                       /*...........234*/
                           &neighListOfCurrentPart, &Rij_list);                 /*...........234*/
        request = i;                                                            /*.....678901...*/
        ier = KIM_API_get_neigh(pkim, one, request, &currentPart,               /*.....678901...*/
                                &numOfPartNeigh, &neighListOfCurrentPart,       /*.....678901...*/
                                &Rij_list);                                     /*.....678901...*/
        if (KIM_STATUS_OK != ier)                                               /*.....678901234*/
        {                                                                       /*.....678901234*/
          /* some sort of problem, exit */                                      /*.....678901234*/
          KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);   /*.....678901234*/
          ier = KIM_STATUS_FAIL;                                                /*.....678901234*/
          return ier;                                                           /*.....678901234*/
        }                                                                       /*.....678901234*/
      }                                                                         /*.....678901234*/
    }                                                                           /*.....678901234*/
                                                                                /*..............*/
    ++i;                                                                        /*....5.........*/
    if (*nParts <= i)                                                           /*....5.........*/
    {                                                                           /*....5.........*/
      /* incremented past end of list, terminate loop */                        /*....5.........*/
      break;                                                                    /*....5.........*/
    }                                                                           /*....5.........*/
                                                                                /*....5.........*/
    if (3 == NBC)                                                               /*....5.........*/
    {                                                                           /*....5.........*/
      /* CLUSTER NBC method */                                                  /*....5.........*/
      numOfPartNeigh = *nParts - (i + 1);                                       /*....5.........*/
      for (k = 0; k < numOfPartNeigh; ++k)                                      /*....5.........*/
      {                                                                         /*....5.........*/
        neighListOfCurrentPart[k] = i + k + 1;                                  /*....5.........*/
      }                                                                         /*....5.........*/
      ier = KIM_STATUS_OK;                                                      /*....5.........*/
    }                                                                           /*....5.........*/
    else                                                                        /*....5.........*/
    {                                                                           /*....5.........*/
      request = i;                                                              /*....5.........*/
      ier = KIM_API_get_neigh(pkim, one, request, &currentPart,                 /*....5.........*/
                              &numOfPartNeigh, &neighListOfCurrentPart,         /*....5.........*/
                              &Rij_list);                                       /*....5.........*/
      if (KIM_STATUS_OK != ier)                                                 /*....5.........*/
      {                                                                         /*....5.........*/
        /* some sort of problem, exit */                                        /*....5.........*/
        KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh", ier);     /*....5.........*/
        ier = KIM_STATUS_FAIL;                                                  /*....5.........*/
        return ier;                                                             /*....5.........*/
      }                                                                         /*....5.........*/
    }                                                                           /*....5.........*/
                                                                                /*....5678901234*/
  for (i = 0; i < *nParts; ++i)                                                 /*1234..........*/
  {                                                                             /*1234..........*/
    /* loop over the neighbors of particle i */                                 /*12345678901234*/
    for (jj = 0; jj < numOfPartNeigh; ++ jj)                                    /*....5678901234*/
    for (j = i+1; j < *nParts; ++j)                                             /*1234..........*/
    {                                                                           /*12345678901234*/
      j = neighListOfCurrentPart[jj] + model_index_shift;  /* get neighbor ID *//*...........234*/
      j = neighListOfCurrentPart[jj];  /* get neighbor ID */                    /*....5678901...*/
                                                                                /*....5678901234*/
      /* compute relative position vector and squared distance */               /*12345678901234*/
      Rsqij = 0.0;                                                              /*12345678901234*/
      for (k = 0; k < DIM; ++k)                                                 /*12345678901234*/
      {                                                                         /*12345678901234*/
        if (0 != NBC)                                                           /*.......8901234*/
        {                                                                       /*.......8901234*/
          /* all methods except NEIGH_RVEC */                                   /*.......8901234*/
          Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];                       /*.......8901234*/
        }                                                                       /*.......8901234*/
        else                                                                    /*.......8901234*/
        {                                                                       /*.......8901234*/
          /* NEIGH_RVEC_* methods */                                            /*.......8901234*/
          Rij[k] = Rij_list[jj*DIM + k];                                        /*.......8901234*/
        }                                                                       /*.......8901234*/
        Rij[k] = coords[j*DIM + k] - coords[i*DIM + k];                         /*1234567.......*/
                                                                                /*........901234*/
        /* apply periodic boundary conditions if required */                    /*........901234*/
        if (2 == NBC)                                                           /*........901234*/
        {                                                                       /*........901234*/
          if (fabs(Rij[k]) > 0.5*boxSideLengths[k])                             /*........901234*/
          {                                                                     /*........901234*/
            Rij[k] -= (Rij[k]/fabs(Rij[k]))*boxSideLengths[k];                  /*........901234*/
          }                                                                     /*........901234*/
        }                                                                       /*........901234*/
                                                                                /*12345678901234*/
        /* compute squared distance */                                          /*12345678901234*/
        Rsqij += Rij[k]*Rij[k];                                                 /*12345678901234*/
      }                                                                         /*12345678901234*/
                                                                                /*12345678901234*/
      /* compute energy and force */                                            /*12345678901234*/
      if (Rsqij < *cutsq)                                                       /*.........01234*/
      if (Rsqij < cutsq)                                                        /*123456789.....*/
      {                                                                         /*12345678901234*/
        /* particles are interacting ? */                                       /*12345678901234*/
        R = sqrt(Rsqij);                                                        /*12345678901234*/
        if (comp_process_d2Edr2)                                                /*...45678901234*/
        if (comp_hessian)                                                       /*..3...........*/
        {                                                                       /*..345678901234*/
          /* compute pair potential and its derivatives */                      /*..345678901234*/
          calc_phi_d2phi(<FILL_parameter_1>,                                    /*.........01234*/
                         <FILL_parameter_2>,                                    /*.........01234*/
                         <FILL_parameter_3>,                                    /*.........01234*/
                         <FILL_parameter_4>,                                    /*.........01234*/
                         /* FILL as many parameters as needed */                /*.............4*/
                         cutoff, R, &phi, &dphi, &d2phi);                       /*.........01234*/
          calc_phi_d2phi(&<FILL_parameter_1>,                                   /*..3456789.....*/
                         &<FILL_parameter_2>,                                   /*..3456789.....*/
                         &<FILL_parameter_3>,                                   /*..3456789.....*/
                         &<FILL_parameter_4>,                                   /*..3456789.....*/
                         cutoff, R, &phi, &dphi, &d2phi);                       /*..3456789.....*/
                                                                                /*..345678901234*/
          /* compute dEidr */                                                   /*..345678901234*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*......78901234*/
          if (j < numberContrib)                                                /*....56........*/
          {                                                                     /*....5678901234*/
            /* Half mode -- double contribution */                              /*....5678901234*/
            dEidr = dphi;                                                       /*....5678901234*/
            d2Eidr = d2phi;                                                     /*....5678901234*/
          }                                                                     /*....5678901234*/
          else                                                                  /*....5678901234*/
          {                                                                     /*....5678901234*/
            /* Full mode -- regular contribution */                             /*......78901234*/
            dEidr = 0.5*dphi;                                                   /*....5678901234*/
            d2Eidr = 0.5*d2phi;                                                 /*....5678901234*/
          }                                                                     /*....5678901234*/
          /* Half mode -- double contribution */                                /*..34..........*/
          dEidr = dphi;                                                         /*..34..........*/
          d2Eidr = d2phi;                                                       /*..34..........*/
        }                                                                       /*..345678901234*/
        else if (comp_force || comp_process_dEdr)                               /*...45678901234*/
        else if (comp_force || comp_virial || comp_particleVirial)              /*..3...........*/
        if (comp_force)                                                         /*.2............*/
        {                                                                       /*.2345678901234*/
          /* compute pair potential and its derivative */                       /*.2345678901234*/
          calc_phi_dphi(<FILL_parameter_1>,                                     /*.........01234*/
                        <FILL_parameter_2>,                                     /*.........01234*/
                        <FILL_parameter_3>,                                     /*.........01234*/
                        <FILL_parameter_4>,                                     /*.........01234*/
                        /* FILL as many parameters as needed */                 /*.............4*/
                        cutoff, R, &phi, &dphi);                                /*.........01234*/
          calc_phi_dphi(&<FILL_parameter_1>,                                    /*.23456789.....*/
                        &<FILL_parameter_2>,                                    /*.23456789.....*/
                        &<FILL_parameter_3>,                                    /*.23456789.....*/
                        &<FILL_parameter_4>,                                    /*.23456789.....*/
                        cutoff, R, &phi, &dphi);                                /*.23456789.....*/
                                                                                /*.2345678901234*/
          /* compute dEidr */                                                   /*.2345678901234*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*......78901234*/
          if (j < numberContrib)                                                /*....56........*/
          {                                                                     /*....5678901234*/
            /* Half mode -- double contribution */                              /*....5678901234*/
            dEidr = dphi;                                                       /*....5678901234*/
          }                                                                     /*....5678901234*/
          else                                                                  /*....5678901234*/
          {                                                                     /*....5678901234*/
            /* Full mode -- regular contribution */                             /*......78901234*/
            dEidr = 0.5*dphi;                                                   /*....5678901234*/
          }                                                                     /*....5678901234*/
          /* Half mode -- double contribution */                                /*.234..........*/
          dEidr = dphi;                                                         /*.234..........*/
        }                                                                       /*.2345678901234*/
        else                                                                    /*.2345678901234*/
        {                                                                       /*.2345678901234*/
          /* compute just pair potential */                                     /*.2345678901234*/
          calc_phi(<FILL_parameter_1>,                                          /*.........01234*/
                   <FILL_parameter_2>,                                          /*.........01234*/
                   <FILL_parameter_3>,                                          /*.........01234*/
                   <FILL_parameter_4>,                                          /*.........01234*/
                   /* FILL as many parameters as needed */                      /*.............4*/
                   cutoff, R, &phi);                                            /*.........01234*/
          calc_phi(&<FILL_parameter_1>,                                         /*.23456789.....*/
                   &<FILL_parameter_2>,                                         /*.23456789.....*/
                   &<FILL_parameter_3>,                                         /*.23456789.....*/
                   &<FILL_parameter_4>,                                         /*.23456789.....*/
                   cutoff, R, &phi);                                            /*.23456789.....*/
        }                                                                       /*.2345678901234*/
                                                                                /*..............*/
        calc_phi(&<FILL_parameter_1>,                                           /*1.............*/
                 &<FILL_parameter_2>,                                           /*1.............*/
                 &<FILL_parameter_3>,                                           /*1.............*/
                 &<FILL_parameter_4>,                                           /*1.............*/
                 cutoff, R, &phi);                                              /*1.............*/
                                                                                /*.2345678901234*/
        /* contribution to energy */                                            /*.2345678901234*/
        if (comp_particleEnergy)                                                /*.2345678901234*/
        {                                                                       /*.2345678901234*/
          particleEnergy[i] += 0.5*phi;                                         /*.2345678901234*/
          /* if half list add energy for the other particle in the pair */      /*.2345678901234*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*......78901234*/
          if (j < numberContrib)                                                /*....56........*/
          {                                                                     /*....5678901234*/
            particleEnergy[j] += 0.5*phi;                                       /*....5678901234*/
          }                                                                     /*....5678901234*/
          particleEnergy[j] += 0.5*phi;                                         /*.234..........*/
        }                                                                       /*.2345678901234*/
        if (comp_energy)                                                        /*.2345678901234*/
        {                                                                       /*.2345678901234*/
          if ((1 == HalfOrFull) && (j < numberContrib))                         /*......78901234*/
          if (j < numberContrib)                                                /*....56........*/
          {                                                                     /*....5678901234*/
            /* Half mode -- add v to total energy */                            /*....5678901234*/
            *energy += phi;                                                     /*....5678901234*/
          }                                                                     /*....5678901234*/
          else                                                                  /*....5678901234*/
          {                                                                     /*....5678901234*/
            /* Full mode -- add half v to total energy */                       /*......78901234*/
            *energy += 0.5*phi;                                                 /*....5678901234*/
          }                                                                     /*....5678901234*/
          /* Half mode -- add v to total energy */                              /*.234..........*/
          *energy += phi;                                                       /*.234..........*/
        }                                                                       /*.2345678901234*/
        *energy += phi;                                                         /*1.............*/
                                                                                /*.2345678901234*/
        /* contribution to process_dEdr */                                      /*...45678901234*/
        if (comp_process_dEdr)                                                  /*...45678901234*/
        {                                                                       /*...45678901234*/
          ier = KIM_API_process_dEdr(km, &dEidr, &R, &pRij, &i, &j);            /*...45678901234*/
        }                                                                       /*...45678901234*/
                                                                                /*...45678901234*/
        /* contribution to process_d2Edr2 */                                    /*...45678901234*/
        if (comp_process_d2Edr2)                                                /*...45678901234*/
        {                                                                       /*...45678901234*/
          R_pairs[0] = R_pairs[1] = R;                                          /*...45678901234*/
          Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];                           /*...45678901234*/
          Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];                           /*...45678901234*/
          Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];                           /*...45678901234*/
          i_pairs[0] = i_pairs[1] = i;                                          /*...45678901234*/
          j_pairs[0] = j_pairs[1] = j;                                          /*...45678901234*/
                                                                                /*...45678901234*/
          ier = KIM_API_process_d2Edr2(km, &d2Eidr, &pR_pairs, &pRij_pairs,     /*...45678901234*/
                                       &pi_pairs, &pj_pairs);                   /*...45678901234*/
        }                                                                       /*...45678901234*/
                                                                                /*...45678901234*/
        if (comp_virial)                                                        /*..3...........*/
        {                                                                       /*..3...........*/
          fac = dEidr/R;                                                        /*..3...........*/
          virial[0] += fac*Rij[0]*Rij[0];                                       /*..3...........*/
          virial[1] += fac*Rij[1]*Rij[1];                                       /*..3...........*/
          virial[2] += fac*Rij[2]*Rij[2];                                       /*..3...........*/
          virial[3] += fac*Rij[1]*Rij[2];                                       /*..3...........*/
          virial[4] += fac*Rij[2]*Rij[0];                                       /*..3...........*/
          virial[5] += fac*Rij[0]*Rij[1];                                       /*..3...........*/
        }                                                                       /*..3...........*/
                                                                                /*..3...........*/
        if (comp_particleVirial)                                                /*..3...........*/
        {                                                                       /*..3...........*/
          for (k = 0; k < *nParts; ++k)                                         /*..3...........*/
          {                                                                     /*..3...........*/
            for (m = 0; m < 6; ++m)                                             /*..3...........*/
            {                                                                   /*..3...........*/
              fac = dEidr/R;                                                    /*..3...........*/
              particleVirial[k*6 + 0] += 0.5 * fac*Rij[0]*Rij[0];               /*..3...........*/
              particleVirial[k*6 + 1] += 0.5 * fac*Rij[1]*Rij[1];               /*..3...........*/
              particleVirial[k*6 + 2] += 0.5 * fac*Rij[2]*Rij[2];               /*..3...........*/
              particleVirial[k*6 + 3] += 0.5 * fac*Rij[1]*Rij[2];               /*..3...........*/
              particleVirial[k*6 + 4] += 0.5 * fac*Rij[2]*Rij[0];               /*..3...........*/
              particleVirial[k*6 + 5] += 0.5 * fac*Rij[0]*Rij[1];               /*..3...........*/
              particleVirial[m*6 + 0] += 0.5 * fac*Rij[0]*Rij[0];               /*..3...........*/
              particleVirial[m*6 + 1] += 0.5 * fac*Rij[1]*Rij[1];               /*..3...........*/
              particleVirial[m*6 + 2] += 0.5 * fac*Rij[2]*Rij[2];               /*..3...........*/
              particleVirial[m*6 + 3] += 0.5 * fac*Rij[1]*Rij[2];               /*..3...........*/
              particleVirial[m*6 + 4] += 0.5 * fac*Rij[2]*Rij[0];               /*..3...........*/
              particleVirial[m*6 + 5] += 0.5 * fac*Rij[0]*Rij[1];               /*..3...........*/
            }                                                                   /*..3...........*/
          }                                                                     /*..3...........*/
        }                                                                       /*..3...........*/
                                                                                /*..3...........*/
        if (comp_hessian)                                                       /*..3...........*/
        {                                                                       /*..3...........*/
          fac1 = dEidr/R;                                                       /*..3...........*/
          fac2 = dEidr/(R*R*R);                                                 /*..3...........*/
          /* dEidr contribution */                                              /*..3...........*/
          stiff[0][0] = -fac2 * Rij[0] * Rij[0] + fac1;                         /*..3...........*/
          stiff[1][1] = -fac2 * Rij[1] * Rij[1] + fac1;                         /*..3...........*/
          stiff[2][2] = -fac2 * Rij[2] * Rij[2] + fac1;                         /*..3...........*/
          stiff[1][2] = stiff[2][1] = -fac2 * Rij[1] * Rij[2];                  /*..3...........*/
          stiff[0][2] = stiff[2][0] = -fac2 * Rij[0] * Rij[2];                  /*..3...........*/
          stiff[0][1] = stiff[1][0] = -fac2 * Rij[0] * Rij[1];                  /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[i*(*nParts)*DIM*DIM + i*DIM*DIM + k*DIM + m]              /*..3...........*/
                  += stiff[k][m];                                               /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[i*(*nParts)*DIM*DIM + j*DIM*DIM + k*DIM + m]              /*..3...........*/
                  -= stiff[k][m];                                               /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[j*(*nParts)*DIM*DIM + i*DIM*DIM + k*DIM + m]              /*..3...........*/
                  -= stiff[k][m];                                               /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[j*(*nParts)*DIM*DIM + j*DIM*DIM + k*DIM + m]              /*..3...........*/
                  += stiff[k][m];                                               /*..3...........*/
                                                                                /*..3...........*/
          /* d2Eidr2 contribution */                                            /*..3...........*/
          fac3 = d2Eidr/(R*R);                                                  /*..3...........*/
          stiff[0][0] = fac3 * Rij[0] * Rij[0];                                 /*..3...........*/
          stiff[1][1] = fac3 * Rij[1] * Rij[1];                                 /*..3...........*/
          stiff[2][2] = fac3 * Rij[2] * Rij[2];                                 /*..3...........*/
          stiff[1][2] = stiff[2][1] = fac3 * Rij[1] * Rij[2];                   /*..3...........*/
          stiff[0][2] = stiff[2][0] = fac3 * Rij[0] * Rij[2];                   /*..3...........*/
          stiff[0][1] = stiff[1][0] = fac3 * Rij[0] * Rij[1];                   /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[i*(*nParts)*DIM*DIM + i*DIM*DIM + k*DIM + m]              /*..3...........*/
                  += stiff[k][m];                                               /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[i*(*nParts)*DIM*DIM + j*DIM*DIM + k*DIM + m]              /*..3...........*/
                  -= stiff[k][m];                                               /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[j*(*nParts)*DIM*DIM + i*DIM*DIM + k*DIM + m]              /*..3...........*/
                  -= stiff[k][m];                                               /*..3...........*/
          for (k = 0; k < 3; ++k)                                               /*..3...........*/
            for (m = 0; m < 3; ++m)                                             /*..3...........*/
              hessian[j*(*nParts)*DIM*DIM + j*DIM*DIM + k*DIM + m]              /*..3...........*/
                  += stiff[k][m];                                               /*..3...........*/
        }                                                                       /*..3...........*/
                                                                                /*..3...........*/
        /* contribution to forces */                                            /*.2345678901234*/
        if (comp_force)                                                         /*.2345678901234*/
        {                                                                       /*.2345678901234*/
          for (k = 0; k < DIM; ++k)                                             /*.2345678901234*/
          {                                                                     /*.2345678901234*/
            force[i*DIM + k] += dEidr*Rij[k]/R;  /* accumulate force on i */    /*.2345678901234*/
            force[j*DIM + k] -= dEidr*Rij[k]/R;  /* accumulate force on j */    /*.2345678901234*/
          }                                                                     /*.2345678901234*/
        }                                                                       /*.2345678901234*/
      }                                                                         /*12345678901234*/
    }  /* loop on jj */                                                         /*12345678901234*/
  }  /* infinite while loop (terminated by break statements above) */           /*12345678901234*/
                                                                                /*....5678901234*/
  /* Free temporary storage */                                                  /*....5678901234*/
  if (3 == NBC)                                                                 /*....5678901234*/
  {                                                                             /*....5678901234*/
    free(neighListOfCurrentPart);                                               /*....5678901234*/
  }                                                                             /*....5678901234*/
                                                                                /*12345678901234*/
  /* everything is great */                                                     /*12345678901234*/
  ier = KIM_STATUS_OK;                                                          /*12345678901234*/
                                                                                /*12345678901234*/
  return ier;                                                                   /*12345678901234*/
}                                                                               /*12345678901234*/
                                                                                /*12345678901234*/
/* Initialization function */                                                   /*12345678901234*/
int model_driver_init(void *km, char* paramfile_names, int* nmstrlen,           /*............34*/
                      int* numparamfiles)                                       /*............34*/
int model_init(void* km)                                                        /*123456789012..*/
{                                                                               /*12345678901234*/
  /* KIM variables */                                                           /*12345678901234*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*12345678901234*/
  char* paramfile1name;                                                         /*............34*/
  char* paramfile2name;                                                         /*.............4*/
  <FILL as many file name pointers as needed>                                   /*.............4*/
                                                                                /*12345678901234*/
  /* Local variables */                                                         /*12345678901234*/
  FILE* fid;                                                                    /*............34*/
  double cutoff;                                                                /*.........01234*/
  double <FILL_parameter_1>;                                                    /*.........01234*/
  double <FILL_parameter_2>;                                                    /*.........01234*/
  double <FILL_parameter_3>;                                                    /*.........01234*/
  double <FILL_parameter_4>;                                                    /*.............4*/
  /* FILL as many parameters as needed */                                       /*.............4*/
  double* model_cutoff;                                                         /*12345678901234*/
  double* model_Pcutoff;                                                        /*.........01...*/
  double* model_cutsq;                                                          /*.........01...*/
  double* model_epsilon;                                                        /*.........01...*/
  double* model_C;                                                              /*.........01...*/
  double* model_Rzero;                                                          /*.........01...*/
  double* model_shift;                                                          /*.........01...*/
  int ier;                                                                      /*12345678901234*/
  double dummy;                                                                 /*.........0123.*/
  struct model_buffer* buffer;                                                  /*...........234*/
  const char* NBCstr;                                                           /*...........234*/
                                                                                /*12345678901234*/
  /* set paramfile1name */                                                      /*............34*/
  if (*numparamfiles != 1)                                                      /*............34*/
  {                                                                             /*............34*/
    ier = KIM_STATUS_FAIL;                                                      /*............34*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*............34*/
                         "Incorrect number of parameter files.", ier);          /*............34*/
    return ier;                                                                 /*............34*/
  }                                                                             /*............34*/
  paramfile1name = paramfile_names;                                             /*............34*/
  paramfile2name = &(paramfile_names[1*(*nmstrlen)]);                           /*.............4*/
  <FILL as many file name pointers as needed>                                   /*.............4*/
                                                                                /*............34*/
  /* store pointer to functions in KIM object */                                /*.........01234*/
  KIM_API_setm_method(pkim, &ier, 3*4,                                          /*.........01234*/
                      "compute", 1, &compute, 1,                                /*.........01234*/
                      "reinit",  1, &reinit,  1,                                /*.........01234*/
                      "destroy", 1, &destroy, 1);                               /*.........01234*/
  if (KIM_STATUS_OK > ier)                                                      /*.........01234*/
  {                                                                             /*.........01234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);         /*.........01234*/
    return ier;                                                                 /*.........01234*/
  }                                                                             /*.........01234*/
  /* store pointer to function in KIM object */                                 /*123456789.....*/
  ier = KIM_API_set_method(pkim, "compute", 1, (func_ptr) &compute);            /*123456789.....*/
  if (KIM_STATUS_OK > ier)                                                      /*123456789.....*/
  {                                                                             /*123456789.....*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_data", ier);          /*123456789.....*/
    return ier;                                                                 /*123456789.....*/
  }                                                                             /*123456789.....*/
                                                                                /*12345678901234*/
  /* Read in model parameters from parameter file */                            /*............34*/
  fid = fopen(paramfile1name, "r");                                             /*............34*/
  if (fid == NULL)                                                              /*............34*/
  {                                                                             /*............34*/
    ier = KIM_STATUS_FAIL;                                                      /*............34*/
    KIM_API_report_error(                                                       /*............34*/
        __LINE__, __FILE__,                                                     /*............34*/
        "Unable to open parameter file for Morse parameters", ier);             /*............34*/
    return ier;                                                                 /*............34*/
  }                                                                             /*............34*/
                                                                                /*............34*/
  ier = fscanf(fid, "%lf \n%lf \n%lf \n%lf",                                    /*............34*/
               &cutoff,  /* cutoff distance in angstroms */                     /*............34*/
               &epsilon,  /* Morse epsilon in eV */                             /*............3.*/
               &C,  /* Morse C in 1/Angstroms */                                /*............3.*/
               &Rzero  /* Morse Rzero in Angstroms */                           /*............3.*/
               &<FILL_parameter_1>,                                             /*.............4*/
               &<FILL_parameter_2>,                                             /*.............4*/
               &<FILL_parameter_3>,                                             /*.............4*/
               &<FILL_parameter_4>,                                             /*.............4*/
               /* FILL as many parameters as needed */                          /*.............4*/
               );                                                               /*............34*/
  fclose(fid);                                                                  /*............34*/
                                                                                /*............34*/
  /* check that we read the right number of parameters */                       /*............34*/
  if (<FILL_number_of_parameters> != ier)                                       /*............34*/
  {                                                                             /*............34*/
    ier = KIM_STATUS_FAIL;                                                      /*............34*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*............34*/
                         "Unable to read all parameters", ier);                 /*............34*/
    return ier;                                                                 /*............34*/
  }                                                                             /*............34*/
                                                                                /*............34*/
  /* FILL process the remaining parameter files */                              /*.............4*/
                                                                                /*.............4*/
  /* set parameters */                                                          /*.........012..*/
  cutoff = 8.15;  /* Angstroms */                                               /*.........012..*/
  epsilon = -0.0134783698072604;  /* eV */                                      /*.........012..*/
  C = 1.545;  /* 1/Angstroms */                                                 /*.........012..*/
  Rzero = 3.786;  /* Angstroms */                                               /*.........012..*/
                                                                                /*..........12..*/
  /* convert to appropriate units */                                            /*..........1234*/
  cutoff *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",        /*..........1234*/
                                        1.0, 0.0,  0.0, 0.0, 0.0, &ier);        /*..........1234*/
  if (KIM_STATUS_OK > ier)                                                      /*..........1234*/
  {                                                                             /*..........1234*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*..........1234*/
                         "KIM_API_convert_to_act_unit", ier);                   /*..........1234*/
    return ier;                                                                 /*..........1234*/
  }                                                                             /*..........1234*/
                                                                                /*..........1234*/
  epsilon *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",       /*..........123.*/
                                         0.0, 1.0,  0.0, 0.0, 0.0, &ier);       /*..........123.*/
  if (KIM_STATUS_OK > ier)                                                      /*..........123.*/
  {                                                                             /*..........123.*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*..........123.*/
                         "KIM_API_convert_to_act_unit", ier);                   /*..........123.*/
    return ier;                                                                 /*..........123.*/
  }                                                                             /*..........123.*/
                                                                                /*..........123.*/
  C *= KIM_API_convert_to_act_unit(pkim, "A",  "eV", "e", "K", "ps",            /*..........123.*/
                                   -1.0, 0.0,  0.0, 0.0, 0.0, &ier);            /*..........123.*/
  if (KIM_STATUS_OK > ier)                                                      /*..........123.*/
  {                                                                             /*..........123.*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*..........123.*/
                         "KIM_API_convert_to_act_unit", ier);                   /*..........123.*/
    return ier;                                                                 /*..........123.*/
  }                                                                             /*..........123.*/
                                                                                /*..........123.*/
  Rzero *= KIM_API_convert_to_act_unit(pkim, "A", "eV", "e", "K", "ps",         /*..........123.*/
                                       1.0, 0.0,  0.0, 0.0, 0.0, &ier);         /*..........123.*/
  if (KIM_STATUS_OK > ier)                                                      /*..........123.*/
  {                                                                             /*..........123.*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*..........123.*/
                         "KIM_API_convert_to_act_unit", ier);                   /*..........123.*/
    return ier;                                                                 /*..........123.*/
  }                                                                             /*..........123.*/
                                                                                /*..............*/
  <FILL_parameter_1> *= KIM_API_convert_to_act_unit(                            /*.............4*/
      pkim, "A", "eV", "e", "K", "ps",                                          /*.............4*/
      <FILL exponents (5) for parameter 1>, &ier);                              /*.............4*/
  if (KIM_STATUS_OK > ier)                                                      /*.............4*/
  {                                                                             /*.............4*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*.............4*/
                         "KIM_API_convert_to_act_unit", ier);                   /*.............4*/
    return ier;                                                                 /*.............4*/
  }                                                                             /*.............4*/
                                                                                /*.............4*/
  <FILL_parameter_2> *= KIM_API_convert_to_act_unit(                            /*.............4*/
      pkim, "A", "eV", "e", "K", "ps",                                          /*.............4*/
      <FILL exponents (5) for parameter 2>, &ier);                              /*.............4*/
  if (KIM_STATUS_OK > ier)                                                      /*.............4*/
  {                                                                             /*.............4*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*.............4*/
                         "KIM_API_convert_to_act_unit", ier);                   /*.............4*/
    return ier;                                                                 /*.............4*/
  }                                                                             /*.............4*/
                                                                                /*.............4*/
  <FILL_parameter_3> *= KIM_API_convert_to_act_unit(                            /*.............4*/
      pkim, "A", "eV", "e", "K", "ps",                                          /*.............4*/
      <FILL exponents (5) for parameter 3>, &ier);                              /*.............4*/
  if (KIM_STATUS_OK > ier)                                                      /*.............4*/
  {                                                                             /*.............4*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*.............4*/
                         "KIM_API_convert_to_act_unit", ier);                   /*.............4*/
    return ier;                                                                 /*.............4*/
  }                                                                             /*.............4*/
                                                                                /*.............4*/
  <FILL_parameter_4> *= KIM_API_convert_to_act_unit(                            /*.............4*/
      pkim, "A", "eV", "e", "K", "ps",                                          /*.............4*/
      <FILL exponents (5) for parameter 4>, &ier);                              /*.............4*/
  if (KIM_STATUS_OK > ier)                                                      /*.............4*/
  {                                                                             /*.............4*/
    KIM_API_report_error(__LINE__, __FILE__,                                    /*.............4*/
                         "KIM_API_convert_to_act_unit", ier);                   /*.............4*/
    return ier;                                                                 /*.............4*/
  }                                                                             /*.............4*/
                                                                                /*.............4*/
  /* FILL as many parameters as necessary */                                    /*.............4*/
                                                                                /*.........01234*/
  /* store model cutoff in KIM object */                                        /*12345678901234*/
  model_cutoff = (double*) KIM_API_get_data(pkim, "cutoff", &ier);              /*12345678901234*/
  if (KIM_STATUS_OK > ier)                                                      /*12345678901234*/
  {                                                                             /*12345678901234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);          /*12345678901234*/
    return ier;                                                                 /*12345678901234*/
  }                                                                             /*12345678901234*/
  *model_cutoff = cutoff;                                                       /*.........01234*/
  *model_cutoff = CUTOFF;                                                       /*123456789.....*/
                                                                                /*.........01234*/
  /* allocate buffer */                                                         /*...........234*/
  buffer = (struct model_buffer*) malloc(sizeof(struct model_buffer));          /*...........234*/
  if (NULL == buffer)                                                           /*...........234*/
  {                                                                             /*...........234*/
    ier = KIM_STATUS_FAIL;                                                      /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  /* setup buffer */                                                            /*...........234*/
  /* set value of parameters */                                                 /*...........234*/
  buffer->Pcutoff = *model_cutoff;                                              /*...........234*/
  buffer->cutsq = (*model_cutoff)*(*model_cutoff);                              /*...........234*/
  buffer-><FILL_parameter_1> = <FILL_parameter_1>;                              /*...........234*/
  buffer-><FILL_parameter_2> = <FILL_parameter_2>;                              /*...........234*/
  buffer-><FILL_parameter_3> = <FILL_parameter_3>;                              /*...........234*/
  /* set value of parameter shift */                                            /*...........23.*/
  dummy = 0.0;                                                                  /*...........23.*/
  /* call calc_phi with r=cutoff and shift=0.0 */                               /*...........23.*/
  calc_phi(&(buffer->epsilon),                                                  /*...........23.*/
           &(buffer->C),                                                        /*...........23.*/
           &(buffer->Rzero),                                                    /*...........23.*/
           &dummy,                                                              /*...........23.*/
           model_cutoff, *model_cutoff, &(buffer->shift));                      /*...........23.*/
  /* set shift to -shift */                                                     /*...........23.*/
  buffer->shift = -buffer->shift;                                               /*...........23.*/
  buffer-><FILL_parameter_4> = <FILL_parameter_4>;                              /*.............4*/
  /* FILL as many parameters as needed */                                       /*.............4*/
                                                                                /*...........234*/
  /* Determine neighbor list boundary condition (NBC) */                        /*...........234*/
  ier = KIM_API_get_NBC_method(pkim, &NBCstr);                                  /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_NBC_method", ier);    /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
  if ((!strcmp("NEIGH_RVEC_H",NBCstr)) || (!strcmp("NEIGH_RVEC_F",NBCstr)))     /*...........234*/
  {                                                                             /*...........234*/
    buffer->NBC = 0;                                                            /*...........234*/
  }                                                                             /*...........234*/
  else if ((!strcmp("NEIGH_PURE_H",NBCstr)) || (!strcmp("NEIGH_PURE_F",NBCstr)))/*...........234*/
  {                                                                             /*...........234*/
    buffer->NBC = 1;                                                            /*...........234*/
  }                                                                             /*...........234*/
  else if ((!strcmp("MI_OPBC_H",NBCstr)) || (!strcmp("MI_OPBC_F",NBCstr)))      /*...........234*/
  {                                                                             /*...........234*/
    buffer->NBC = 2;                                                            /*...........234*/
  }                                                                             /*...........234*/
  else if (!strcmp("CLUSTER",NBCstr))                                           /*...........234*/
  {                                                                             /*...........234*/
    buffer->NBC = 3;                                                            /*...........234*/
  }                                                                             /*...........234*/
  else                                                                          /*...........234*/
  {                                                                             /*...........234*/
    ier = KIM_STATUS_FAIL;                                                      /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "Unknown NBC method", ier);        /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  /* Determine if Half or Full neighbor lists are being used */                 /*...........234*/
  /*****************************/                                               /*...........234*/
  /* HalfOrFull = 1 -- Half    */                                               /*...........234*/
  /*            = 2 -- Full    */                                               /*...........234*/
  /*****************************/                                               /*...........234*/
  if (KIM_API_is_half_neighbors(pkim, &ier))                                    /*...........234*/
  {                                                                             /*...........234*/
    buffer->HalfOrFull = 1;                                                     /*...........234*/
  }                                                                             /*...........234*/
  else                                                                          /*...........234*/
  {                                                                             /*...........234*/
    buffer->HalfOrFull = 2;                                                     /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  /* determine neighbor list handling mode */                                   /*...........234*/
  if (buffer->NBC != 3)                                                         /*...........234*/
  {                                                                             /*...........234*/
    /******************************/                                            /*...........234*/
    /* IterOrLoca = 1 -- Iterator */                                            /*...........234*/
    /*            = 2 -- Locator  */                                            /*...........234*/
    /******************************/                                            /*...........234*/
    buffer->IterOrLoca = KIM_API_get_neigh_mode(pkim, &ier);                    /*...........234*/
    if (KIM_STATUS_OK > ier)                                                    /*...........234*/
    {                                                                           /*...........234*/
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_neigh_mode", ier);  /*...........234*/
      return ier;                                                               /*...........234*/
    }                                                                           /*...........234*/
    if ((buffer->IterOrLoca != 1) && (buffer->IterOrLoca != 2))                 /*...........234*/
    {                                                                           /*...........234*/
      ier = KIM_STATUS_FAIL;                                                    /*...........234*/
      KIM_API_report_error(__LINE__, __FILE__,                                  /*...........234*/
                           "Unsupported IterOrLoca mode", ier);                 /*...........234*/
      return ier;                                                               /*...........234*/
    }                                                                           /*...........234*/
  }                                                                             /*...........234*/
  else                                                                          /*...........234*/
  {                                                                             /*...........234*/
    buffer->IterOrLoca = 2;  /* for CLUSTER NBC */                              /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  buffer->model_index_shift = KIM_API_get_model_index_shift(pkim);              /*...........234*/
                                                                                /*...........234*/
  KIM_API_getm_index(                                                           /*...........234*/
      pkim, &ier, 12*3,                                                         /*...........234*/
      "cutoff",                      &(buffer->cutoff_ind),                 1,  /*...........234*/
      "numberOfParticles",           &(buffer->numberOfParticles_ind),      1,  /*...........234*/
      "particleSpecies",             &(buffer->particleSpecies_ind),        1,  /*...........234*/
      "numberContributingParticles", &(buffer->numberContribParticles_ind), 1,  /*...........234*/
      "coordinates",                 &(buffer->coordinates_ind),            1,  /*...........234*/
      "get_neigh",                   &(buffer->get_neigh_ind),              1,  /*...........234*/
      "boxSideLengths",              &(buffer->boxSideLengths_ind),         1,  /*...........234*/
      "energy",                      &(buffer->energy_ind),                 1,  /*...........234*/
      "forces",                      &(buffer->forces_ind),                 1,  /*...........234*/
      "particleEnergy",              &(buffer->particleEnergy_ind),         1,  /*...........234*/
      "process_dEdr",                &(buffer->process_dEdr_ind),           1,  /*...........234*/
      "process_d2Edr2",              &(buffer->process_d2Edr2_ind),         1   /*...........234*/
                     );                                                         /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_index", ier);        /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
  /* end setup buffer */                                                        /*...........234*/
                                                                                /*...........234*/
  /* store in model buffer */                                                   /*...........234*/
  KIM_API_set_model_buffer(pkim, (void*) buffer, &ier);                         /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_set_model_buffer", ier);  /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  /* set pointers to parameters in KIM object */                                /*...........234*/
  KIM_API_setm_data(                                                            /*...........234*/
      pkim, &ier, <FILL_number_of_parameters_plus_two>*4,                       /*...........234*/
      "PARAM_FREE_cutoff", 1, &(buffer->Pcutoff), 1,                            /*...........234*/
      "PARAM_FIXED_cutsq", 1, &(buffer->cutsq), 1,                              /*...........234*/
      "<FILL_parameter_type_parameter_1>", 1, &(buffer-><FILL_parameter_1>), 1, /*...........234*/
      "<FILL_parameter_type_parameter_2>", 1, &(buffer-><FILL_parameter_2>), 1, /*...........234*/
      "<FILL_parameter_type_parameter_3>", 1, &(buffer-><FILL_parameter_3>), 1, /*...........234*/
      "<FILL_parameter_type_parameter_4>", 1, &(buffer-><FILL_parameter_4>), 1  /*...........234*/
      /* FILL as many parameters as needed */                                   /*.............4*/
                    );                                                          /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_setm_data", ier);         /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*..............*/
  /* allocate memory for parameters */                                          /*.........01...*/
  model_Pcutoff = (double*) malloc(1*sizeof(double));                           /*.........01...*/
  if (NULL == model_Pcutoff)                                                    /*.........01...*/
  {                                                                             /*.........01...*/
    ier = KIM_STATUS_FAIL;                                                      /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
  model_cutsq = (double*) malloc(1*sizeof(double));                             /*.........01...*/
  if (NULL == model_cutsq)                                                      /*.........01...*/
  {                                                                             /*.........01...*/
    ier = KIM_STATUS_FAIL;                                                      /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
  model_epsilon = (double*) malloc(1*sizeof(double));                           /*.........01...*/
  if (NULL == model_epsilon)                                                    /*.........01...*/
  {                                                                             /*.........01...*/
    ier = KIM_STATUS_FAIL;                                                      /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
  model_C = (double*) malloc(1*sizeof(double));                                 /*.........01...*/
  if (NULL == model_C)                                                          /*.........01...*/
  {                                                                             /*.........01...*/
    ier = KIM_STATUS_FAIL;                                                      /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
  model_Rzero = (double*) malloc(1*sizeof(double));                             /*.........01...*/
  if (NULL == model_Rzero)                                                      /*.........01...*/
  {                                                                             /*.........01...*/
    ier = KIM_STATUS_FAIL;                                                      /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
  model_shift = (double*) malloc(1*sizeof(double));                             /*.........01...*/
  if (NULL == model_shift)                                                      /*.........01...*/
  {                                                                             /*.........01...*/
    ier = KIM_STATUS_FAIL;                                                      /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "malloc", ier);                    /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
                                                                                /*.........01...*/
  /* store parameters in KIM object */                                          /*.........01...*/
  KIM_API_setm_data(pkim, &ier, 6*4,                                            /*.........01...*/
                    "PARAM_FREE_cutoff",  1, model_Pcutoff, 1,                  /*.........01...*/
                    "PARAM_FIXED_cutsq",  1, model_cutsq,   1,                  /*.........01...*/
                    "PARAM_FREE_epsilon", 1, model_epsilon, 1,                  /*.........01...*/
                    "PARAM_FREE_C",       1, model_C,       1,                  /*.........01...*/
                    "PARAM_FREE_Rzero",   1, model_Rzero,   1,                  /*.........01...*/
                    "PARAM_FIXED_shift",  1, model_shift,   1                   /*.........01...*/
                    );                                                          /*.........01...*/
                                                                                /*.........01...*/
  /* set value of parameters */                                                 /*.........01...*/
  *model_Pcutoff = *model_cutoff;                                               /*.........01...*/
  *model_cutsq = (*model_cutoff)*(*model_cutoff);                               /*.........01...*/
  *model_epsilon = epsilon;                                                     /*.........01...*/
  *model_C = C;                                                                 /*.........01...*/
  *model_Rzero = Rzero;                                                         /*.........01...*/
  /* set value of parameter shift */                                            /*.........01...*/
  dummy = 0.0;                                                                  /*.........01...*/
  /* call calc_phi with r=cutoff and shift=0.0 */                               /*.........01...*/
  calc_phi(model_epsilon, model_C, model_Rzero, &dummy,                         /*.........01...*/
           model_cutoff, *model_cutoff, model_shift);                           /*.........01...*/
  /* set shift to -shift */                                                     /*.........01...*/
  *model_shift = -(*model_shift);                                               /*.........01...*/
                                                                                /*12345678901234*/
  return KIM_STATUS_OK;                                                         /*12345678901234*/
}                                                                               /*12345678901234*/
                                                                                /*.........01234*/
/* Reinitialization function */                                                 /*.........01234*/
static int reinit(void *km)                                                     /*.........01234*/
{                                                                               /*.........01234*/
  /* Local variables */                                                         /*.........01234*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*.........01234*/
  int ier;                                                                      /*.........01234*/
  double *cutoff;                                                               /*.........01234*/
  double dummy;                                                                 /*.........0123.*/
  struct model_buffer* buffer;                                                  /*...........234*/
  double *param_cutoff;                                                         /*.........01...*/
  double *param_cutsq;                                                          /*.........01...*/
  double *param_epsilon;                                                        /*.........01...*/
  double *param_C;                                                              /*.........01...*/
  double *param_Rzero;                                                          /*.........01...*/
  double *param_shift;                                                          /*.........01...*/
                                                                                /*.........01234*/
  /* get buffer from KIM object */                                              /*...........234*/
  buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);         /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);  /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*..............*/
  /* get parameters from KIM object */                                          /*.........01...*/
  KIM_API_getm_data(pkim, &ier, 6*3,                                            /*.........01...*/
                    "PARAM_FREE_cutoff",  &param_cutoff,  1,                    /*.........01...*/
                    "PARAM_FIXED_cutsq",  &param_cutsq,   1,                    /*.........01...*/
                    "PARAM_FREE_epsilon", &param_epsilon, 1,                    /*.........01...*/
                    "PARAM_FREE_C",       &param_C,       1,                    /*.........01...*/
                    "PARAM_FREE_Rzero",   &param_Rzero,   1,                    /*.........01...*/
                    "PARAM_FIXED_shift",  &param_shift,   1                     /*.........01...*/
                    );                                                          /*.........01...*/
  if (KIM_STATUS_OK > ier)                                                      /*.........01...*/
  {                                                                             /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);         /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
                                                                                /*.........01234*/
  /* set new values in KIM object     */                                        /*.........01234*/
  /*                                  */                                        /*.........01234*/
  /* store model cutoff in KIM object */                                        /*.........01234*/
  cutoff = KIM_API_get_data(pkim, "cutoff", &ier);                              /*.........01234*/
  if (KIM_STATUS_OK > ier)                                                      /*.........01234*/
  {                                                                             /*.........01234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data", ier);          /*.........01234*/
    return ier;                                                                 /*.........01234*/
  }                                                                             /*.........01234*/
  *cutoff = buffer->Pcutoff;                                                    /*...........234*/
  *cutoff = *param_cutoff;                                                      /*.........01...*/
                                                                                /*.........01234*/
  /* set value of parameter cutsq */                                            /*.........01234*/
  buffer->cutsq = (*cutoff)*(*cutoff);                                          /*...........234*/
  *param_cutsq = (*cutoff)*(*cutoff);                                           /*.........01...*/
                                                                                /*.........01234*/
  /* set value of parameter shift */                                            /*.........0123.*/
  dummy = 0.0;                                                                  /*.........0123.*/
  /* call calc_phi with r=cutoff and shift=0.0 */                               /*.........0123.*/
  calc_phi(&(buffer->epsilon),                                                  /*...........23.*/
           &(buffer->C),                                                        /*...........23.*/
           &(buffer->Rzero),                                                    /*...........23.*/
           &dummy,                                                              /*...........23.*/
           cutoff, *cutoff, &(buffer->shift));                                  /*...........23.*/
  /* set shift to -shift */                                                     /*...........23.*/
  buffer->shift = -buffer->shift;                                               /*...........23.*/
                                                                                /*..............*/
  calc_phi(param_epsilon, param_C, param_Rzero, &dummy, cutoff, *cutoff,        /*.........01...*/
           param_shift);                                                        /*.........01...*/
  /* set shift to -shift */                                                     /*.........01...*/
  *param_shift = -(*param_shift);                                               /*.........01...*/
                                                                                /*..............*/
  /* FILL: recompute any FIXED parameters that need to be */                    /*.............4*/
                                                                                /*.........01234*/
  ier = KIM_STATUS_OK;                                                          /*.........01234*/
  return ier;                                                                   /*.........01234*/
}                                                                               /*.........01234*/
                                                                                /*.........01234*/
/* destroy function */                                                          /*.........01234*/
static int destroy(void *km)                                                    /*.........01234*/
{                                                                               /*.........01234*/
  /* Local variables */                                                         /*.........01234*/
  intptr_t* pkim = *((intptr_t**) km);                                          /*.........01234*/
  struct model_buffer* buffer;                                                  /*...........234*/
  int ier;                                                                      /*.........01234*/
  double *param_cutoff;                                                         /*.........01...*/
  double *param_cutsq;                                                          /*.........01...*/
  double *param_epsilon;                                                        /*.........01...*/
  double *param_C;                                                              /*.........01...*/
  double *param_Rzero;                                                          /*.........01...*/
  double *param_shift;                                                          /*.........01...*/
                                                                                /*.........01234*/
  /* get model buffer from KIM object */                                        /*...........234*/
  buffer = (struct model_buffer*) KIM_API_get_model_buffer(pkim, &ier);         /*...........234*/
  if (KIM_STATUS_OK > ier)                                                      /*...........234*/
  {                                                                             /*...........234*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_model_buffer", ier);  /*...........234*/
    return ier;                                                                 /*...........234*/
  }                                                                             /*...........234*/
                                                                                /*...........234*/
  /* free the buffer */                                                         /*...........234*/
  free(buffer);                                                                 /*...........234*/
                                                                                /*..............*/
  /* get parameters from KIM object */                                          /*.........01...*/
  KIM_API_getm_data(pkim, &ier, 6*3,                                            /*.........01...*/
                    "PARAM_FREE_cutoff",  &param_cutoff,  1,                    /*.........01...*/
                    "PARAM_FIXED_cutsq",  &param_cutsq,   1,                    /*.........01...*/
                    "PARAM_FREE_epsilon", &param_epsilon, 1,                    /*.........01...*/
                    "PARAM_FREE_C",       &param_C,       1,                    /*.........01...*/
                    "PARAM_FREE_Rzero",   &param_Rzero,   1,                    /*.........01...*/
                    "PARAM_FIXED_shift",  &param_shift,   1                     /*.........01...*/
                    );                                                          /*.........01...*/
  if (KIM_STATUS_OK > ier)                                                      /*.........01...*/
  {                                                                             /*.........01...*/
    KIM_API_report_error(__LINE__, __FILE__, "KIM_API_getm_data", ier);         /*.........01...*/
    return ier;                                                                 /*.........01...*/
  }                                                                             /*.........01...*/
                                                                                /*.........01...*/
  /* free parameters */                                                         /*.........01...*/
  free(param_cutoff);                                                           /*.........01...*/
  free(param_cutsq);                                                            /*.........01...*/
  free(param_epsilon);                                                          /*.........01...*/
  free(param_C);                                                                /*.........01...*/
  free(param_Rzero);                                                            /*.........01...*/
  free(param_shift);                                                            /*.........01...*/
                                                                                /*.........01234*/
  ier = KIM_STATUS_OK;                                                          /*.........01234*/
  return ier;                                                                   /*.........01234*/
}                                                                               /*.........01234*/
