/* KIM compliant program to compute the energy of and forces and virial */
/* on an isolated cluster of SPECIES_NAME_STR atoms                                   */
/*                                                                      */
/* Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor      */
/*                                                                      */
/* Release: This file is part of the openkim-api.git repository.        */
/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "KIMserviceC.h"
#include "KIMstatus.h"

#define FCCSPACING    5.260
#define NCELLSPERSIDE 2
#define DIM           3
#define ATYPES        1
#define NCLUSTERATOMS (4*(NCELLSPERSIDE*NCELLSPERSIDE*NCELLSPERSIDE) + 6*(NCELLSPERSIDE*NCELLSPERSIDE) + 3*(NCELLSPERSIDE) + 1)

/* Define prototypes */
static void create_FCC_configuration(double FCCspacing, int nCellsPerSide, int periodic,
                                     double *coords, int *MiddleAtomId);


/* Main program */
int main(int argc, char* argv[])
{
   /* Local variable declarations */
   int i;


   /* KIM variable declarations */
   char testname[] = "TEST_NAME_STR";
   char modelname[80];
   void* pkim;
   int status;
   int atypecode;
   
   /* model inputs */
   int* numberOfAtoms;
   int* numberAtomTypes;
   int* atomTypes;
   double* coords;
   /* model outputs */
   double* cutoff;
   double* energy;
   double* forces;
   double* virialGlobal;
   int middleDum;

   /* Get KIM Model name to use */
   printf("Please enter a valid KIM model name: \n");
   scanf("%s",modelname);
   
   /* Initialize the KIM Model */
   status = KIM_API_init(&pkim, testname, modelname);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_init", status);
      exit(1);
   }

   /* Allocate memory via the KIM system */
   KIM_API_allocate(pkim, NCLUSTERATOMS, ATYPES, &status);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_allocate", status);
      exit(1);
   }

   /* call Model's init routine */
   status = KIM_API_model_init(pkim);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_model_init", status);
      exit(1);
   }

   /* Unpack data from KIM object */
   KIM_API_get_data_multiple(pkim, &status, 8*3,
                             "numberOfAtoms",     &numberOfAtoms,   1,
                             "numberAtomTypes",   &numberAtomTypes, 1,
                             "atomTypes",         &atomTypes,       1,
                             "coordinates",       &coords,          1,
                             "cutoff",            &cutoff,          1,
                             "energy",            &energy,          1,
                             "virialGlobal",      &virialGlobal,    1,
                             "forces",            &forces,          1);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_data_multiple", status);
      exit(1);
   }

   /* Set values */
   *numberOfAtoms   = NCLUSTERATOMS;
   *numberAtomTypes = ATYPES;
   atypecode = KIM_API_get_aTypeCode(pkim, "SPECIES_NAME_STR", &status);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_get_aTypeCode", status);
      exit(1);
   }
   for (i = 0; i < *numberOfAtoms; ++i)
   {
      atomTypes[i] = atypecode;
   }

   /* set up the cluster atom positions */
   create_FCC_configuration(FCCSPACING, NCELLSPERSIDE, 0, coords, &middleDum);

   /* Call model compute */
   KIM_API_model_compute(pkim, &status);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_model_compute", status);
      exit(1);
   }

   /* print results to screen */
   printf("--------------------------------------------------------------------------------\n");
   printf("This is Test          : %s\n",testname);
   printf("Results for KIM Model : %s\n",modelname);
   printf("Forces:\n");
   printf("Atom     "
          "X                        "
          "Y                        "
          "Z                        "
          "\n");
   for (i = 0; i < *numberOfAtoms; ++i)
   {
      printf("%2i   %25.15e%25.15e%25.15e\n", i,
             forces[i*DIM + 0],
             forces[i*DIM + 1],
             forces[i*DIM + 2]
            );
   }
   printf("\n");
   printf("Energy        = %25.15e\n"
          "Global Virial = %25.15e%25.15e%25.15e\n"
          "                %25.15e%25.15e%25.15e\n",
          *energy,
          virialGlobal[0],
          virialGlobal[1],
          virialGlobal[2],
          virialGlobal[3],
          virialGlobal[4],
          virialGlobal[5]
         );

   
   /* don't forget to destroy and deallocate */
   KIM_API_model_destroy(pkim, &status);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_model_destory", status);
      exit(1);
   }
   KIM_API_free(&pkim, &status);
   if (KIM_STATUS_OK > status)
   {
      KIM_API_report_error(__LINE__, __FILE__, "KIM_API_free", status);
      exit(1);
   }
   
   /* everything is great */
   return 0;
}
