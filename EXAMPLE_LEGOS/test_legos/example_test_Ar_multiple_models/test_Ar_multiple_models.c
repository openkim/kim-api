/*                                                                      */
/* Copyright 2011 Ellad B. Tadmor, Ryan S. Elliott, and James P. Sethna */
/* All rights reserved.                                                 */
/*                                                                      */
/* Authors: Valeriu Smirichinski, Ryan S. Elliott, Ellad B. Tadmor      */
/*                                                                      */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "KIMserviceC.h"

#define NAMESTRLEN    128

#define FCCSPACING    5.260
#define DIM           3
#define NCELLSPERSIDE 2
#define NCLUSTERATOMS (4*(NCELLSPERSIDE*NCELLSPERSIDE*NCELLSPERSIDE) + 6*(NCELLSPERSIDE*NCELLSPERSIDE) + 3*(NCELLSPERSIDE) + 1)
/* Define neighborlist structure */
typedef struct
{
   int iteratorId;
   int* NNeighbors;
   int* neighborList;
   double* RijList;
} NeighList;

/* Define prototypes */
void report_error(int line, char* str, int status);
void fcc_periodic_neighborlist(int CellsPerHalfSide, double cutoff,
                               double FCCspacing, NeighList* nl);

void fcc_cluster_neighborlist(int numberOfAtoms, double* coords,
                              double cutoff, NeighList* nl);

int get_periodic_neigh(void* kimmdl, int *mode, int *request, int* atom,
                       int* numnei, int** nei1atom, double** Rij);

int get_cluster_neigh(void* kimmdl, int *mode, int *request, int* atom,
                      int* numnei, int** nei1atom, double** Rij);

void create_FCC_cluster(double FCCspacing, int nCellsPerSide, double *coords);


/* Main program */
int main()
{
   /* Local variable declarations */
   double const MinSpacing = 0.8*FCCSPACING;
   double const MaxSpacing = 1.2*FCCSPACING;
   double const SpacingIncr = 0.025*FCCSPACING;
   double CurrentSpacing;
   double CellsPerCutoff[2];
   int NNeighbors[2];
   int i;
   int status;


   /* KIM variable declarations */
   void* pkim_periodic_model_0;
   void* pkim_periodic_model_1;
   void* pkim_cluster_model_0;
   void* pkim_cluster_model_1;
   /* model inputs */
   long long numberOfAtoms_periodic = 1;
   long long numberOfAtoms_cluster  = NCLUSTERATOMS;
   int numberAtomTypes = 1;
   int AtomTypes_periodic_model_0;
   int AtomTypes_periodic_model_1;
   int AtomTypes_cluster_model_0[NCLUSTERATOMS];
   int AtomTypes_cluster_model_1[NCLUSTERATOMS];
   double coords_periodic[DIM] = {0.0, 0.0, 0.0};
   double coords_cluster[NCLUSTERATOMS][DIM];
   NeighList nl_periodic_model_0;
   NeighList nl_periodic_model_1;
   NeighList nl_cluster_model_0;
   NeighList nl_cluster_model_1;
   /* model outputs */
   double cutoff_periodic_model_0;
   double cutoff_periodic_model_1;
   double cutoff_cluster_model_0;
   double cutoff_cluster_model_1;
   double energy_periodic_model_0;
   double energy_periodic_model_1;
   double energy_cluster_model_0;
   double energy_cluster_model_1;

   char testname[] = "test_Ar_multiple_models";
   char modelname0[NAMESTRLEN];
   char modelname1[NAMESTRLEN];

   /* Get KIM Model names */
   printf("Please eneter two valid KIM Model names: \n");
   scanf("%s %s", modelname0, modelname1);
   
   /* initialize the two models */
   if (1 != (status = KIM_API_init(&pkim_periodic_model_0, testname, modelname0)))
      report_error(__LINE__,"KIM_API_init() for MODEL_ZERO for periodic",status);
   if (1 != (status = KIM_API_init(&pkim_cluster_model_0, testname, modelname0)))
      report_error(__LINE__,"KIM_API_init() for MODEL_ZERO for cluster",status);
   if (1 != (status = KIM_API_init(&pkim_periodic_model_1, testname, modelname1)))
      report_error(__LINE__,"KIM_API_init() for MODEL_ONE for periodic",status);
   if (1 != (status = KIM_API_init(&pkim_cluster_model_1, testname, modelname1)))
      report_error(__LINE__,"KIM_API_init() for MODEL_ONE cluster",status);

   /* Register memory */
   /* model inputs */
   status = KIM_API_set_data(pkim_periodic_model_0, "numberOfAtoms",   1, &numberOfAtoms_periodic);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "numberOfAtoms",   1, &numberOfAtoms_periodic);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "numberOfAtoms",   1, &numberOfAtoms_cluster);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "numberOfAtoms",   1, &numberOfAtoms_cluster);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "numberAtomTypes", 1, &numberAtomTypes);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "numberAtomTypes", 1, &numberAtomTypes);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "numberAtomTypes", 1, &numberAtomTypes);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "numberAtomTypes", 1, &numberAtomTypes);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "atomTypes",       1, &AtomTypes_periodic_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "atomTypes",       1, &AtomTypes_periodic_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "atomTypes",       NCLUSTERATOMS, &AtomTypes_cluster_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "atomTypes",       NCLUSTERATOMS, &AtomTypes_cluster_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "coordinates",     DIM, coords_periodic);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "coordinates",     DIM, coords_periodic);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "coordinates",     (NCLUSTERATOMS*DIM), &(coords_cluster[0][0]));
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "coordinates",     (NCLUSTERATOMS*DIM), &(coords_cluster[0][0]));
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "get_full_neigh",  1, &get_periodic_neigh);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "get_full_neigh",  1, &get_periodic_neigh);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "get_full_neigh",  1, &get_cluster_neigh);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "get_full_neigh",  1, &get_cluster_neigh);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "neighObject",     1, &nl_periodic_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "neighObject",     1, &nl_periodic_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "neighObject",     1, &nl_cluster_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "neighObject",     1, &nl_cluster_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   /* model outputs */
   status = KIM_API_set_data(pkim_periodic_model_0, "destroy",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "destroy",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "destroy",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "destroy",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "compute",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "compute",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "compute",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "compute",         1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "reinit",          1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "reinit",          1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "reinit",          1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "reinit",          1, NULL);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "cutoff",          1, &cutoff_periodic_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "cutoff",          1, &cutoff_periodic_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "cutoff",          1, &cutoff_cluster_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "cutoff",          1, &cutoff_cluster_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_0, "energy",          1, &energy_periodic_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_periodic_model_1, "energy",          1, &energy_periodic_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_0,  "energy",          1, &energy_cluster_model_0);
   if (1 != status) report_error(__LINE__,"set data",status);
   status = KIM_API_set_data(pkim_cluster_model_1,  "energy",          1, &energy_cluster_model_1);
   if (1 != status) report_error(__LINE__,"set data",status);

   /* call model init routines */
   if (1 != (status = KIM_API_model_init(pkim_periodic_model_0))) report_error(__LINE__,"KIM_API_model_init", status);
   if (1 != (status = KIM_API_model_init(pkim_cluster_model_0)))  report_error(__LINE__,"KIM_API_model_init", status);
   if (1 != (status = KIM_API_model_init(pkim_periodic_model_1))) report_error(__LINE__,"KIM_API_model_init", status);
   if (1 != (status = KIM_API_model_init(pkim_cluster_model_1)))  report_error(__LINE__,"KIM_API_model_init", status);

   /* setup atomTypes */
   AtomTypes_periodic_model_0 = KIM_API_get_aTypeCode(pkim_periodic_model_0, "Ar", &status);
   if (1 != status) report_error(__LINE__,"get_aTypeCode", status);

   AtomTypes_cluster_model_0[0] = KIM_API_get_aTypeCode(pkim_cluster_model_0, "Ar", &status);
   if (1 != status) report_error(__LINE__,"get_aTypeCode", status);
   for (i = 1; i < NCLUSTERATOMS; ++i)
      AtomTypes_cluster_model_0[i] = AtomTypes_cluster_model_0[0];
   AtomTypes_periodic_model_1 = KIM_API_get_aTypeCode(pkim_periodic_model_1, "Ar", &status);
   if (1 != status) report_error(__LINE__,"get_aTypeCode", status); 

   AtomTypes_cluster_model_1[0] = KIM_API_get_aTypeCode(pkim_cluster_model_1, "Ar", &status);
   if (1 != status) report_error(__LINE__,"get_aTypeCode", status); 
   for (i = 1; i < NCLUSTERATOMS; ++i)
      AtomTypes_cluster_model_1[i] = AtomTypes_cluster_model_1[0];
   
   
   /* setup neighbor lists */
   CellsPerCutoff[0] = ceil(cutoff_periodic_model_0/MinSpacing + 0.05); /* 0.05 is saftey factor */
   CellsPerCutoff[1] = ceil(cutoff_periodic_model_1/MinSpacing + 0.05); /* 0.05 is saftey factor */
   NNeighbors[0] = 4*pow((2*CellsPerCutoff[0] + 1),DIM);
   NNeighbors[1] = 4*pow((2*CellsPerCutoff[1] + 1),DIM);
   /* allocate memory for list */
   nl_periodic_model_0.NNeighbors = malloc(sizeof(int));
   if (NULL==nl_periodic_model_0.NNeighbors) report_error(__LINE__,"malloc unsuccessful", -1);
   
   nl_cluster_model_0.NNeighbors = malloc(NCLUSTERATOMS*sizeof(int));
   if (NULL==nl_cluster_model_0.NNeighbors) report_error(__LINE__,"malloc unsuccessful", -1);
      
   nl_periodic_model_1.NNeighbors = malloc(sizeof(int));
   if (NULL==nl_periodic_model_1.NNeighbors) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_cluster_model_1.NNeighbors = malloc(NCLUSTERATOMS*sizeof(int));
   if (NULL==nl_cluster_model_1.NNeighbors) report_error(__LINE__,"malloc unsuccessful", -1);
   
   nl_periodic_model_0.neighborList = malloc(NNeighbors[0]*sizeof(int));
   if (NULL==nl_periodic_model_0.neighborList) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_cluster_model_0.neighborList = malloc(NCLUSTERATOMS*NCLUSTERATOMS*sizeof(int));
   if (NULL==nl_cluster_model_0.neighborList) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_periodic_model_1.neighborList = malloc(NNeighbors[1]*sizeof(int));
   if (NULL==nl_periodic_model_1.neighborList) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_cluster_model_1.neighborList = malloc(NCLUSTERATOMS*NCLUSTERATOMS*sizeof(int));
   if (NULL==nl_cluster_model_1.neighborList) report_error(__LINE__,"malloc unsuccessful", -1);
   
   nl_periodic_model_0.RijList = malloc(DIM*NNeighbors[0]*sizeof(double));
   if (NULL==nl_periodic_model_0.RijList) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_cluster_model_0.RijList = malloc(DIM*NCLUSTERATOMS*NCLUSTERATOMS*sizeof(double));
   if (NULL==nl_cluster_model_0.RijList) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_periodic_model_1.RijList = malloc(DIM*NNeighbors[1]*sizeof(double));
   if (NULL==nl_periodic_model_1.RijList) report_error(__LINE__,"malloc unsuccessful", -1);

   nl_cluster_model_1.RijList = malloc(DIM*NCLUSTERATOMS*NCLUSTERATOMS*sizeof(double));
   if (NULL==nl_cluster_model_1.RijList) report_error(__LINE__,"malloc unsuccessful", -1);

   /* ready to compute */
   printf("                             MODEL_ZERO_periodic   MODEL_ZERO_cluster    MODEL_ONE_periodic    MODEL_ONE_cluster      Spacing\n");

   for (CurrentSpacing = MinSpacing; CurrentSpacing < MaxSpacing; CurrentSpacing += SpacingIncr)
   {
      /* update coordinates for cluster */
      create_FCC_cluster(CurrentSpacing, NCELLSPERSIDE, &(coords_cluster[0][0]));

      /* compute neighbor lists */
      fcc_periodic_neighborlist(CellsPerCutoff[0], (cutoff_periodic_model_0 + 0.75),
                                CurrentSpacing, &nl_periodic_model_0);
      fcc_cluster_neighborlist(NCLUSTERATOMS, &(coords_cluster[0][0]),
                               (cutoff_cluster_model_0 + 0.75), &nl_cluster_model_0);
      fcc_periodic_neighborlist(CellsPerCutoff[1], (cutoff_periodic_model_1 + 0.75),
                                CurrentSpacing, &nl_periodic_model_1);
      fcc_cluster_neighborlist(NCLUSTERATOMS, &(coords_cluster[0][0]),
                               (cutoff_cluster_model_1 + 0.75), &nl_cluster_model_1);

      /* call compute functions */
      KIM_API_model_compute(pkim_periodic_model_0, &status);
      if (1 != status) report_error(__LINE__,"compute", status);

      KIM_API_model_compute(pkim_cluster_model_0, &status);
      if (1 != status) report_error(__LINE__,"compute", status);

      KIM_API_model_compute(pkim_periodic_model_1, &status);
      if (1 != status) report_error(__LINE__,"compute", status);

      KIM_API_model_compute(pkim_cluster_model_1, &status);
      if (1 != status) report_error(__LINE__,"compute", status);

      /* print the results */
      printf("Energy for %i atoms = %20.10f, %20.10f, %20.10f, %20.10f, %20.10f\n",
             NCLUSTERATOMS,
             energy_periodic_model_0*NCLUSTERATOMS,
             energy_cluster_model_0,
             energy_periodic_model_1*NCLUSTERATOMS,
             energy_cluster_model_1,
             CurrentSpacing);
   }


   /* call model destroy */
   KIM_API_model_destroy(pkim_periodic_model_0, &status);
   if (1 != status) report_error(__LINE__,"destroy", status);

   KIM_API_model_destroy(pkim_cluster_model_0, &status);
   if (1 != status) report_error(__LINE__,"destroy", status);

   KIM_API_model_destroy(pkim_periodic_model_1, &status);
   if (1 != status) report_error(__LINE__,"destroy", status);

   KIM_API_model_destroy(pkim_cluster_model_1, &status);
   if (1 != status) report_error(__LINE__,"destroy", status);
   
   /* free memory of neighbor lists */
   free(nl_periodic_model_0.NNeighbors);
   free(nl_cluster_model_0.NNeighbors);
   free(nl_periodic_model_1.NNeighbors);
   free(nl_cluster_model_1.NNeighbors);
   free(nl_periodic_model_0.neighborList);
   free(nl_cluster_model_0.neighborList);
   free(nl_periodic_model_1.neighborList);
   free(nl_cluster_model_1.neighborList);
   free(nl_periodic_model_0.RijList);
   free(nl_cluster_model_0.RijList);
   free(nl_periodic_model_1.RijList);
   free(nl_cluster_model_1.RijList);

   /* free pkim objects */
   KIM_API_free(&pkim_periodic_model_0, &status);
   if (1 != status) report_error(__LINE__,"free", status);
   KIM_API_free(&pkim_periodic_model_1, &status);
   if (1 != status) report_error(__LINE__,"free", status);
   KIM_API_free(&pkim_cluster_model_0, &status);
   if (1 != status) report_error(__LINE__,"free", status);
   KIM_API_free(&pkim_cluster_model_1, &status);
   if (1 != status) report_error(__LINE__,"free", status);

   /* everything is great */
   return 0;
}


void fcc_periodic_neighborlist(int CellsPerHalfSide, double cutoff,
                               double FCCspacing, NeighList* nl)
{
   /* local variables */
   double dx[DIM];
   double r2;
   double cutoff2;
   double FCCshifts[4][DIM];
   double latVec[DIM];
   int a;
   int i;
   int j;
   int k;
   int m;
   int n;

   cutoff2 = cutoff*cutoff;

   /* cubic FCC cell positions */
   FCCshifts[0][0] = 0.0;            FCCshifts[0][1] = 0.0;            FCCshifts[0][2] = 0.0;
   FCCshifts[1][0] = 0.5*FCCspacing; FCCshifts[1][1] = 0.5*FCCspacing; FCCshifts[1][2] = 0.0;
   FCCshifts[2][0] = 0.5*FCCspacing; FCCshifts[2][1] = 0.0;            FCCshifts[2][2] = 0.5*FCCspacing;
   FCCshifts[3][0] = 0.0;            FCCshifts[3][1] = 0.5*FCCspacing; FCCshifts[3][2] = 0.5*FCCspacing;

   a = 0;
   for (i=-CellsPerHalfSide; i <= CellsPerHalfSide; ++i)
   {
      latVec[0] = i*FCCspacing;
      for (j=-CellsPerHalfSide; j <= CellsPerHalfSide; ++j)
      {
         latVec[1] = j*FCCspacing;
         for (k=-CellsPerHalfSide; k <= CellsPerHalfSide; ++k)
         {
            latVec[2] = k*FCCspacing;
            for (m=0; m < 4; ++m)
            {
               /* compute dx and dot product at same time */
               r2 = 0.0;
               for (n=0;n<DIM;++n)
               {
                  dx[n] = latVec[n] + FCCshifts[m][n];
                  r2 += dx[n]*dx[n];
               }

               if (r2 < cutoff2)
               {
                  if (! ((0 == i) &&
                         (0 == j) &&
                         (0 == k) &&
                         (0 == m)))
                  {
                     /* We have a neighbor */
                     (*nl).neighborList[a] = 0;
                     for (n = 0; n<DIM; ++n)
                        (*nl).RijList[DIM*a + n] = dx[n];
                     a++;
                  }
               }
            }
         }
      }
   }
   /* there are `a' neighbors */
   *((*nl).NNeighbors) = a;
   
   return;
}

int get_periodic_neigh(void* kimmdl, int *mode, int *request, int* atom,
                       int* numnei, int** nei1atom, double** Rij)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) kimmdl);
   int atomToReturn;
   int status;
   intptr_t* numberOfAtoms;
   int N;
   NeighList* nl;

   /* initialize numnei */
   *numnei = 0;

   /* unpack neighbor list object */
   numberOfAtoms = (intptr_t*) KIM_API_get_data(pkim, "numberOfAtoms", &status);
   if (1 != status) report_error(__LINE__,"get_data", status);

   N = *numberOfAtoms;
   nl = (NeighList*) KIM_API_get_data(pkim, "neighObject", &status);
   if (1 != status) report_error(__LINE__,"get_data", status);

   /* check mode and request */
   if (0 == *mode) /* iterator mode */
   {
      if (0 == *request) /* reset iterator */
      {
         (*nl).iteratorId = -1;
         return 2;
      }
      else if (1 == *request) /* increment iterator */
      {
         (*nl).iteratorId++;
         if ((*nl).iteratorId >= N)
         {
            return 0;
         }
         else
         {
            atomToReturn = (*nl).iteratorId;
         }
      }
      else /* invalid request value */
      {
         report_error(__LINE__,"Invalid request in get_periodic_neigh", -6);
         return -6;
      }
   }
   else if (1 == *mode) /* locator mode */
   {
      if ((*request >= N) || (*request < 0)) /* invalid request */
      {
         report_error(__LINE__,"Invalid request in get_periodic_neigh", -1);
         return -1;
      }
      else
      {
         atomToReturn = *request;
      }
   }
   else /* invalid mode */
   {
      report_error(__LINE__,"Invalid mode in get_periodic_neigh", -2);
      return -2;
   }

   /* set the returned atom */
   *atom = atomToReturn;

   /* set the returned number of neighbors for the returned atom */
   *numnei = *((*nl).NNeighbors);

   /* set the location for the returned neighbor list */
   *nei1atom = (*nl).neighborList;

   /* set the pointer to Rij to appropriate value */
   *Rij = (*nl).RijList;

   return 1;
}

void create_FCC_cluster(double FCCspacing, int nCellsPerSide, double *coords)
{
   /* local variables */
   double FCCshifts[4][DIM];
   double latVec[DIM];
   int a;
   int i;
   int j;
   int k;
   int m;
   int n;

   /* create a cubic FCC cluster of atoms */
   FCCshifts[0][0] = 0.0;            FCCshifts[0][1] = 0.0;            FCCshifts[0][2] = 0.0;
   FCCshifts[1][0] = 0.5*FCCspacing; FCCshifts[1][1] = 0.5*FCCspacing; FCCshifts[1][2] = 0.0;
   FCCshifts[2][0] = 0.5*FCCspacing; FCCshifts[2][1] = 0.0;            FCCshifts[2][2] = 0.5*FCCspacing;
   FCCshifts[3][0] = 0.0;            FCCshifts[3][1] = 0.5*FCCspacing; FCCshifts[3][2] = 0.5*FCCspacing;

   a = 0;
   for (i = 0; i < nCellsPerSide; ++i)
   {
      latVec[0] = ((double) i)*FCCspacing;
      for (j = 0; j < nCellsPerSide; ++j)
      {
         latVec[1] = ((double) j)*FCCspacing;
         for (k = 0; k < nCellsPerSide; ++k)
         {
            latVec[2] = ((double) k)*FCCspacing;
            for (m = 0; m < 4; ++m)
            {
               for (n = 0; n < DIM; ++n)
               {
                  coords[a*DIM + n] = latVec[n] + FCCshifts[m][n];
               }
               a++;
            }
         }
         /* add in the remaining three faces */
         /* pos-x face */
         latVec[0] = NCELLSPERSIDE*FCCspacing;
         latVec[1] = ((double) i)*FCCspacing;
         latVec[2] = ((double) j)*FCCspacing;
         for (n = 0; n < DIM; ++n)
         {
            coords[a*DIM + n] = latVec[n];
         }
         a++;
         for (n = 0; n < DIM; ++n)
         {
            coords[a*DIM + n] = latVec[n] + FCCshifts[3][n];
         }
         a++;
         /* pos-y face */
         latVec[0] = ((double) i)*FCCspacing;
         latVec[1] = NCELLSPERSIDE*FCCspacing;
         latVec[2] = ((double) j)*FCCspacing;
         for (n = 0; n < DIM; ++n)
         {
            coords[a*DIM + n] = latVec[n];
         }
         a++;
         for (n = 0; n < DIM; ++n)
         {
            coords[a*DIM + n] = latVec[n] + FCCshifts[2][n];
         }
         a++;
         /* pos-z face */
         latVec[0] = ((double) i)*FCCspacing;
         latVec[1] = ((double) j)*FCCspacing;
         latVec[2] = NCELLSPERSIDE*FCCspacing;
         for (n = 0; n < DIM; ++n)
         {
            coords[a*DIM + n] = latVec[n];
         }
         a++;
         for (n = 0; n < DIM; ++n)
         {
            coords[a*DIM + n] = latVec[n] + FCCshifts[1][n];
         }
         a++;
      }
      /* add in the remaining three edges */
      latVec[0] = ((double) i)*FCCspacing;
      latVec[1] = NCELLSPERSIDE*FCCspacing;
      latVec[2] = NCELLSPERSIDE*FCCspacing;
      for (n = 0; n < DIM; ++n)
      {
         coords[a*DIM + n] = latVec[n];
      }
      a++;
      latVec[0] = NCELLSPERSIDE*FCCspacing;
      latVec[1] = ((double) i)*FCCspacing;
      latVec[2] = NCELLSPERSIDE*FCCspacing;
      for (n = 0; n < DIM; ++n)
      {
         coords[a*DIM + n] = latVec[n];
      }
      a++;
      latVec[0] = NCELLSPERSIDE*FCCspacing;
      latVec[1] = NCELLSPERSIDE*FCCspacing;
      latVec[2] = ((double) i)*FCCspacing;
      for (n = 0; n < DIM; ++n)
      {
         coords[a*DIM + n] = latVec[n];
      }
      a++;
   }
   /* add in the remaining corner */
   for (n = 0; n < DIM; ++n)
   {
      coords[a*DIM + n] = NCELLSPERSIDE*FCCspacing;
   }
   a++;

   return;
}


void fcc_cluster_neighborlist(int numberOfAtoms, double* coords,
                              double cutoff, NeighList* nl)
{
   /* local variables */
   int i;
   int j;
   int k;
   int a;

   double dx[DIM];
   double r2;
   double cutoff2;

   cutoff2 = cutoff*cutoff;

   for (i = 0; i < numberOfAtoms; ++i)
   {
      a = 0;
      for (j = 0; j < numberOfAtoms; ++j)
      {
         r2 = 0.0;
         for (k = 0; k < DIM; ++k)
         {
            dx[k] = coords[j*DIM + k] - coords[i*DIM + k];
            r2 += dx[k]*dx[k];
         }

         if (r2 < cutoff2)
         {
            if (i != j)
            {
               /* atom j is a neighbor of atom i */
               (*nl).neighborList[i*NCLUSTERATOMS + a] = j;
               for (k = 0; k < DIM; ++k)
               {
                  (*nl).RijList[i*DIM*NCLUSTERATOMS + a*DIM + k] = dx[k];
               }
               a++;
            }
         }
      }
      /* atom i has `a' neighbors */
      (*nl).NNeighbors[i] = a;
   }

   return;
}

int get_cluster_neigh(void* kimmdl, int *mode, int *request, int* atom,
                      int* numnei, int** nei1atom, double** Rij)
{
   /* local variables */
   intptr_t* pkim = *((intptr_t**) kimmdl);
   int atomToReturn;
   int status;
   intptr_t* numberOfAtoms;
   int N;
   NeighList* nl;

   /* initialize numnei */
   *numnei = 0;

   /* unpack neighbor list object */
   numberOfAtoms = (intptr_t*) KIM_API_get_data(pkim, "numberOfAtoms", &status);
   if (1 != status) report_error(__LINE__,"get_data", status);

   N = *numberOfAtoms;
   nl = (NeighList*) KIM_API_get_data(pkim, "neighObject", &status);
   if (1 != status) report_error(__LINE__,"get_data", status);

   /* check mode and request */
   if (0 == *mode) /* iterator mode */
   {
      if (0 == *request) /* reset iterator */
      {
         (*nl).iteratorId = -1;
         return 2;
      }
      else if (1 == *request) /* increment iterator */
      {
         (*nl).iteratorId++;
         if ((*nl).iteratorId >= N)
         {
            return 0;
         }
         else
         {
            atomToReturn = (*nl).iteratorId;
         }
      }
      else /* invalid request value */
      {
         report_error(__LINE__,"Invalid request in get_cluster_neigh", -6);
         return -6;
      }
   }
   else if (1 == *mode) /* locator mode */
   {
      if ((*request >= N) || (*request < 0)) /* invalid request */
      {
         report_error(__LINE__,"Invalid request in get_cluster_neigh", -1);
         return -1;
      }
      else
      {
         atomToReturn = *request;
      }
   }
   else /* invalid mode */
   {
      report_error(__LINE__,"Invalid mode in get_cluster_neigh", -2);
      return -2;
   }

   /* set the returned atom */
   *atom = atomToReturn;

   /* set the returned number of neighbors for the returned atom */
   *numnei = (*nl).NNeighbors[*atom];

   /* set the location for the returned neighbor list */
   *nei1atom = &((*nl).neighborList[(*atom)*NCLUSTERATOMS]);

   /* set the pointer to Rij to appropriate value */
   *Rij = &((*nl).RijList[(*atom)*DIM*NCLUSTERATOMS]);

   return 1;
}

void report_error(int line, char* str, int status)
{
   printf("Error at line %i of %s: %s with status = %i.\n",line,__FILE__,str,status);
   exit(-1);
}
