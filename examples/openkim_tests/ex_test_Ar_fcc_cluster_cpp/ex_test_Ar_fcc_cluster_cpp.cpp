//
//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//
//
//
// Copyright (c) 2013--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Stephen M. Whalen
//
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <string>
#include "KIM_SpeciesName.hpp"
#include "KIM_Model.hpp"
#include "KIM_Simulator.hpp"
#include "KIM_Compute.hpp"
#include "KIM_UTILITY_Compute.hpp"
#include "KIM_Logger.hpp"

#define NAMESTRLEN    128

#define FCCSPACING    5.260
#define DIM           3
#define NCELLSPERSIDE 2
#define NCLUSTERPARTS (4*(NCELLSPERSIDE*NCELLSPERSIDE*NCELLSPERSIDE) + \
                       6*(NCELLSPERSIDE*NCELLSPERSIDE)                 \
                       + 3*(NCELLSPERSIDE) + 1)

#define REPORT_ERROR(LN, FL, MSG, STAT) {                 \
    KIM::report_error(LN, FL, MSG, STAT);                 \
    exit(STAT);                                           \
  }

/* Define neighborlist structure */
typedef struct
{
  int iteratorId;
  int* NNeighbors;
  int* neighborList;
} NeighList;

/* Define prototypes */
char const * const descriptor();
void fcc_cluster_neighborlist(int half, int numberOfParticles, double* coords,
                              double cutoff, NeighList* nl);

int get_cluster_neigh(KIM::Simulator const * const simulator,
                      int const neighborListIndex,
                      int const particleNumber, int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle);

void create_FCC_cluster(double FCCspacing, int nCellsPerSide, double *coords);


/* Main program */
int main()
{
  /* Local variable declarations */
  double const MinSpacing = 0.8*FCCSPACING;
  double const MaxSpacing = 1.2*FCCSPACING;
  double const SpacingIncr = 0.025*FCCSPACING;
  double CurrentSpacing;
  double cutpad = 0.75; /* Angstroms */
  int i;
  int error;


  /* model inputs */
  int numberOfParticles_cluster = NCLUSTERPARTS;
  int numberOfSpecies = 1;
  int particleSpecies_cluster_model[NCLUSTERPARTS];
  int particleContributing_cluster_model[NCLUSTERPARTS];
  double coords_cluster[NCLUSTERPARTS][DIM];
  NeighList nl_cluster_model;
  /* model outputs */
  double influence_distance_cluster_model;
  int number_of_cutoffs;
  double const * cutoff_cluster_model;
  double energy_cluster_model;

  std::string modelname;

  /* Get KIM Model names */
  printf("Please enter valid KIM Model name: \n");
  std::cin >> modelname;


  /* initialize the model */
  KIM::Model * kim_cluster_model;
  error = KIM::Model::create(descriptor(),modelname, &kim_cluster_model);
  if (error)
    REPORT_ERROR(__LINE__, __FILE__,"KIM_create_model_interface()",error);

  error = KIM::UTILITY::COMPUTE::setm_data(
      kim_cluster_model,
      KIM::COMPUTE::ARGUMENT_NAME::numberOfParticles, 1,                       &numberOfParticles_cluster,       1,
      KIM::COMPUTE::ARGUMENT_NAME::numberOfSpecies, 1,                         &numberOfSpecies,                 1,
      KIM::COMPUTE::ARGUMENT_NAME::particleSpecies, numberOfParticles_cluster, particleSpecies_cluster_model,   1,
      KIM::COMPUTE::ARGUMENT_NAME::particleContributing, numberOfParticles_cluster, particleContributing_cluster_model, 1,
      KIM::COMPUTE::ARGUMENT_NAME::coordinates, DIM*numberOfParticles_cluster, coords_cluster,                   1,
      KIM::COMPUTE::ARGUMENT_NAME::energy, 1,                                  &energy_cluster_model,            1,
      KIM::COMPUTE::ARGUMENT_NAME::End);
  if (error) REPORT_ERROR(__LINE__, __FILE__,"KIM_API_setm_data",error);
  kim_cluster_model->set_get_neigh(KIM::LANGUAGE_NAME::Cpp, (KIM::func *) &get_cluster_neigh);
  kim_cluster_model->set_neighObject((void *) &nl_cluster_model);

  /* call model init routine */
  error = kim_cluster_model->init();
  if (error) REPORT_ERROR(__LINE__, __FILE__,"KIM_API_model_init", error);

  kim_cluster_model->get_influence_distance(&influence_distance_cluster_model);
  kim_cluster_model->get_cutoffs(&number_of_cutoffs, &cutoff_cluster_model);
  if (number_of_cutoffs != 1) REPORT_ERROR(__LINE__, __FILE__,"too many cutoffs", 1);

  /* setup particleSpecies */
  error = kim_cluster_model->get_species_code(KIM::SPECIES_NAME::Ar,
                                              &(particleSpecies_cluster_model[0]));
  if (error) REPORT_ERROR(__LINE__, __FILE__,"get_species_code", error);
  for (i = 1; i < NCLUSTERPARTS; ++i)
    particleSpecies_cluster_model[i] = particleSpecies_cluster_model[0];
  /* setup particleContributing */
  for (i = 0; i < NCLUSTERPARTS; ++i)
    particleContributing_cluster_model[i] = 1;  /* every particle contributes */

  /* setup neighbor lists */
  /* allocate memory for list */
  nl_cluster_model.NNeighbors = new int[NCLUSTERPARTS];
  if (NULL==nl_cluster_model.NNeighbors) REPORT_ERROR(__LINE__, __FILE__,"new unsuccessful", -1);

  nl_cluster_model.neighborList = new int[NCLUSTERPARTS*NCLUSTERPARTS];
  if (NULL==nl_cluster_model.neighborList) REPORT_ERROR(__LINE__, __FILE__,"new unsuccessful", -1);

  /* ready to compute */
  std::cout << std::setiosflags(std::ios::scientific) << std::setprecision(10);
  std::cout << "--------------------------------------------------------------------------------\n";
  std::cout << "This is Test  : ex_test_Ar_fcc_cluster\n";
  std::cout << "MODEL is : " << modelname << std::endl;

  for (CurrentSpacing = MinSpacing; CurrentSpacing < MaxSpacing; CurrentSpacing += SpacingIncr)
  {
    /* update coordinates for cluster */
    create_FCC_cluster(CurrentSpacing, NCELLSPERSIDE, &(coords_cluster[0][0]));
    /* compute neighbor lists */
    fcc_cluster_neighborlist(0, NCLUSTERPARTS, &(coords_cluster[0][0]),
                             (*cutoff_cluster_model + cutpad), &nl_cluster_model);

    /* call compute functions */
    error = kim_cluster_model->compute();
    if (error) REPORT_ERROR(__LINE__, __FILE__,"compute", error);

    /* print the results */
    std::cout << "Energy for " << NCLUSTERPARTS << " parts = "
              << std::setw(20) << energy_cluster_model
              << std::setw(20) << CurrentSpacing
              << std::endl;
  }


  /* call model destroy */
  error = kim_cluster_model->destroy_model();
  if (error) REPORT_ERROR(__LINE__, __FILE__,"destroy", error);

  KIM::Model::destroy(&kim_cluster_model);

  /* free memory of neighbor lists */
  delete [] nl_cluster_model.NNeighbors;
  delete [] nl_cluster_model.neighborList;

  /* everything is great */
  return 0;
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

  /* create a cubic FCC cluster of parts */
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


void fcc_cluster_neighborlist(int half, int numberOfParticles, double* coords,
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

  for (i = 0; i < numberOfParticles; ++i)
  {
    a = 0;
    for (j = 0; j < numberOfParticles; ++j)
    {
      r2 = 0.0;
      for (k = 0; k < DIM; ++k)
      {
        dx[k] = coords[j*DIM + k] - coords[i*DIM + k];
        r2 += dx[k]*dx[k];
      }

      if (r2 < cutoff2)
      {
        if ((half && i < j) || (!half && i != j))
        {
          /* part j is a neighbor of part i */
          (*nl).neighborList[i*NCLUSTERPARTS + a] = j;
          a++;
        }
      }
    }
    /* part i has `a' neighbors */
    (*nl).NNeighbors[i] = a;
  }

  return;
}

int get_cluster_neigh(KIM::Simulator const * const simulator,
                      int const neighborListIndex,
                      int const particleNumber, int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle)
{
  /* local variables */
  int error = true;
  int* numberOfParticles;
  NeighList* nl;

  if (neighborListIndex != 0) return error;

  /* initialize numNeigh */
  *numberOfNeighbors = 0;

  /* unpack neighbor list object */
  error = simulator->get_data(KIM::COMPUTE::ARGUMENT_NAME::numberOfParticles, (void **) &numberOfParticles);
  if (error)
  {
    KIM::report_error(__LINE__, __FILE__,"get_data", error);
    return error;
  }

  simulator->get_neighObject((void **) &nl);

  if ((particleNumber >= *numberOfParticles) || (particleNumber < 0)) /* invalid id */
  {
    KIM::report_error(__LINE__, __FILE__,"Invalid part ID in get_cluster_neigh", true);
    return true;
  }

  /* set the returned number of neighbors for the returned part */
  *numberOfNeighbors = (*nl).NNeighbors[particleNumber];

  /* set the location for the returned neighbor list */
  *neighborsOfParticle = &((*nl).neighborList[(particleNumber)*NCLUSTERPARTS]);

  return false;
}

char const * const descriptor()
{
  return
      "KIM_API_Version := 1.6.0\n"
      "Unit_length := A\n"
      "Unit_energy := eV\n"
      "Unit_charge := e\n"
      "Unit_temperature := K\n"
      "Unit_time := ps\n"
      "\n"
      "PARTICLE_SPECIES:\n"
      "Ar spec 1\n"
      "\n"
      "CONVENTIONS:\n"
      "ZeroBasedLists flag\n"
      "\n"
      "MODEL_INPUT:\n"
      "numberOfParticles integer none\n"
      "numberOfSpecies integer none\n"
      "particleSpecies integer none\n"
      "particleContributing integer none\n"
      "coordinates double length\n"
      "get_neigh method none\n"
      "neighObject pointer none\n"
      "\n"
      "MODEL_OUTPUT:\n"
      "destroy method none\n"
      "compute method none\n"
      "reinit method none\n"
      "energy double energy\n";
}
