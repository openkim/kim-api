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
// Copyright (c) 2013--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Stephen M. Whalen
//
//


#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include "KIM_SimulatorHeaders.hpp"

#define NAMESTRLEN    128

#define FCCSPACING    5.260
#define DIM           3
#define NCELLSPERSIDE 2
#define NCLUSTERPARTS (4*(NCELLSPERSIDE*NCELLSPERSIDE*NCELLSPERSIDE) + \
                       6*(NCELLSPERSIDE*NCELLSPERSIDE)                 \
                       + 3*(NCELLSPERSIDE) + 1)

#define MY_ERROR(message)                                       \
  {                                                             \
    std::cout << "* Error : \"" << message << "\" : "           \
              << __LINE__ << ":" << __FILE__ << std::endl;      \
    exit(1);                                                    \
  }

#define MY_WARNING(message)                                             \
  {                                                                     \
    std::cout << "* Error : \"" << message << "\" : "                   \
              << __LINE__ << ":" << __FILE__ << std::endl;              \
  }


/* Define neighborlist structure */
typedef struct
{
  double cutoff;
  int numberOfParticles;
  int* NNeighbors;
  int* neighborList;
} NeighList;

/* Define prototypes */
void fcc_cluster_neighborlist(int half, int numberOfParticles, double* coords,
                              double cutoff, NeighList* nl);

int get_cluster_neigh(void * const dataObject,
                      int const numberOfNeighborLists,
                      double const * const cutoffs,
                      int const neighborListIndex, int const particleNumber,
                      int * const numberOfNeighbors,
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
  double force_norm;
  int i;
  int error;


  /* model inputs */
  int numberOfParticles_cluster = NCLUSTERPARTS;
  int particleSpecies_cluster_model[NCLUSTERPARTS];
  int particleContributing_cluster_model[NCLUSTERPARTS];
  double coords_cluster[NCLUSTERPARTS][DIM];
  NeighList nl_cluster_model;
  /* model outputs */
  double influence_distance_cluster_model;
  int number_of_neighbor_lists;
  double const * cutoff_cluster_model;
  double energy_cluster_model;
  double forces_cluster[NCLUSTERPARTS*DIM];

  std::string modelname;

  /* Get KIM Model names */
  printf("Please enter valid KIM Model name: \n");
  std::cin >> modelname;


  /* initialize the model */
  KIM::Model * kim_cluster_model;
  int requestedUnitsAccepted;
  error = KIM::Model::Create(KIM::NUMBERING::zeroBased,
                             KIM::LENGTH_UNIT::A,
                             KIM::ENERGY_UNIT::eV,
                             KIM::CHARGE_UNIT::e,
                             KIM::TEMPERATURE_UNIT::K,
                             KIM::TIME_UNIT::ps,
                             modelname,
                             &requestedUnitsAccepted,
                             &kim_cluster_model);
  if (error)
  {
    MY_ERROR("KIM::Model::Create()");
  }

  // Check for compatibility with the model
  if (!requestedUnitsAccepted)
  {
    MY_ERROR("Must Adapt to model units");
  }

  // print model units
  KIM::LengthUnit lengthUnit;
  KIM::EnergyUnit energyUnit;
  KIM::ChargeUnit chargeUnit;
  KIM::TemperatureUnit temperatureUnit;
  KIM::TimeUnit timeUnit;

  kim_cluster_model->GetUnits(&lengthUnit, &energyUnit, &chargeUnit,
                              &temperatureUnit, &timeUnit);

  std::cout << "LengthUnit is \"" << lengthUnit.String() << "\"" << std::endl
            << "EnergyUnit is \"" << energyUnit.String() << "\"" << std::endl
            << "ChargeUnit is \"" << chargeUnit.String() << "\"" << std::endl
            << "TemperatureUnit is \"" << temperatureUnit.String()
            << "\"" << std::endl
            << "TimeUnit is \"" << timeUnit.String() << "\"" << std::endl;

  // check species
  int speciesIsSupported;
  int modelArCode;
  error = kim_cluster_model->GetSpeciesSupportAndCode(
      KIM::SPECIES_NAME::Ar, &speciesIsSupported, &modelArCode);
  if ((error) || (!speciesIsSupported))
  {
    MY_ERROR("Species Ar not supported");
  }

  KIM::ComputeArguments * computeArguments;
  error = kim_cluster_model->ComputeArgumentsCreate(&computeArguments);
  if (error)
  {
    MY_ERROR("Unable to create a ComputeArguments object.");
  }

  // check compute arguments
  int numberOfComputeArgumentNames;
  KIM::COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames(
      &numberOfComputeArgumentNames);
  for (int i=0; i<numberOfComputeArgumentNames; ++i)
  {
    KIM::ComputeArgumentName computeArgumentName;
    KIM::SupportStatus supportStatus;
    KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName(i, &computeArgumentName);
    KIM::DataType dataType;
    KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentDataType(
        computeArgumentName, &dataType);
    error = computeArguments->GetArgumentSupportStatus(computeArgumentName,
                                                       &supportStatus);
    if (error)
      MY_ERROR("unable to get ComputeArgument SupportStatus");

    std::cout << "ComputeArgument Name \""
              << computeArgumentName.String() << "\""
              << " is of type \""
              << dataType.String() << "\""
              << " and has supportStatus \""
              << supportStatus.String() << "\""
              << std::endl;

    // can only handle energy and force as a required arg
    if (supportStatus == KIM::SUPPORT_STATUS::required)
    {
      if ((computeArgumentName != KIM::COMPUTE_ARGUMENT_NAME::partialEnergy)
          &&
          (computeArgumentName != KIM::COMPUTE_ARGUMENT_NAME::partialForces))
      {
        MY_ERROR("unsupported required ComputeArgument");
      }
    }

    // must have energy and forces
    if ((computeArgumentName == KIM::COMPUTE_ARGUMENT_NAME::partialEnergy)
        ||
        (computeArgumentName == KIM::COMPUTE_ARGUMENT_NAME::partialForces))
    {
      if (! ((supportStatus == KIM::SUPPORT_STATUS::required)
             ||
             (supportStatus == KIM::SUPPORT_STATUS::optional)))
      {
        MY_ERROR("energy or forces not available");
      }
    }
  }

  // check compute callbacks
  int numberOfComputeCallbackNames;
  KIM::COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbackNames(
      &numberOfComputeCallbackNames);
  for (int i=0; i<numberOfComputeCallbackNames; ++i)
  {
    KIM::ComputeCallbackName computeCallbackName;
    KIM::COMPUTE_CALLBACK_NAME::GetComputeCallbackName(i, &computeCallbackName);
    KIM::SupportStatus supportStatus;
    computeArguments->GetCallbackSupportStatus(computeCallbackName,
                                               &supportStatus);

    std::cout << "ComputeCallback Name \""
              << computeCallbackName.String() << "\""
              << " has supportStatus \""
              << supportStatus.String() << "\""
              << std::endl;

    // cannot handle any "required" callbacks
    if (supportStatus == KIM::SUPPORT_STATUS::required)
    {
      MY_ERROR("unsupported required ComputeCallback");
    }
  }

  int numberOfParameters;
  kim_cluster_model->GetNumberOfParameters(&numberOfParameters);
  for (int i=0; i<numberOfParameters; ++i)
  {
    KIM::DataType dataType;
    std::string const * str;
    int extent;
    kim_cluster_model->GetParameterDataTypeExtentAndDescription(
        i, &dataType, &extent, &str);
    std::cout << "Parameter No. " << i
              << " has data type \"" << dataType.String() << "\""
              << " with extent " << extent
              << " and description : " << *str << std::endl;
  }

  // We're compatible with the model. Let's do it.

  error = computeArguments->SetArgumentPointer(
      KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles,
      (int *) &numberOfParticles_cluster)
      || computeArguments->SetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes,
          particleSpecies_cluster_model)
      || computeArguments->SetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::particleContributing,
          particleContributing_cluster_model)
      || computeArguments->SetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::coordinates, (double*) coords_cluster)
      || computeArguments->SetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialEnergy, &energy_cluster_model)
      || computeArguments->SetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialForces, (double*) forces_cluster);
  if (error) MY_ERROR("KIM_API_set_data");
  error = computeArguments->SetCallbackPointer(
      KIM::COMPUTE_CALLBACK_NAME::GetNeighborList,
      KIM::LANGUAGE_NAME::cpp,
      (KIM::func *) &get_cluster_neigh,
      &nl_cluster_model);
  if (error) MY_ERROR("set_call_back");

  kim_cluster_model->GetInfluenceDistance(&influence_distance_cluster_model);
  int const * modelWillNotRequestNeighborsOfNoncontributingParticles;
  kim_cluster_model->GetNeighborListPointers(
      &number_of_neighbor_lists,
      &cutoff_cluster_model,
      &modelWillNotRequestNeighborsOfNoncontributingParticles);
  std::cout << "Model has influence distance of : "
            << influence_distance_cluster_model << std::endl;
  std::cout << "Model has numberOfNeighborLists : " << number_of_neighbor_lists
            << std::endl;
  for (int i=0; i<number_of_neighbor_lists; ++i)
  {
    std::cout
        << "\t" << "Neighbor list " << i << " has cutoff "
        << cutoff_cluster_model[i] << " with "
        "modelWillNotRequestNeighborsOfNoncontributingParticles "
        << modelWillNotRequestNeighborsOfNoncontributingParticles[i]
        << std::endl;
  }
  // ignoring hints from here on...
  if (number_of_neighbor_lists != 1) MY_ERROR("too many neighbor lists");

  /* setup particleSpecies */
  int isSpeciesSupported;
  error = kim_cluster_model->GetSpeciesSupportAndCode(
      KIM::SPECIES_NAME::Ar,
      &isSpeciesSupported,
      &(particleSpecies_cluster_model[0]));
  if (error) MY_ERROR("get_species_code");
  for (i = 1; i < NCLUSTERPARTS; ++i)
    particleSpecies_cluster_model[i] = particleSpecies_cluster_model[0];
  /* setup particleContributing */
  for (i = 0; i < NCLUSTERPARTS; ++i)
    particleContributing_cluster_model[i] = 1;  /* every particle contributes */

  /* setup neighbor lists */
  /* allocate memory for list */
  nl_cluster_model.numberOfParticles = NCLUSTERPARTS;
  nl_cluster_model.NNeighbors = new int[NCLUSTERPARTS];
  if (NULL==nl_cluster_model.NNeighbors) MY_ERROR("new unsuccessful");

  nl_cluster_model.neighborList = new int[NCLUSTERPARTS*NCLUSTERPARTS];
  if (NULL==nl_cluster_model.neighborList) MY_ERROR("new unsuccessful");

  /* ready to compute */
  std::cout << std::setiosflags(std::ios::scientific) << std::setprecision(10);
  std::cout << "This is Test : ex_test_Ar_fcc_cluster_cpp\n";
  std::cout << "--------------------------------------------------------------------------------\n";
  std::cout << "Results for KIM Model : " << modelname << std::endl;

  std::cout << std::setw(20) << "Energy"
            << std::setw(20) << "Force Norm"
            << std::setw(20) << "Lattice Spacing"
            << std::endl;
  for (CurrentSpacing = MinSpacing; CurrentSpacing < MaxSpacing; CurrentSpacing += SpacingIncr)
  {
    /* update coordinates for cluster */
    create_FCC_cluster(CurrentSpacing, NCELLSPERSIDE, &(coords_cluster[0][0]));
    /* compute neighbor lists */
    fcc_cluster_neighborlist(0, NCLUSTERPARTS, &(coords_cluster[0][0]),
                             (*cutoff_cluster_model + cutpad), &nl_cluster_model);

    /* call compute functions */
    error = kim_cluster_model->Compute(computeArguments);
    if (error) MY_ERROR("compute");

    /* compute force norm */
    force_norm = 0.0;
    for (i=0; i < DIM*numberOfParticles_cluster; ++i)
    {
      force_norm += forces_cluster[i]*forces_cluster[i];
    }
    force_norm = sqrt(force_norm);

    /* print the results */
    std::cout << std::setw(20) << energy_cluster_model
              << std::setw(20) << force_norm
              << std::setw(20) << CurrentSpacing
              << std::endl;
  }

  /* call compute arguments destroy */
  error = kim_cluster_model->ComputeArgumentsDestroy(&computeArguments);
  if (error)
  {
    MY_ERROR("Unable to destroy compute arguments");
  }

  /* call model destroy */
  KIM::Model::Destroy(&kim_cluster_model);

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

  nl->cutoff = cutoff;

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

int get_cluster_neigh(void * const dataObject,
                      int const numberOfNeighborLists,
                      double const * const cutoffs,
                      int const neighborListIndex, int const particleNumber,
                      int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle)
{
  /* local variables */
  int error = true;
  NeighList* nl = (NeighList*) dataObject;
  int numberOfParticles = nl->numberOfParticles;

  if ((numberOfNeighborLists != 1) || (cutoffs[0] > nl->cutoff)) return error;

  if (neighborListIndex != 0) return error;

  /* initialize numNeigh */
  *numberOfNeighbors = 0;

  if ((particleNumber >= numberOfParticles) || (particleNumber < 0)) /* invalid id */
  {
    MY_WARNING("Invalid part ID in get_cluster_neigh");
    return true;
  }

  /* set the returned number of neighbors for the returned part */
  *numberOfNeighbors = (*nl).NNeighbors[particleNumber];

  /* set the location for the returned neighbor list */
  *neighborsOfParticle = &((*nl).neighborList[(particleNumber)*numberOfParticles]);

  return false;
}
