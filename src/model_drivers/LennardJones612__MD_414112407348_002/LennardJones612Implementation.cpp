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
// Copyright (c) 2013--2015, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//    Stephen M. Whalen
//    Andrew Akerson
//


#include <cmath>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>

#include "LennardJones612.hpp"
#include "LennardJones612Implementation.hpp"
#include "KIM_API_status.h"

#define MAXLINE 1024
#define IGNORE_RESULT(fn) if(fn){}


//==============================================================================
//
// Implementation of LennardJones612Implementation public member functions
//
//==============================================================================

//******************************************************************************
LennardJones612Implementation::LennardJones612Implementation(
    KIM_API_model* const pkim,
    char const* const  parameterFileNames,
    int const parameterFileNameLength,
    int const numberParameterFiles,
    int* const ier)
    : numberOfSpeciesIndex_(-1),  // initizlize index, pointer, and cached
      numberOfParticlesIndex_(-1),    // member variables
      numberContributingParticlesIndex_(-1),
      particleSpeciesIndex_(-1),
      coordinatesIndex_(-1),
      boxSideLengthsIndex_(-1),
      get_neighIndex_(-1),
      process_dEdrIndex_(-1),
      process_d2Edr2Index_(-1),
      cutoffIndex_(-1),
      energyIndex_(-1),
      forcesIndex_(-1),
      particleEnergyIndex_(-1),
      numberModelSpecies_(0),
      numberUniqueSpeciesPairs_(0),
      shift_(0),
      cutoffs_(0),
      epsilons_(0),
      sigmas_(0),
      cutoffsSq2D_(0),
      fourEpsilonSigma6_2D_(0),
      fourEpsilonSigma12_2D_(0),
      twentyFourEpsilonSigma6_2D_(0),
      fortyEightEpsilonSigma12_2D_(0),
      oneSixtyEightEpsilonSigma6_2D_(0),
      sixTwentyFourEpsilonSigma12_2D_(0),
      shifts2D_(0),
      cachedNumberOfParticles_(0),
      cachedNumberContributingParticles_(0)
{
  *ier = SetConstantValues(pkim);
  if (*ier < KIM_STATUS_OK) return;

  AllocateFreeParameterMemory();

  FILE* parameterFilePointers[MAX_PARAMETER_FILES];
  *ier = OpenParameterFiles(pkim, parameterFileNames, parameterFileNameLength,
                            numberParameterFiles, parameterFilePointers);
  if (*ier < KIM_STATUS_OK) return;

  *ier = ProcessParameterFiles(pkim, parameterFilePointers,
                               numberParameterFiles);
  CloseParameterFiles(parameterFilePointers, numberParameterFiles);
  if (*ier < KIM_STATUS_OK) return;

  *ier = ConvertUnits(pkim);
  if (*ier < KIM_STATUS_OK) return;

  *ier = SetReinitMutableValues(pkim);
  if (*ier < KIM_STATUS_OK) return;

  *ier = RegisterKIMParameters(pkim);
  if (*ier < KIM_STATUS_OK) return;

  *ier = RegisterKIMFunctions(pkim);
  if (*ier < KIM_STATUS_OK) return;

  // everything is good
  *ier = KIM_STATUS_OK;
  return;
}

//******************************************************************************
LennardJones612Implementation::~LennardJones612Implementation()
{ // note: it is ok to delete a null pointer and we have ensured that
  // everything is initialized to null

  delete [] cutoffs_;
  Deallocate2DArray(cutoffsSq2D_);
  delete [] epsilons_;
  delete [] sigmas_;
  Deallocate2DArray(fourEpsilonSigma6_2D_);
  Deallocate2DArray(fourEpsilonSigma12_2D_);
  Deallocate2DArray(twentyFourEpsilonSigma6_2D_);
  Deallocate2DArray(fortyEightEpsilonSigma12_2D_);
  Deallocate2DArray(oneSixtyEightEpsilonSigma6_2D_);
  Deallocate2DArray(sixTwentyFourEpsilonSigma12_2D_);
  Deallocate2DArray(shifts2D_);
}

//******************************************************************************
int LennardJones612Implementation::Reinit(KIM_API_model* const pkim)
{
  int ier;

  ier = SetReinitMutableValues(pkim);
  if (ier < KIM_STATUS_OK) return ier;

  // nothing else to do for this case

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::Compute(KIM_API_model* const pkim)
{
  int ier;

  // KIM API Model Input compute flags
  bool isComputeProcess_dEdr;
  bool isComputeProcess_d2Edr2;
  //
  // KIM API Model Output compute flags
  bool isComputeEnergy;
  bool isComputeForces;
  bool isComputeParticleEnergy;
  //
  // KIM API Model Input
  int const* particleSpecies = 0;
  GetNeighborFunction * get_neigh = 0;
  double const* boxSideLengths = 0;
  VectorOfSizeDIM const* coordinates = 0;
  //
  // KIM API Model Output
  double* energy = 0;
  double* particleEnergy = 0;
  VectorOfSizeDIM* forces = 0;
  ier = SetComputeMutableValues(pkim, isComputeProcess_dEdr,
                                isComputeProcess_d2Edr2, isComputeEnergy,
                                isComputeForces, isComputeParticleEnergy,
                                particleSpecies, get_neigh, boxSideLengths,
                                coordinates, energy, particleEnergy, forces);
  if (ier < KIM_STATUS_OK) return ier;

  // Skip this check for efficiency
  //
  // ier = CheckParticleSpecies(pkim, particleSpecies);
  // if (ier < KIM_STATUS_OK) return ier;

  bool const isShift = (1 == shift_);

#include "LennardJones612ImplementationComputeDispatch.cpp"
  return ier;
}

//==============================================================================
//
// Implementation of LennardJones612Implementation private member functions
//
//==============================================================================

//******************************************************************************
int LennardJones612Implementation::SetConstantValues(KIM_API_model* const pkim)
{
  int ier = KIM_STATUS_FAIL;

  // get baseconvert value from KIM API object
  baseconvert_ = pkim->get_model_index_shift();

  ier = DetermineNBCTypeAndHalf(pkim);
  if (ier < KIM_STATUS_OK) return ier;

  // set isLocator
  if (NBCType_ != Cluster)
  { // all NBC cases except CLUSTER
    isLocatorMode_ = pkim->get_neigh_mode(&ier) == 2;  // Locator mode
    if (ier < KIM_STATUS_OK)
    {
      pkim->report_error(__LINE__, __FILE__, "get_neigh_mode", ier);
      return ier;
    }
  }
  else
  {  // use true as a default value for CLUSTER
    isLocatorMode_ = true;
  }

  // obtain indices for various KIM API Object arguments
  pkim->getm_index(
      &ier, 3 * 13,
      "numberOfSpecies",             &numberOfSpeciesIndex_,             1,
      "numberOfParticles",           &numberOfParticlesIndex_,           1,
      "numberContributingParticles", &numberContributingParticlesIndex_, 1,
      "particleSpecies",             &particleSpeciesIndex_,             1,
      "coordinates",                 &coordinatesIndex_,                 1,
      "boxSideLengths",              &boxSideLengthsIndex_,              1,
      "get_neigh",                   &get_neighIndex_,                   1,
      "process_dEdr",                &process_dEdrIndex_,                1,
      "process_d2Edr2",              &process_d2Edr2Index_,              1,
      "cutoff",                      &cutoffIndex_,                      1,
      "energy",                      &energyIndex_,                      1,
      "forces",                      &forcesIndex_,                      1,
      "particleEnergy",              &particleEnergyIndex_,              1);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "getm_index", ier);
    return ier;
  }

  // set numberModelSpecies & numberUniqueSpeciesPairs
  int dummy;
  ier = pkim->get_num_model_species(&numberModelSpecies_, &dummy);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_num_model_species", ier);
    return ier;
  }
  numberUniqueSpeciesPairs_ = ((numberModelSpecies_+1)*numberModelSpecies_)/2;

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::DetermineNBCTypeAndHalf(
    KIM_API_model* const pkim)
{
  int ier;
  const char* NBC;

  // record the active NBC method
  ier = pkim->get_NBC_method(&NBC);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_NBC_method", ier);
    return ier;
  }
  if (strcmp(NBC, "NEIGH_RVEC_H") == 0)
  {
    NBCType_ = Neigh_Rvec;
    isHalf_ = true;
  }
  else if (strcmp(NBC, "NEIGH_PURE_H") == 0)
  {
    NBCType_ = Neigh_Pure;
    isHalf_ = true;
  }
  else if (strcmp(NBC, "NEIGH_RVEC_F") == 0)
  {
    NBCType_ = Neigh_Rvec;
    isHalf_ = false;
  }
  else if (strcmp(NBC, "NEIGH_PURE_F") == 0)
  {
    NBCType_ = Neigh_Pure;
    isHalf_ = false;
  }
  else if (strcmp(NBC, "MI_OPBC_H") == 0)
  {
    NBCType_ = Mi_Opbc;
    isHalf_ = true;
  }
  else if (strcmp(NBC, "MI_OPBC_F") == 0)
  {
    NBCType_ = Mi_Opbc;
    isHalf_ = false;
  }
  else if (strcmp(NBC, "CLUSTER") == 0)
  {
    NBCType_ = Cluster;
    isHalf_ = true;
  }
  else
  {
    ier = KIM_STATUS_FAIL;
    pkim->report_error(__LINE__, __FILE__,
                       "(eam_init_) unknown NBC type", ier);
    return ier;
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::OpenParameterFiles(
    KIM_API_model* const pkim,
    char const* const parameterFileNames,
    int const parameterFileNameLength,
    int const numberParameterFiles,
    FILE* parameterFilePointers[MAX_PARAMETER_FILES])
{
  int ier;

  if (numberParameterFiles > MAX_PARAMETER_FILES)
  {
    ier = KIM_STATUS_FAIL;
    pkim->report_error(__LINE__, __FILE__, "LennardJones612 given too many"
                       " parameter files", ier);
    return ier;
  }

  for (int i = 0; i < numberParameterFiles; ++i)
  {
    parameterFilePointers[i]
        = fopen(&parameterFileNames[i * parameterFileNameLength], "r");
    if (parameterFilePointers[i] == 0)
    {
      char message[MAXLINE];
      sprintf(message,
              "LennardJones612 parameter file number %d cannot be opened",
              i);
      ier = KIM_STATUS_FAIL;
      pkim->report_error(__LINE__, __FILE__, message, ier);
      for (int j = i - 1; i <= 0; --i)
      {
        fclose(parameterFilePointers[j]);
      }
      return ier;
    }
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::ProcessParameterFiles(
    KIM_API_model* const pkim,
    FILE* const parameterFilePointers[MAX_PARAMETER_FILES],
    int const numberParameterFiles)
{
  int N, ier;
  int endOfFileFlag = 0;
  char spec1[MAXLINE], spec2[MAXLINE], nextLine[MAXLINE];
  char *nextLinePtr;
  int iIndex, jIndex , indx, iiIndex, jjIndex;
  double nextCutoff, nextEpsilon, nextSigma;
  nextLinePtr = nextLine;

  getNextDataLine(parameterFilePointers[0], nextLinePtr,
                  MAXLINE, &endOfFileFlag);
  ier = sscanf(nextLine, "%d %d", &N, &shift_);
  if (ier != 2)
  {
    sprintf(nextLine, "unable to read first line of the parameter file");
    ier = KIM_STATUS_FAIL;
    pkim->report_error(__LINE__, __FILE__, nextLine, ier);
    fclose(parameterFilePointers[0]);
    return ier;
  }
  if (N != numberModelSpecies_)
  {
    sprintf(nextLine, "The value for N from the parameter file is inconsistent "
            "with numberModelSpecies_");
    ier = KIM_STATUS_FAIL;
    pkim->report_error(__LINE__, __FILE__, nextLine, ier);
    fclose(parameterFilePointers[0]);
    return ier;
  }

  // get and correctly order the particle names
  const char** const particleNames = new const char*[numberModelSpecies_];
  for (int i = 0; i < numberModelSpecies_; ++i)
  {
    const char* kimModelParticleSpecies;
    ier = pkim->get_model_species(i, &kimModelParticleSpecies);
    if (ier < KIM_STATUS_OK)
    {
      pkim->report_error(__LINE__, __FILE__, "get_model_species", ier);
      delete [] particleNames;
      return ier;
    }
    int const index = pkim->get_species_code(kimModelParticleSpecies, &ier);
    if (index >= numberModelSpecies_)
    {
      pkim->report_error(__LINE__, __FILE__, "get_species_code",
                         KIM_STATUS_FAIL);
      delete [] particleNames;
      return KIM_STATUS_FAIL;
    }
    particleNames[index] = kimModelParticleSpecies;
  }

  // set all values in the arrays to -1 for mixing later
  for (int i = 0; i < ((N+1)*N/2); i++)
  {
    cutoffs_[i]  = -1;
    epsilons_[i] = -1;
    sigmas_[i] = -1;
  }

  // Read and process data lines
  getNextDataLine(parameterFilePointers[0], nextLinePtr,
                  MAXLINE, &endOfFileFlag);
  while (endOfFileFlag == 0)
  {
    ier = sscanf(nextLine, "%s  %s %lg %lg %lg",
	         spec1, spec2, &nextCutoff, &nextEpsilon, &nextSigma);
    if (ier != 5)
    {
      sprintf(nextLine, "error reading lines of the parameter file");
      pkim->report_error(__LINE__, __FILE__, nextLine, KIM_STATUS_FAIL);
      delete [] particleNames;
      return KIM_STATUS_FAIL;
    }
    iIndex = jIndex = -1;
    for (int i = 0; i <  N; i++)
    {
      if (strcmp(spec1, particleNames[i]) == 0)
      {
        iIndex = i;
      }
      if (strcmp(spec2, particleNames[i]) == 0)
      {
        jIndex = i;
      }
    }
    if ((iIndex == -1) || (jIndex == -1))
    {
      sprintf(nextLine, "Unsupported Species name found in parameter file");
      pkim->report_error(__LINE__, __FILE__, nextLine, KIM_STATUS_FAIL);
      delete [] particleNames;
      return KIM_STATUS_FAIL;
    }
    if (iIndex >= jIndex)
    {
      indx = jIndex*N + iIndex - (jIndex*jIndex + jIndex)/2;
    }
    else
    {
      indx = iIndex*N + jIndex - (iIndex*iIndex + iIndex)/2;
    }
    cutoffs_[indx] = nextCutoff;
    epsilons_[indx] = nextEpsilon;
    sigmas_[indx] = nextSigma;

    getNextDataLine(parameterFilePointers[0], nextLinePtr,
                    MAXLINE, &endOfFileFlag);
  }

  // check that we got all like - like pairs
  sprintf(nextLine, "There are not values for like-like pairs of:");
  for (int i = 0; i < N; i++)
  {
    if (cutoffs_[(i*N + i - (i*i + i)/2)] == -1)
    {
      strcat(nextLine, "  ");
      strcat(nextLine, particleNames[i]);
      ier = -1;
    }
  }
  if (ier == -1)
  {
    pkim->report_error(__LINE__, __FILE__, nextLine, KIM_STATUS_FAIL);
    delete [] particleNames;
    return KIM_STATUS_FAIL;
  }

  // Perform Mixing if nessisary
  for (int jIndex = 0; jIndex < N; jIndex++)
  {
    jjIndex = (jIndex*N + jIndex - (jIndex*jIndex + jIndex)/2);
    for (int iIndex = (jIndex+1) ; iIndex < N; iIndex++)
    {
      indx = jIndex*N + iIndex - (jIndex*jIndex + jIndex)/2;
      if (cutoffs_[indx] == -1)
      {
        iiIndex = (iIndex*N + iIndex - (iIndex*iIndex + iIndex)/2);
        epsilons_[indx] = sqrt(epsilons_[iiIndex]*epsilons_[jjIndex]);
        sigmas_[indx] = (sigmas_[iiIndex] + sigmas_[jjIndex])/2.0;
        cutoffs_[indx] = (cutoffs_[iiIndex] + cutoffs_[jjIndex])/2.0;
      }
    }
  }
  delete [] particleNames;

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
void LennardJones612Implementation::getNextDataLine(
    FILE* const filePtr, char* nextLinePtr, int const maxSize,
    int *endOfFileFlag)
{
  do
  {
    if(fgets(nextLinePtr, maxSize, filePtr) == NULL)
    {
       *endOfFileFlag = 1;
       break;
    }
    while ((nextLinePtr[0] == ' ' || nextLinePtr[0] == '\t') ||
           (nextLinePtr[0] == '\n' || nextLinePtr[0] == '\r' ))
    {
      nextLinePtr = (nextLinePtr + 1);
    }
  }
  while ((strncmp("#", nextLinePtr, 1) == 0) || (strlen(nextLinePtr) == 0));
}

//******************************************************************************
void LennardJones612Implementation::CloseParameterFiles(
    FILE* const parameterFilePointers[MAX_PARAMETER_FILES],
    int const numberParameterFiles)
{
  for (int i = 0; i < numberParameterFiles; ++i)
    fclose(parameterFilePointers[i]);
}

//******************************************************************************
void LennardJones612Implementation::AllocateFreeParameterMemory()
{ // allocate memory for data
  cutoffs_ = new double[numberUniqueSpeciesPairs_];
  AllocateAndInitialize2DArray(cutoffsSq2D_, numberModelSpecies_,
                               numberModelSpecies_);

  epsilons_ = new double[numberUniqueSpeciesPairs_];
  sigmas_ = new double[numberUniqueSpeciesPairs_];
  AllocateAndInitialize2DArray(fourEpsilonSigma6_2D_, numberModelSpecies_,
                               numberModelSpecies_);
  AllocateAndInitialize2DArray(fourEpsilonSigma12_2D_, numberModelSpecies_,
                               numberModelSpecies_);
  AllocateAndInitialize2DArray(twentyFourEpsilonSigma6_2D_, numberModelSpecies_,
                               numberModelSpecies_);
  AllocateAndInitialize2DArray(fortyEightEpsilonSigma12_2D_,
                               numberModelSpecies_, numberModelSpecies_);
  AllocateAndInitialize2DArray(oneSixtyEightEpsilonSigma6_2D_,
                               numberModelSpecies_, numberModelSpecies_);
  AllocateAndInitialize2DArray(sixTwentyFourEpsilonSigma12_2D_,
                               numberModelSpecies_, numberModelSpecies_);

  AllocateAndInitialize2DArray(shifts2D_, numberModelSpecies_,
                               numberModelSpecies_);
}

//******************************************************************************
int LennardJones612Implementation::ConvertUnits(KIM_API_model* const pkim)
{
  int ier;

  // define default base units
  char length[] = "A";
  char energy[] = "eV";
  char charge[] = "e";
  char temperature[] = "K";
  char time[] = "ps";

  // changing units of cutoffs and sigmas
  double const convertLength
      = pkim->convert_to_act_unit(length, energy, charge, temperature, time,
                                  1.0, 0.0, 0.0, 0.0, 0.0, &ier);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "convert_to_act_unit", ier);
    return ier;
  }
  if (convertLength != ONE)
  {
    for (int i = 0; i < numberUniqueSpeciesPairs_; ++i)
    {
      cutoffs_[i] *= convertLength;  // convert to active units
      sigmas_[i] *= convertLength;  // convert to active units
    }
  }
  // changing units of epsilons
  double const convertEnergy
      = pkim->convert_to_act_unit(length, energy, charge, temperature, time,
                                  0.0, 1.0, 0.0, 0.0, 0.0, &ier);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "convert_to_act_unit", ier);
    return ier;
  }
  if (convertEnergy != ONE)
  {
    for (int i = 0; i < numberUniqueSpeciesPairs_; ++i)
    {
      epsilons_[i] *= convertEnergy;  // convert to active units
    }
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::RegisterKIMParameters(
    KIM_API_model* const pkim) const
{
  int ier;

  // publish parameters
  pkim->setm_data(&ier, 4 * 4,
                  "PARAM_FREE_shift", 1,
                  (void*) &shift_,
                  1,
                  //
                  "PARAM_FREE_cutoffs",
                  numberUniqueSpeciesPairs_,
                  (void*) cutoffs_,
                  1,
                  //
                  "PARAM_FREE_epsilons",
                  numberUniqueSpeciesPairs_,
                  (void*) epsilons_,
                  1,
                  //
                  "PARAM_FREE_sigmas",
                  numberUniqueSpeciesPairs_,
                  (void*) sigmas_,
                  1);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "setm_data", ier);
    return ier;
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::RegisterKIMFunctions(
    KIM_API_model* const pkim)
    const
{
  int ier;

  // register the destroy() and reinit() functions
  pkim->setm_method(&ier, 3 * 4,
                    "destroy", 1, (func_ptr) &(LennardJones612::Destroy), 1,
                    "reinit",  1, (func_ptr) &(LennardJones612::Reinit),  1,
                    "compute", 1, (func_ptr) &(LennardJones612::Compute), 1);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "setm_method", ier);
    return ier;
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::SetReinitMutableValues(
    KIM_API_model* const pkim)
{ // use (possibly) new values of free parameters to compute other quantities
  int ier;

  // update cutoffsSq, epsilons, and sigmas
  for (int i = 0; i < numberModelSpecies_; ++i)
  {
    for (int j = 0; j <= i ; ++j)
    {
      int const index = j*numberModelSpecies_ + i - (j*j + j)/2;
      cutoffsSq2D_[i][j] = cutoffsSq2D_[j][i]
          = (cutoffs_[index]*cutoffs_[index]);
      fourEpsilonSigma6_2D_[i][j] = fourEpsilonSigma6_2D_[j][i]
          = 4.0*epsilons_[index]*pow(sigmas_[index],6.0);
      fourEpsilonSigma12_2D_[i][j] = fourEpsilonSigma12_2D_[j][i]
          = 4.0*epsilons_[index]*pow(sigmas_[index],12.0);
      twentyFourEpsilonSigma6_2D_[i][j] = twentyFourEpsilonSigma6_2D_[j][i]
          = 6.0*fourEpsilonSigma6_2D_[i][j];
      fortyEightEpsilonSigma12_2D_[i][j] = fortyEightEpsilonSigma12_2D_[j][i]
          = 12.0*fourEpsilonSigma12_2D_[i][j];
      oneSixtyEightEpsilonSigma6_2D_[i][j]
          = oneSixtyEightEpsilonSigma6_2D_[j][i]
          = 7.0*twentyFourEpsilonSigma6_2D_[i][j];
      sixTwentyFourEpsilonSigma12_2D_[i][j]
          = sixTwentyFourEpsilonSigma12_2D_[j][i]
          = 13.0*fortyEightEpsilonSigma12_2D_[i][j];
    }
  }

  // get cutoff pointer
  double* const cutoff
      = static_cast<double*>(pkim->get_data_by_index(cutoffIndex_, &ier));
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_data_by_index", ier);
    return ier;
  }

  // update cutoff value in KIM API object
  *cutoff = 0;
  int numberSpecies, maxStringLength;
  ier = pkim->get_num_sim_species(&numberSpecies, &maxStringLength);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "get_num_sim_species", ier);
    return ier;
  }
  const char* simSpeciesI;
  const char* simSpeciesJ;
  for (int i = 0; i < numberSpecies; i++)
  {
    ier = pkim->get_sim_species( i, &simSpeciesI);
    if (ier < KIM_STATUS_OK)
    {
      pkim->report_error(__LINE__, __FILE__, "get_num_sim_species", ier);
      return ier;
    }
    int const indexI = pkim->get_species_code(simSpeciesI, &ier);
    if (indexI >= numberModelSpecies_ || ier<KIM_STATUS_OK )
    {
      pkim->report_error(__LINE__, __FILE__, "get_species_code",
                         KIM_STATUS_FAIL);
      return KIM_STATUS_FAIL;
    }

    for (int j = 0; j < numberSpecies; j++)
    {
      ier = pkim->get_sim_species( i, &simSpeciesJ);
      if (ier < KIM_STATUS_OK)
      {
        pkim->report_error(__LINE__, __FILE__, "get_num_sim_species", ier);
      return ier;
      }
      int const indexJ = pkim->get_species_code(simSpeciesJ, &ier);
      if (indexJ >= numberModelSpecies_ || ier<KIM_STATUS_OK )
      {
        pkim->report_error(__LINE__, __FILE__, "get_species_code",
                           KIM_STATUS_FAIL);
        return KIM_STATUS_FAIL;
      }
      if (*cutoff < cutoffsSq2D_[indexI][indexJ])
      {
        *cutoff = cutoffsSq2D_[indexI][indexJ];
      }
    }
  }
  *cutoff = sqrt(*cutoff);

  // update shifts
  // compute and set shifts2D_ check if minus sign
  double const* const* const  constFourEpsSig6_2D = fourEpsilonSigma6_2D_;
  double const* const* const  constFourEpsSig12_2D = fourEpsilonSigma12_2D_;
  if (1 == shift_)
  {
    double phi;
    for (int iSpecies = 0; iSpecies < numberModelSpecies_; iSpecies++)
    {
      for(int jSpecies = 0; jSpecies <= iSpecies; jSpecies++)
      {
        int const index = jSpecies*numberModelSpecies_ + iSpecies
                           - (jSpecies*jSpecies + jSpecies)/2;
        double const rij2 = cutoffs_[index]*cutoffs_[index];
        double const r2iv = 1.0/rij2;
        double const r6iv = r2iv*r2iv*r2iv;
        LENNARD_JONES_PHI(;);
        shifts2D_[iSpecies][jSpecies] = shifts2D_[jSpecies][iSpecies] = phi;
      }
    }
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::SetComputeMutableValues(
    KIM_API_model* const pkim,
    bool& isComputeProcess_dEdr,
    bool& isComputeProcess_d2Edr2,
    bool& isComputeEnergy,
    bool& isComputeForces,
    bool& isComputeParticleEnergy,
    int const*& particleSpecies,
    GetNeighborFunction *& get_neigh,
    double const*& boxSideLengths,
    VectorOfSizeDIM const*& coordinates,
    double*& energy,
    double*& particleEnergy,
    VectorOfSizeDIM*& forces)
{
  int ier = KIM_STATUS_FAIL;

  // get compute flags
  int compEnergy;
  int compForces;
  int compParticleEnergy;
  int compProcess_dEdr;
  int compProcess_d2Edr2;
  pkim->getm_compute_by_index(&ier, 3 * 5,
                              energyIndex_,         &compEnergy,         1,
                              forcesIndex_,         &compForces,         1,
                              particleEnergyIndex_, &compParticleEnergy, 1,
                              process_dEdrIndex_,   &compProcess_dEdr,   1,
                              process_d2Edr2Index_, &compProcess_d2Edr2, 1);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "getm_compute_by_index", ier);
    return ier;
  }

  isComputeEnergy = (compEnergy == KIM_COMPUTE_TRUE);
  isComputeForces = (compForces == KIM_COMPUTE_TRUE);
  isComputeParticleEnergy = (compParticleEnergy == KIM_COMPUTE_TRUE);
  isComputeProcess_dEdr = (compProcess_dEdr == KIM_COMPUTE_TRUE);
  isComputeProcess_d2Edr2 = (compProcess_d2Edr2 == KIM_COMPUTE_TRUE);

  // extract pointers based on compute flags
  //
  // double const* cutoff;            // currently unused
  // int const* numberOfSpecies;  // currently unused
  int const* numberOfParticles;
  int const* numberContributingParticles;
  pkim->getm_data_by_index(
      &ier, 3 * 8,
      // cutoffIndex_, &cutoff, 1,
      // numberOfSpeciesIndex_, &numberOfSpecies, 1,
      numberOfParticlesIndex_, &numberOfParticles, 1,
      numberContributingParticlesIndex_, &numberContributingParticles, isHalf_,
      particleSpeciesIndex_, &particleSpecies, 1,
      boxSideLengthsIndex_, &boxSideLengths, (NBCType_ == Mi_Opbc),
      coordinatesIndex_, &coordinates, 1,
      energyIndex_, &energy, compEnergy,
      particleEnergyIndex_, &particleEnergy, compParticleEnergy,
      forcesIndex_, &forces, compForces);
  if (ier < KIM_STATUS_OK)
  {
    pkim->report_error(__LINE__, __FILE__, "getm_data_by_index", ier);
    return ier;
  }
  if (NBCType_ != Cluster)
  {
    get_neigh = (GetNeighborFunction *)
        pkim->get_method_by_index(get_neighIndex_, &ier);
    if (ier < KIM_STATUS_OK)
    {
      pkim->report_error(__LINE__, __FILE__, "get_method_by_index", ier);
      return ier;
    }
  }

  // update values
  cachedNumberOfParticles_ = *numberOfParticles;

  // set cachedNumberContributingParticles based on half/full neighbor list
  if ((isHalf_) && (NBCType_ != Cluster))
  {
    cachedNumberContributingParticles_ = *numberContributingParticles;
  }
  else
  { // set so that it can be used even with a full neighbor list
    cachedNumberContributingParticles_ = *numberOfParticles;
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::CheckParticleSpecies(
    KIM_API_model* const pkim,
    int const* const particleSpecies)
    const
{
  int ier;
  for (int i = 0; i < cachedNumberOfParticles_; ++i)
  {
    if ((particleSpecies[i] < 0) || (particleSpecies[i] >= numberModelSpecies_))
    {
      ier = KIM_STATUS_FAIL;
      pkim->report_error(__LINE__, __FILE__,
                         "unsupported particle species detected", ier);
      return ier;
    }
  }

  // everything is good
  ier = KIM_STATUS_OK;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::GetComputeIndex(
    const bool& isComputeProcess_dEdr,
    const bool& isComputeProcess_d2Edr2,
    const bool& isComputeEnergy,
    const bool& isComputeForces,
    const bool& isComputeParticleEnergy,
    const bool& isShift) const
{
  // const int iter = 3;
  const int half = 2;
  const int rij = 3;
  const int processdE = 2;
  const int processd2E = 2;
  const int energy = 2;
  const int force = 2;
  const int particleEnergy = 2;
  const int shift = 2;


  int index = 0;

  // iter
  //  Cluster  = 0
  //        (for EAM when NBCType_==Cluster then isLocatorMode_==true)
  //  Locator  = 1
  //  Iterator = 2
  index += (int(NBCType_ != Cluster) + int(!isLocatorMode_))
      * half * rij * processdE * processd2E * energy * force * particleEnergy
      * shift;

  // half
  index += (int(isHalf_))
      * rij * processdE * processd2E * energy * force * particleEnergy * shift;

  // rij
  //  Coordinates = 0
  //       (NBCType_ != Neigh_Rvec and != Mi_OPbc)
  //  RVec        = 1
  //  Mi_Opbc     = 2
  index += (int(NBCType_ == Neigh_Rvec) + 2*int(NBCType_ == Mi_Opbc))
      * processdE * processd2E * energy * force * particleEnergy * shift;

  // processdE
  index += (int(isComputeProcess_dEdr))
      * processd2E * energy * force * particleEnergy * shift;

  // processd2E
  index += (int(isComputeProcess_d2Edr2))
      * energy * force * particleEnergy * shift;

  // energy
  index += (int(isComputeEnergy))
      * force * particleEnergy * shift;

  // force
  index += (int(isComputeForces))
      * particleEnergy * shift;

  // particleEnergy
  index += (int(isComputeParticleEnergy))
      * shift;

  // shift
  index += (int(isShift));

  return index;
}

//******************************************************************************
void LennardJones612Implementation::ApplyMIOPBC(
    double const* const boxSideLengths,
    double* const dx)
{
  double sign;
  for (int i = 0; i < DIMENSION; ++i)
  {
    sign = dx[i] > 0 ? ONE : -ONE;
    dx[i] = (abs(dx[i]) > HALF * boxSideLengths[i]) ? dx[i]
        - sign * boxSideLengths[i] : dx[i];
  }
}

//==============================================================================
//
// Implementation of helper functions
//
//==============================================================================

//******************************************************************************
void AllocateAndInitialize2DArray(double**& arrayPtr, int const extentZero,
                                  int const extentOne)
{ // allocate memory and set pointers
  arrayPtr = new double*[extentZero];
  arrayPtr[0] = new double[extentZero * extentOne];
  for (int i = 1; i < extentZero; ++i)
  {
    arrayPtr[i] = arrayPtr[i-1] + extentOne;
  }

  // initialize
  for (int i = 0; i < extentZero; ++i)
  {
    for (int j = 0; j < extentOne; ++j)
    {
      arrayPtr[i][j] = 0.0;
    }
  }
}

//******************************************************************************
void Deallocate2DArray(double**& arrayPtr)
{ // deallocate memory
  if (arrayPtr != 0) delete [] arrayPtr[0];
  delete [] arrayPtr;

  // nullify pointer
  arrayPtr = 0;
}
