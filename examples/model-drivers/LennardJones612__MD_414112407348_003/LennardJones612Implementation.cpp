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
#include <map>

#include "LennardJones612Implementation.hpp"
#include "KIM_ModelDriverHeaders.hpp"

#define MAXLINE 1024
#define IGNORE_RESULT(fn) if(fn){}


//==============================================================================
//
// Implementation of LennardJones612Implementation public member functions
//
//==============================================================================

//******************************************************************************
#include "KIM_ModelDriverCreateLogMacros.hpp"
LennardJones612Implementation::LennardJones612Implementation(
    KIM::ModelDriverCreate * const modelDriverCreate,
    KIM::LengthUnit const requestedLengthUnit,
    KIM::EnergyUnit const requestedEnergyUnit,
    KIM::ChargeUnit const requestedChargeUnit,
    KIM::TemperatureUnit const requestedTemperatureUnit,
    KIM::TimeUnit const requestedTimeUnit,
    int * const ier)
: numberModelSpecies_(0),
  numberUniqueSpeciesPairs_(0),
  shift_(0),
  cutoffs_(NULL),
  epsilons_(NULL),
  sigmas_(NULL),
  influenceDistance_(0.0),
  cutoffsSq2D_(NULL),
  modelWillNotRequestNeighborsOfNoncontributingParticles_(1),
  fourEpsilonSigma6_2D_(NULL),
  fourEpsilonSigma12_2D_(NULL),
  twentyFourEpsilonSigma6_2D_(NULL),
  fortyEightEpsilonSigma12_2D_(NULL),
  oneSixtyEightEpsilonSigma6_2D_(NULL),
  sixTwentyFourEpsilonSigma12_2D_(NULL),
  shifts2D_(NULL),
  cachedNumberOfParticles_(0)
{
  FILE* parameterFilePointers[MAX_PARAMETER_FILES];
  int numberParameterFiles;
  modelDriverCreate->GetNumberOfParameterFiles(
      &numberParameterFiles);
  *ier = OpenParameterFiles(modelDriverCreate, numberParameterFiles,
                            parameterFilePointers);
  if (*ier) return;

  *ier = ProcessParameterFiles(modelDriverCreate,
                               numberParameterFiles,
                               parameterFilePointers);
  CloseParameterFiles(numberParameterFiles, parameterFilePointers);
  if (*ier) return;

  *ier = ConvertUnits(modelDriverCreate,
                      requestedLengthUnit,
                      requestedEnergyUnit,
                      requestedChargeUnit,
                      requestedTemperatureUnit,
                      requestedTimeUnit);
  if (*ier) return;

  *ier = SetRefreshMutableValues(modelDriverCreate);
  if (*ier) return;

  *ier = RegisterKIMModelSettings(modelDriverCreate);
  if (*ier) return;

  *ier = RegisterKIMParameters(modelDriverCreate);
  if (*ier) return;

  *ier = RegisterKIMFunctions(modelDriverCreate);
  if (*ier) return;

  // everything is good
  *ier = false;
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
#include "KIM_ModelRefreshLogMacros.hpp"
int LennardJones612Implementation::Refresh(
    KIM::ModelRefresh * const modelRefresh)
{
  int ier;

  ier = SetRefreshMutableValues(modelRefresh);
  if (ier) return ier;

  // nothing else to do for this case

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::Compute(
    KIM::ModelCompute const * const modelCompute,
    KIM::ModelComputeArguments const * const modelComputeArguments)
{
  int ier;

  // KIM API Model Input compute flags
  bool isComputeProcess_dEdr = false;
  bool isComputeProcess_d2Edr2 = false;
  //
  // KIM API Model Output compute flags
  bool isComputeEnergy = false;
  bool isComputeForces = false;
  bool isComputeParticleEnergy = false;
  bool isComputeVirial = false;
  bool isComputeParticleVirial = false;
  //
  // KIM API Model Input
  int const* particleSpeciesCodes = NULL;
  int const* particleContributing = NULL;
  VectorOfSizeDIM const* coordinates = NULL;
  //
  // KIM API Model Output
  double* energy = NULL;
  double* particleEnergy = NULL;
  VectorOfSizeDIM* forces = NULL;
  VectorOfSizeSix* virial = NULL;
  VectorOfSizeSix* particleVirial = NULL;
  ier = SetComputeMutableValues(modelComputeArguments,
                                isComputeProcess_dEdr,
                                isComputeProcess_d2Edr2, isComputeEnergy,
                                isComputeForces, isComputeParticleEnergy,
                                isComputeVirial, isComputeParticleVirial,
                                particleSpeciesCodes, particleContributing,
                                coordinates, energy, particleEnergy, forces,
                                virial, particleVirial);
  if (ier) return ier;

  // Skip this check for efficiency
  //
  //ier = CheckParticleSpecies(modelComputeArguments, particleSpeciesCodes);
  //if (ier) return ier;

  bool const isShift = (1 == shift_);

#include "LennardJones612ImplementationComputeDispatch.cpp"
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::ComputeArgumentsCreate(
    KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate) const
{
  int ier;

  ier = RegisterKIMComputeArgumentsSettings(modelComputeArgumentsCreate);
  if (ier) return ier;

  // nothing else to do for this case

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::ComputeArgumentsDestroy(
    KIM::ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
    const
{
  int ier;

  // nothing else to do for this case

  (void)modelComputeArgumentsDestroy;  // avoid unused parameter warning

  // everything is good
  ier = false;
  return ier;
}


//==============================================================================
//
// Implementation of LennardJones612Implementation private member functions
//
//==============================================================================

//******************************************************************************
void LennardJones612Implementation::AllocatePrivateParameterMemory()
{
  // nothing to do for this case
}

//******************************************************************************
void LennardJones612Implementation::AllocateParameterMemory()
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
#include "KIM_ModelDriverCreateLogMacros.hpp"
int LennardJones612Implementation::OpenParameterFiles(
    KIM::ModelDriverCreate * const modelDriverCreate,
    int const numberParameterFiles,
    FILE* parameterFilePointers[MAX_PARAMETER_FILES])
{
  int ier;

  if (numberParameterFiles > MAX_PARAMETER_FILES)
  {
    ier = true;
    LOG_ERROR("LennardJones612 given too many parameter files");
    return ier;
  }

  for (int i = 0; i < numberParameterFiles; ++i)
  {
    std::string const * paramFileName;
    ier = modelDriverCreate->GetParameterFileName(
        i,
        &paramFileName);
    if (ier)
    {
      LOG_ERROR("Unable to get parameter file name");
      return ier;
    }
    parameterFilePointers[i] = fopen(paramFileName->c_str(), "r");
    if (parameterFilePointers[i] == 0)
    {
      char message[MAXLINE];
      sprintf(message,
              "LennardJones612 parameter file number %d cannot be opened",
              i);
      ier = true;
      LOG_ERROR(message);
      for (int j = i - 1; i <= 0; --i)
      {
        fclose(parameterFilePointers[j]);
      }
      return ier;
    }
  }

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
#include "KIM_ModelDriverCreateLogMacros.hpp"
int LennardJones612Implementation::ProcessParameterFiles(
    KIM::ModelDriverCreate * const modelDriverCreate,
    int const numberParameterFiles,
    FILE* const parameterFilePointers[MAX_PARAMETER_FILES])
{
  int N, ier;
  int endOfFileFlag = 0;
  char spec1[MAXLINE], spec2[MAXLINE], nextLine[MAXLINE];
  char *nextLinePtr;
  int iIndex, jIndex , indx, iiIndex, jjIndex;
  double nextCutoff, nextEpsilon, nextSigma;

  (void)numberParameterFiles;  // avoid unused parameter warning

  nextLinePtr = nextLine;

  getNextDataLine(parameterFilePointers[0], nextLinePtr,
                  MAXLINE, &endOfFileFlag);
  ier = sscanf(nextLine, "%d %d", &N, &shift_);
  if (ier != 2)
  {
    sprintf(nextLine, "unable to read first line of the parameter file");
    ier = true;
    LOG_ERROR(nextLine);
    fclose(parameterFilePointers[0]);
    return ier;
  }
  numberModelSpecies_ = N;
  numberUniqueSpeciesPairs_ = ((numberModelSpecies_+1)*numberModelSpecies_)/2;
  AllocateParameterMemory();

  // set all values in the arrays to -1 for mixing later
  for (int i = 0; i < ((N+1)*N/2); i++)
  {
    cutoffs_[i]  = -1;
    epsilons_[i] = -1;
    sigmas_[i] = -1;
  }


  // keep track of known species
  std::map<KIM::SpeciesName const, int, KIM::SPECIES_NAME::Comparator>
      modelSpeciesMap;
  std::vector<KIM::SpeciesName> speciesNameVector;
  int index = 0;

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
      LOG_ERROR(nextLine);
      return true;
    }

    // convert species strings to proper type instances
    KIM::SpeciesName const specName1(spec1);
    KIM::SpeciesName const specName2(spec2);

    // check for new species
    std::map<KIM::SpeciesName const, int, KIM::SPECIES_NAME::Comparator>::
        const_iterator iIter = modelSpeciesMap.find(specName1);
    if (iIter == modelSpeciesMap.end())
    {
      modelSpeciesMap[specName1] = index;
      modelSpeciesCodeList_.push_back(index);
      speciesNameVector.push_back(specName1);

      ier = modelDriverCreate->SetSpeciesCode(specName1, index);
      if (ier) return ier;
      iIndex = index;
      index++;
    }
    else
    {
      iIndex = modelSpeciesMap[specName1];
    }
    std::map<KIM::SpeciesName const, int, KIM::SPECIES_NAME::Comparator>::
        const_iterator jIter = modelSpeciesMap.find(specName2);
    if (jIter == modelSpeciesMap.end())
    {
      modelSpeciesMap[specName2] = index;
      modelSpeciesCodeList_.push_back(index);
      speciesNameVector.push_back(specName2);

      ier = modelDriverCreate->SetSpeciesCode(specName2, index);
      if (ier) return ier;
      jIndex = index;
      index++;
    }
    else
    {
      jIndex = modelSpeciesMap[specName2];
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
      strcat(nextLine, (speciesNameVector[i].String()).c_str());
      ier = -1;
    }
  }
  if (ier == -1)
  {
    LOG_ERROR(nextLine);
    return true;
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

  // everything is good
  ier = false;
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
    int const numberParameterFiles,
    FILE* const parameterFilePointers[MAX_PARAMETER_FILES])
{
  for (int i = 0; i < numberParameterFiles; ++i)
    fclose(parameterFilePointers[i]);
}

//******************************************************************************
#include "KIM_ModelDriverCreateLogMacros.hpp"
int LennardJones612Implementation::ConvertUnits(
    KIM::ModelDriverCreate * const modelDriverCreate,
    KIM::LengthUnit const requestedLengthUnit,
    KIM::EnergyUnit const requestedEnergyUnit,
    KIM::ChargeUnit const requestedChargeUnit,
    KIM::TemperatureUnit const requestedTemperatureUnit,
    KIM::TimeUnit const requestedTimeUnit)
{
  int ier;

  // define default base units
  KIM::LengthUnit fromLength = KIM::LENGTH_UNIT::A;
  KIM::EnergyUnit fromEnergy = KIM::ENERGY_UNIT::eV;
  KIM::ChargeUnit fromCharge = KIM::CHARGE_UNIT::e;
  KIM::TemperatureUnit fromTemperature = KIM::TEMPERATURE_UNIT::K;
  KIM::TimeUnit fromTime = KIM::TIME_UNIT::ps;

  // changing units of cutoffs and sigmas
  double convertLength = 1.0;
  ier = modelDriverCreate->ConvertUnit(
      fromLength, fromEnergy, fromCharge, fromTemperature, fromTime,
      requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
      requestedTemperatureUnit, requestedTimeUnit,
      1.0, 0.0, 0.0, 0.0, 0.0,
      &convertLength);
  if (ier)
  {
    LOG_ERROR("Unable to convert length unit");
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
  double convertEnergy = 1.0;
  ier = modelDriverCreate->ConvertUnit(
      fromLength, fromEnergy, fromCharge, fromTemperature, fromTime,
      requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
      requestedTemperatureUnit, requestedTimeUnit,
      0.0, 1.0, 0.0, 0.0, 0.0,
      &convertEnergy);
  if (ier)
  {
    LOG_ERROR("Unable to convert energy unit");
    return ier;
  }
  if (convertEnergy != ONE)
  {
    for (int i = 0; i < numberUniqueSpeciesPairs_; ++i)
    {
      epsilons_[i] *= convertEnergy;  // convert to active units
    }
  }

  // register units
  ier = modelDriverCreate->SetUnits(
      requestedLengthUnit,
      requestedEnergyUnit,
      KIM::CHARGE_UNIT::unused,
      KIM::TEMPERATURE_UNIT::unused,
      KIM::TIME_UNIT::unused);
  if (ier)
  {
    LOG_ERROR("Unable to set units to requested values");
    return ier;
  }

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::RegisterKIMModelSettings(
    KIM::ModelDriverCreate * const modelDriverCreate) const
{
  // register numbering
  int error = modelDriverCreate->SetModelNumbering(
      KIM::NUMBERING::zeroBased);

  return error;
}

//******************************************************************************
#include "KIM_ModelComputeArgumentsCreateLogMacros.hpp"
int LennardJones612Implementation::RegisterKIMComputeArgumentsSettings(
    KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate) const
{
  // register arguments
  LOG_INFORMATION("Register argument supportStatus");
  int error =
      modelComputeArgumentsCreate->SetArgumentSupportStatus(
          KIM::COMPUTE_ARGUMENT_NAME::partialEnergy,
          KIM::SUPPORT_STATUS::optional)
      || modelComputeArgumentsCreate->SetArgumentSupportStatus(
          KIM::COMPUTE_ARGUMENT_NAME::partialForces,
          KIM::SUPPORT_STATUS::optional)
      || modelComputeArgumentsCreate->SetArgumentSupportStatus(
          KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy,
          KIM::SUPPORT_STATUS::optional)
      || modelComputeArgumentsCreate->SetArgumentSupportStatus(
          KIM::COMPUTE_ARGUMENT_NAME::partialVirial,
          KIM::SUPPORT_STATUS::optional)
      || modelComputeArgumentsCreate->SetArgumentSupportStatus(
          KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial,
          KIM::SUPPORT_STATUS::optional);


  // register callbacks
  LOG_INFORMATION("Register callback supportStatus");
  error = error
      || modelComputeArgumentsCreate->SetCallbackSupportStatus(
          KIM::COMPUTE_CALLBACK_NAME::ProcessDEDrTerm,
          KIM::SUPPORT_STATUS::optional)
      || modelComputeArgumentsCreate->SetCallbackSupportStatus(
          KIM::COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term,
          KIM::SUPPORT_STATUS::optional);

  return error;
}

//******************************************************************************
#include "KIM_ModelDriverCreateLogMacros.hpp"
int LennardJones612Implementation::RegisterKIMParameters(
    KIM::ModelDriverCreate * const modelDriverCreate)
{
  int ier = false;

  // publish parameters (order is important)
  ier = modelDriverCreate->SetParameterPointer(1, &shift_, "shift");
  if (ier)
  {
    LOG_ERROR("set_parameter shift");
    return ier;
  }
  ier = modelDriverCreate->SetParameterPointer(
      numberUniqueSpeciesPairs_, cutoffs_, "cutoffs");
  if (ier)
  {
    LOG_ERROR("set_parameter cutoffs");
    return ier;
  }
  ier = modelDriverCreate->SetParameterPointer(
      numberUniqueSpeciesPairs_, epsilons_, "epsilons");
  if (ier)
  {
    LOG_ERROR("set_parameter epsilons");
    return ier;
  }
  ier = modelDriverCreate->SetParameterPointer(
      numberUniqueSpeciesPairs_, sigmas_, "sigmas");
  if (ier)
  {
    LOG_ERROR("set_parameter sigmas");
    return ier;
  }

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::RegisterKIMFunctions(
    KIM::ModelDriverCreate * const modelDriverCreate)
    const
{
  int error;

  // register the destroy() and reinit() functions
  error = modelDriverCreate->SetDestroyPointer(
      KIM::LANGUAGE_NAME::cpp, (KIM::func*) &(LennardJones612::Destroy))
      || modelDriverCreate->SetRefreshPointer(
          KIM::LANGUAGE_NAME::cpp, (KIM::func*) &(LennardJones612::Refresh))
      || modelDriverCreate->SetComputePointer(
          KIM::LANGUAGE_NAME::cpp, (KIM::func*) &(LennardJones612::Compute))
      || modelDriverCreate->SetComputeArgumentsCreatePointer(
          KIM::LANGUAGE_NAME::cpp,
          (KIM::func*) &(LennardJones612::ComputeArgumentsCreate))
      || modelDriverCreate->SetComputeArgumentsDestroyPointer(
          KIM::LANGUAGE_NAME::cpp,
          (KIM::func*) &(LennardJones612::ComputeArgumentsDestroy));

  return error;
}

//******************************************************************************
template<class ModelObj>
int LennardJones612Implementation::SetRefreshMutableValues(
    ModelObj * const modelObj)
{ // use (possibly) new values of parameters to compute other quantities
  // NOTE: This function is templated because it's called with both a
  //       modelDriverCreate object during initialization and with a
  //       modelRefresh object when the Model's parameters have been altered
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

  // update cutoff value in KIM API object
  influenceDistance_ = 0.0;

  for (int i = 0; i < numberModelSpecies_; i++)
  {
    int indexI = modelSpeciesCodeList_[i];

    for (int j = 0; j < numberModelSpecies_; j++)
    {
      int indexJ = modelSpeciesCodeList_[j];

      if (influenceDistance_ < cutoffsSq2D_[indexI][indexJ])
      {
        influenceDistance_ = cutoffsSq2D_[indexI][indexJ];
      }
    }
  }

  influenceDistance_ = sqrt(influenceDistance_);
  modelObj->SetInfluenceDistancePointer(&influenceDistance_);
  modelObj->SetNeighborListPointers(
      1,
      &influenceDistance_,
      &modelWillNotRequestNeighborsOfNoncontributingParticles_);

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
  ier = false;
  return ier;
}

//******************************************************************************
#include "KIM_ModelComputeArgumentsLogMacros.hpp"
int LennardJones612Implementation::SetComputeMutableValues(
    KIM::ModelComputeArguments const * const modelComputeArguments,
    bool& isComputeProcess_dEdr,
    bool& isComputeProcess_d2Edr2,
    bool& isComputeEnergy,
    bool& isComputeForces,
    bool& isComputeParticleEnergy,
    bool& isComputeVirial,
    bool& isComputeParticleVirial,
    int const*& particleSpeciesCodes,
    int const*& particleContributing,
    VectorOfSizeDIM const*& coordinates,
    double*& energy,
    double*& particleEnergy,
    VectorOfSizeDIM*& forces,
    VectorOfSizeSix*& virial,
    VectorOfSizeSix*& particleVirial)
{
  int ier = true;

  // get compute flags
  int compProcess_dEdr;
  int compProcess_d2Edr2;

  modelComputeArguments->IsCallbackPresent(
      KIM::COMPUTE_CALLBACK_NAME::ProcessDEDrTerm,
      &compProcess_dEdr);
  modelComputeArguments->IsCallbackPresent(
      KIM::COMPUTE_CALLBACK_NAME::ProcessD2EDr2Term,
      &compProcess_d2Edr2);

  isComputeProcess_dEdr = compProcess_dEdr;
  isComputeProcess_d2Edr2 = compProcess_d2Edr2;

  int const* numberOfParticles;
  ier =
      modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::numberOfParticles,
          &numberOfParticles)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::particleSpeciesCodes,
          &particleSpeciesCodes)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::particleContributing,
          &particleContributing)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::coordinates,
          (double const **) &coordinates)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialEnergy,
          &energy)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialParticleEnergy,
          &particleEnergy)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialForces,
          (double const **) &forces)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialVirial,
          (double const **) &virial)
      || modelComputeArguments->GetArgumentPointer(
          KIM::COMPUTE_ARGUMENT_NAME::partialParticleVirial,
          (double const **) &particleVirial);
  if (ier)
  {
    LOG_ERROR("GetArgumentPointer");
    return ier;
  }

  isComputeEnergy = (energy != NULL);
  isComputeParticleEnergy = (particleEnergy != NULL);
  isComputeForces = (forces != NULL);
  isComputeVirial = (virial != NULL);
  isComputeParticleVirial = (particleVirial != NULL);

  // update values
  cachedNumberOfParticles_ = *numberOfParticles;

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
#include "KIM_ModelComputeLogMacros.hpp"
int LennardJones612Implementation::CheckParticleSpeciesCodes(
    KIM::ModelCompute const * const modelCompute,
    int const* const particleSpeciesCodes)
    const
{
  int ier;
  for (int i = 0; i < cachedNumberOfParticles_; ++i)
  {
    if ((particleSpeciesCodes[i] < 0) ||
        (particleSpeciesCodes[i] >= numberModelSpecies_))
    {
      ier = true;
      LOG_ERROR("unsupported particle species codes detected");
      return ier;
    }
  }

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::GetComputeIndex(
    const bool& isComputeProcess_dEdr,
    const bool& isComputeProcess_d2Edr2,
    const bool& isComputeEnergy,
    const bool& isComputeForces,
    const bool& isComputeParticleEnergy,
    const bool& isComputeVirial,
    const bool& isComputeParticleVirial,
    const bool& isShift) const
{
  //const int processdE = 2;
  const int processd2E = 2;
  const int energy = 2;
  const int force = 2;
  const int particleEnergy = 2;
  const int virial = 2;
  const int particleVirial = 2;
  const int shift = 2;


  int index = 0;

  // processdE
  index += (int(isComputeProcess_dEdr))
      * processd2E * energy * force * particleEnergy * virial
      * particleVirial* shift;

  // processd2E
  index += (int(isComputeProcess_d2Edr2))
      * energy * force * particleEnergy * virial * particleVirial * shift;

  // energy
  index += (int(isComputeEnergy))
      * force * particleEnergy * virial * particleVirial * shift;

  // force
  index += (int(isComputeForces))
      * particleEnergy * virial * particleVirial * shift;

  // particleEnergy
  index += (int(isComputeParticleEnergy))
      * virial * particleVirial * shift;

  // virial
  index += (int(isComputeVirial))
      * particleVirial * shift;

  // particleVirial
  index += (int(isComputeParticleVirial))
      * shift;

  // shift
  index += (int(isShift));

  return index;
}

//******************************************************************************
void LennardJones612Implementation::ProcessVirialTerm(
    const double& dEidr,
    const double& rij,
    const double* const r_ij,
    const int& i,
    const int& j,
    VectorOfSizeSix virial) const
{
  (void)i;  // avoid unused parameter warning
  (void)j;

  double const v = dEidr/rij;

  virial[0] += v * r_ij[0] * r_ij[0];
  virial[1] += v * r_ij[1] * r_ij[1];
  virial[2] += v * r_ij[2] * r_ij[2];
  virial[3] += v * r_ij[1] * r_ij[2];
  virial[4] += v * r_ij[0] * r_ij[2];
  virial[5] += v * r_ij[0] * r_ij[1];
}

//******************************************************************************
void LennardJones612Implementation::ProcessParticleVirialTerm(
    const double& dEidr,
    const double& rij,
    const double* const r_ij,
    const int& i, const int& iContrib,
    const int& j, const int& jContrib,
    VectorOfSizeSix* const particleVirial) const
{
  (void)iContrib;  // avoid unused parameter warning

  double const v = dEidr/rij;
  VectorOfSizeSix vir;

  // if iContrib && jContrib then v is dphi/dr
  // otherwise v is 0.5*dphi/dr

  // assumes each ij pair is processed only once.
  vir[0] = v * r_ij[0] * r_ij[0];
  vir[1] = v * r_ij[1] * r_ij[1];
  vir[2] = v * r_ij[2] * r_ij[2];
  vir[3] = v * r_ij[1] * r_ij[2];
  vir[4] = v * r_ij[0] * r_ij[2];
  vir[5] = v * r_ij[0] * r_ij[1];

  for (int k = 0; k < 6; ++k)
  {
    // assuming iContrib == 1
    if (jContrib)
    {
      particleVirial[i][k] += 0.5*vir[k];
      particleVirial[j][k] += 0.5*vir[k];
    }
    else
      particleVirial[i][k] += vir[k];
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
  if (arrayPtr != NULL) delete [] arrayPtr[0];
  delete [] arrayPtr;

  // nullify pointer
  arrayPtr = NULL;
}
