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
#include "KIM_SpeciesName.hpp"
#include "KIM_UnitSystem.hpp"
#include "KIM_COMPUTE_ArgumentName.hpp"

#define MAXLINE 1024
#define IGNORE_RESULT(fn) if(fn){}


//==============================================================================
//
// Implementation of LennardJones612Implementation public member functions
//
//==============================================================================

// Helper routine declaration
namespace
{
char const * const speciesString(KIM::SpeciesName const speciesName);
}

//******************************************************************************
LennardJones612Implementation::LennardJones612Implementation(
    KIM::Simulator * const simulator,
    char const * const  parameterFileNames,
    int const parameterFileNameLength,
    int const numberParameterFiles,
    int * const ier)
    : numberModelSpecies_(0),
      numberUniqueSpeciesPairs_(0),
      shift_(0),
      cutoffs_(0),
      epsilons_(0),
      sigmas_(0),
      influenceDistance_(0.0),
      cutoffsSq2D_(0),
      fourEpsilonSigma6_2D_(0),
      fourEpsilonSigma12_2D_(0),
      twentyFourEpsilonSigma6_2D_(0),
      fortyEightEpsilonSigma12_2D_(0),
      oneSixtyEightEpsilonSigma6_2D_(0),
      sixTwentyFourEpsilonSigma12_2D_(0),
      shifts2D_(0),
      cachedNumberOfParticles_(0)
{
  *ier = SetConstantValues(simulator);
  if (*ier) return;

  AllocateFreeParameterMemory();

  FILE* parameterFilePointers[MAX_PARAMETER_FILES];
  *ier = OpenParameterFiles(parameterFileNames, parameterFileNameLength,
                            numberParameterFiles, parameterFilePointers);
  if (*ier) return;

  *ier = ProcessParameterFiles(simulator, parameterFilePointers,
                               numberParameterFiles);
  CloseParameterFiles(parameterFilePointers, numberParameterFiles);
  if (*ier) return;

  *ier = ConvertUnits(simulator);
  if (*ier) return;

  *ier = SetReinitMutableValues(simulator);
  if (*ier) return;

  *ier = RegisterKIMParameters(simulator);
  if (*ier) return;

  *ier = RegisterKIMFunctions(simulator);
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
int LennardJones612Implementation::Reinit(KIM::Simulator * const simulator)
{
  int ier;

  ier = SetReinitMutableValues(simulator);
  if (ier) return ier;

  // nothing else to do for this case

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::Compute(
    KIM::Simulator const * const simulator,
    KIM::COMPUTE::SimulatorComputeArguments const * const arguments)
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
  //
  // KIM API Model Input
  int const* particleSpecies = 0;
  int const* particleContributing = 0;
  VectorOfSizeDIM const* coordinates = 0;
  //
  // KIM API Model Output
  double* energy = 0;
  double* particleEnergy = 0;
  VectorOfSizeDIM* forces = 0;
  ier = SetComputeMutableValues(simulator, arguments, isComputeProcess_dEdr,
                                isComputeProcess_d2Edr2, isComputeEnergy,
                                isComputeForces, isComputeParticleEnergy,
                                particleSpecies, particleContributing,
                                coordinates, energy, particleEnergy, forces);
  if (ier) return ier;

  // Skip this check for efficiency
  //
  // ier = CheckParticleSpecies(pkim, particleSpecies);
  // if (ier) return ier;

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
int LennardJones612Implementation::
SetConstantValues(KIM::Simulator const * const simulator)
{
  // set numberModelSpecies & numberUniqueSpeciesPairs
  simulator->get_num_model_species(&numberModelSpecies_);
  numberUniqueSpeciesPairs_ = ((numberModelSpecies_+1)*numberModelSpecies_)/2;

  // everything is good
  return false;
}

//******************************************************************************
int LennardJones612Implementation::OpenParameterFiles(
    char const* const parameterFileNames,
    int const parameterFileNameLength,
    int const numberParameterFiles,
    FILE* parameterFilePointers[MAX_PARAMETER_FILES])
{
  int ier;

  if (numberParameterFiles > MAX_PARAMETER_FILES)
  {
    ier = true;
    KIM::report_error(__LINE__, __FILE__, "LennardJones612 given too many"
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
      ier = true;
      KIM::report_error(__LINE__, __FILE__, message, ier);
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
int LennardJones612Implementation::ProcessParameterFiles(
    KIM::Simulator const * const simulator,
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
    ier = true;
    KIM::report_error(__LINE__, __FILE__, nextLine, ier);
    fclose(parameterFilePointers[0]);
    return ier;
  }
  if (N != numberModelSpecies_)
  {
    sprintf(nextLine, "The value for N from the parameter file is inconsistent "
            "with numberModelSpecies_");
    ier = true;
    KIM::report_error(__LINE__, __FILE__, nextLine, ier);
    fclose(parameterFilePointers[0]);
    return ier;
  }

  // get and correctly order the particle names
  const char** const particleNames = new const char*[numberModelSpecies_];
  for (int i = 0; i < numberModelSpecies_; ++i)
  {
    KIM::SpeciesName kimModelParticleSpecies;
    ier = simulator->get_model_species(i, &kimModelParticleSpecies);
    if (ier)
    {
      KIM::report_error(__LINE__, __FILE__, "get_model_species", ier);
      delete [] particleNames;
      return ier;
    }
    int index;
    ier = simulator->get_species_code(kimModelParticleSpecies, &index);
    if (index >= numberModelSpecies_)
    {
      KIM::report_error(__LINE__, __FILE__, "get_species_code",
                        true);
      delete [] particleNames;
      return true;
    }
    particleNames[index] = speciesString(kimModelParticleSpecies);
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
      KIM::report_error(__LINE__, __FILE__, nextLine, true);
      delete [] particleNames;
      return true;
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
      KIM::report_error(__LINE__, __FILE__, nextLine, true);
      delete [] particleNames;
      return true;
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
    KIM::report_error(__LINE__, __FILE__, nextLine, true);
    delete [] particleNames;
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
  delete [] particleNames;

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
int LennardJones612Implementation::ConvertUnits(
    KIM::Simulator const * const simulator)
{
  int ier;

  // define default base units
  KIM::LengthUnit length = KIM::UNITS::A;
  KIM::EnergyUnit energy = KIM::UNITS::eV;
  KIM::ChargeUnit charge = KIM::UNITS::e;
  KIM::TemperatureUnit temperature = KIM::UNITS::K;
  KIM::TimeUnit time = KIM::UNITS::ps;

  // changing units of cutoffs and sigmas
  double convertLength;
  ier = simulator->convert_to_act_unit(length, energy, charge, temperature, time,
                                       1.0, 0.0, 0.0, 0.0, 0.0, &convertLength);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "convert_to_act_unit", ier);
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
  double convertEnergy;
  ier = simulator->convert_to_act_unit(length, energy, charge, temperature, time,
                                       0.0, 1.0, 0.0, 0.0, 0.0, &convertEnergy);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "convert_to_act_unit", ier);
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
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::RegisterKIMParameters(
    KIM::Simulator * const simulator) const
{
  int ier = false;

  // publish parameters
  ier = simulator->set_parameter(PARAM_SHIFT_INDEX, 1, (void *) &shift_);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "set_parameter", ier);
    return ier;
  }
  ier = simulator->set_parameter(PARAM_CUTOFFS_INDEX,
                                 numberUniqueSpeciesPairs_,
                                 (void *) cutoffs_);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "set_parameter", ier);
    return ier;
  }
  ier = simulator->set_parameter(PARAM_EPSILONS_INDEX,
                                 numberUniqueSpeciesPairs_,
                                 (void *) epsilons_);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "set_parameter", ier);
    return ier;
  }
  ier = simulator->set_parameter(PARAM_SIGMAS_INDEX,
                                 numberUniqueSpeciesPairs_,
                                 (void *) sigmas_);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "set_parameter", ier);
    return ier;
  }

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::RegisterKIMFunctions(
    KIM::Simulator * const simulator)
    const
{
  int ier;

  // register the destroy() and reinit() functions
  simulator->set_destroy(KIM::LANGUAGE_NAME::Cpp,
                         (KIM::func*) &(LennardJones612::Destroy));
  simulator->set_reinit(KIM::LANGUAGE_NAME::Cpp,
                        (KIM::func*) &(LennardJones612::Reinit));
  simulator->set_compute_func(KIM::LANGUAGE_NAME::Cpp,
                              (KIM::func*) &(LennardJones612::Compute));

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::SetReinitMutableValues(
    KIM::Simulator * const simulator)
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

  // update cutoff value in KIM API object
  influenceDistance_ = 0.0;
  int numberSpecies;
  simulator->get_num_sim_species(&numberSpecies);
  KIM::SpeciesName simSpeciesI;
  KIM::SpeciesName simSpeciesJ;
  for (int i = 0; i < numberSpecies; i++)
  {
    ier = simulator->get_sim_species(i, &simSpeciesI);
    if (ier)
    {
      KIM::report_error(__LINE__, __FILE__, "get_num_sim_species", ier);
      return ier;
    }
    int indexI;
    ier = simulator->get_species_code(simSpeciesI, &indexI);
    if (indexI >= numberModelSpecies_ || ier )
    {
      KIM::report_error(__LINE__, __FILE__, "get_species_code",
                        true);
      return true;
    }

    for (int j = 0; j < numberSpecies; j++)
    {
      ier = simulator->get_sim_species(i, &simSpeciesJ);
      if (ier)
      {
        KIM::report_error(__LINE__, __FILE__, "get_num_sim_species", ier);
        return ier;
      }
      int indexJ;
      ier = simulator->get_species_code(simSpeciesJ, &indexJ);
      if (indexJ >= numberModelSpecies_ || ier )
      {
        KIM::report_error(__LINE__, __FILE__, "get_species_code",
                          true);
        return true;
      }
      if (influenceDistance_ < cutoffsSq2D_[indexI][indexJ])
      {
        influenceDistance_ = cutoffsSq2D_[indexI][indexJ];
      }
    }
  }
  influenceDistance_ = sqrt(influenceDistance_);
  simulator->set_influence_distance(&influenceDistance_);
  simulator->set_cutoffs(1, &influenceDistance_);

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
int LennardJones612Implementation::SetComputeMutableValues(
    KIM::Simulator const * const simulator,
    KIM::COMPUTE::SimulatorComputeArguments const * const arguments,
    bool& isComputeProcess_dEdr,
    bool& isComputeProcess_d2Edr2,
    bool& isComputeEnergy,
    bool& isComputeForces,
    bool& isComputeParticleEnergy,
    int const*& particleSpecies,
    int const*& particleContributing,
    VectorOfSizeDIM const*& coordinates,
    double*& energy,
    double*& particleEnergy,
    VectorOfSizeDIM*& forces)
{
  int ier = true;

  // get compute flags
  int compEnergy;
  int compForces;
  int compParticleEnergy;
  int compProcess_dEdr;
  int compProcess_d2Edr2;

  arguments->get_process_dEdr_compute(&compProcess_dEdr);
  arguments->get_process_d2Edr2_compute(&compProcess_d2Edr2);

  ier =
      arguments->get_compute(KIM::COMPUTE::ARGUMENT_NAME::energy, &compEnergy)
      || arguments->get_compute(KIM::COMPUTE::ARGUMENT_NAME::forces,
                                &compForces)
      || arguments->get_compute(KIM::COMPUTE::ARGUMENT_NAME::particleEnergy,
                                &compParticleEnergy);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "get_compute", ier);
    return ier;
  }

  isComputeEnergy = compEnergy;
  isComputeForces = compForces;
  isComputeParticleEnergy = compParticleEnergy;
  isComputeProcess_dEdr = compProcess_dEdr;
  isComputeProcess_d2Edr2 = compProcess_d2Edr2;

  // extract pointers based on compute flags
  //
  // double const* cutoff;            // currently unused
  // int const* numberOfSpecies;  // currently unused
  int const* numberOfParticles;
  ier =
      arguments->get_data(KIM::COMPUTE::ARGUMENT_NAME::numberOfParticles,
                          &numberOfParticles)
      || arguments->get_data(KIM::COMPUTE::ARGUMENT_NAME::particleSpecies,
                             &particleSpecies)
      || arguments->get_data(KIM::COMPUTE::ARGUMENT_NAME::particleContributing,
                              &particleContributing)
      || arguments->get_data(KIM::COMPUTE::ARGUMENT_NAME::coordinates,
                             (double const ** const) &coordinates)
      || (compEnergy ? arguments->get_data(KIM::COMPUTE::ARGUMENT_NAME::energy,
                                           &energy) : false)
      || (compParticleEnergy ? arguments->get_data(
          KIM::COMPUTE::ARGUMENT_NAME::particleEnergy, &particleEnergy) : false)
      || (compForces ? arguments->get_data(
          KIM::COMPUTE::ARGUMENT_NAME::forces,
          (double const ** const) &forces) : false);
  if (ier)
  {
    KIM::report_error(__LINE__, __FILE__, "get_data", ier);
    return ier;
  }

  // update values
  cachedNumberOfParticles_ = *numberOfParticles;

  // everything is good
  ier = false;
  return ier;
}

//******************************************************************************
int LennardJones612Implementation::CheckParticleSpecies(
    int const* const particleSpecies)
    const
{
  int ier;
  for (int i = 0; i < cachedNumberOfParticles_; ++i)
  {
    if ((particleSpecies[i] < 0) || (particleSpecies[i] >= numberModelSpecies_))
    {
      ier = true;
      KIM::report_error(__LINE__, __FILE__,
                        "unsupported particle species detected", ier);
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
    const bool& isShift) const
{
  //const int processdE = 2;
  const int processd2E = 2;
  const int energy = 2;
  const int force = 2;
  const int particleEnergy = 2;
  const int shift = 2;


  int index = 0;

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

namespace
{
char const * const speciesString(KIM::SpeciesName const speciesName)
{
  if (speciesName == KIM::SPECIES_NAME::electron) return "electron";
  else if (speciesName == KIM::SPECIES_NAME::H) return "H";
  else if (speciesName == KIM::SPECIES_NAME::He) return "He";
  else if (speciesName == KIM::SPECIES_NAME::Li) return "Li";
  else if (speciesName == KIM::SPECIES_NAME::Be) return "Be";
  else if (speciesName == KIM::SPECIES_NAME::B) return "B";
  else if (speciesName == KIM::SPECIES_NAME::C) return "C";
  else if (speciesName == KIM::SPECIES_NAME::N) return "N";
  else if (speciesName == KIM::SPECIES_NAME::O) return "O";
  else if (speciesName == KIM::SPECIES_NAME::F) return "F";
  else if (speciesName == KIM::SPECIES_NAME::Ne) return "Ne";
  else if (speciesName == KIM::SPECIES_NAME::Na) return "Na";
  else if (speciesName == KIM::SPECIES_NAME::Mg) return "Mg";
  else if (speciesName == KIM::SPECIES_NAME::Al) return "Al";
  else if (speciesName == KIM::SPECIES_NAME::Si) return "Si";
  else if (speciesName == KIM::SPECIES_NAME::P) return "P";
  else if (speciesName == KIM::SPECIES_NAME::S) return "S";
  else if (speciesName == KIM::SPECIES_NAME::Cl) return "Cl";
  else if (speciesName == KIM::SPECIES_NAME::Ar) return "Ar";
  else if (speciesName == KIM::SPECIES_NAME::K) return "K";
  else if (speciesName == KIM::SPECIES_NAME::Ca) return "Ca";
  else if (speciesName == KIM::SPECIES_NAME::Sc) return "Sc";
  else if (speciesName == KIM::SPECIES_NAME::Ti) return "Ti";
  else if (speciesName == KIM::SPECIES_NAME::V) return "V";
  else if (speciesName == KIM::SPECIES_NAME::Cr) return "Cr";
  else if (speciesName == KIM::SPECIES_NAME::Mn) return "Mn";
  else if (speciesName == KIM::SPECIES_NAME::Fe) return "Fe";
  else if (speciesName == KIM::SPECIES_NAME::Co) return "Co";
  else if (speciesName == KIM::SPECIES_NAME::Ni) return "Ni";
  else if (speciesName == KIM::SPECIES_NAME::Cu) return "Cu";
  else if (speciesName == KIM::SPECIES_NAME::Zn) return "Zn";
  else if (speciesName == KIM::SPECIES_NAME::Ga) return "Ga";
  else if (speciesName == KIM::SPECIES_NAME::Ge) return "Ge";
  else if (speciesName == KIM::SPECIES_NAME::As) return "As";
  else if (speciesName == KIM::SPECIES_NAME::Se) return "Se";
  else if (speciesName == KIM::SPECIES_NAME::Br) return "Br";
  else if (speciesName == KIM::SPECIES_NAME::Kr) return "Kr";
  else if (speciesName == KIM::SPECIES_NAME::Rb) return "Rb";
  else if (speciesName == KIM::SPECIES_NAME::Sr) return "Sr";
  else if (speciesName == KIM::SPECIES_NAME::Y) return "Y";
  else if (speciesName == KIM::SPECIES_NAME::Zr) return "Zr";
  else if (speciesName == KIM::SPECIES_NAME::Nb) return "Nb";
  else if (speciesName == KIM::SPECIES_NAME::Mo) return "Mo";
  else if (speciesName == KIM::SPECIES_NAME::Tc) return "Tc";
  else if (speciesName == KIM::SPECIES_NAME::Ru) return "Ru";
  else if (speciesName == KIM::SPECIES_NAME::Rh) return "Rh";
  else if (speciesName == KIM::SPECIES_NAME::Pd) return "Pd";
  else if (speciesName == KIM::SPECIES_NAME::Ag) return "Ag";
  else if (speciesName == KIM::SPECIES_NAME::Cd) return "Cd";
  else if (speciesName == KIM::SPECIES_NAME::In) return "In";
  else if (speciesName == KIM::SPECIES_NAME::Sn) return "Sn";
  else if (speciesName == KIM::SPECIES_NAME::Sb) return "Sb";
  else if (speciesName == KIM::SPECIES_NAME::Te) return "Te";
  else if (speciesName == KIM::SPECIES_NAME::I) return "I";
  else if (speciesName == KIM::SPECIES_NAME::Xe) return "Xe";
  else if (speciesName == KIM::SPECIES_NAME::Cs) return "Cs";
  else if (speciesName == KIM::SPECIES_NAME::Ba) return "Ba";
  else if (speciesName == KIM::SPECIES_NAME::La) return "La";
  else if (speciesName == KIM::SPECIES_NAME::Ce) return "Ce";
  else if (speciesName == KIM::SPECIES_NAME::Pr) return "Pr";
  else if (speciesName == KIM::SPECIES_NAME::Nd) return "Nd";
  else if (speciesName == KIM::SPECIES_NAME::Pm) return "Pm";
  else if (speciesName == KIM::SPECIES_NAME::Sm) return "Sm";
  else if (speciesName == KIM::SPECIES_NAME::Eu) return "Eu";
  else if (speciesName == KIM::SPECIES_NAME::Gd) return "Gd";
  else if (speciesName == KIM::SPECIES_NAME::Tb) return "Tb";
  else if (speciesName == KIM::SPECIES_NAME::Dy) return "Dy";
  else if (speciesName == KIM::SPECIES_NAME::Ho) return "Ho";
  else if (speciesName == KIM::SPECIES_NAME::Er) return "Er";
  else if (speciesName == KIM::SPECIES_NAME::Tm) return "Tm";
  else if (speciesName == KIM::SPECIES_NAME::Yb) return "Yb";
  else if (speciesName == KIM::SPECIES_NAME::Lu) return "Lu";
  else if (speciesName == KIM::SPECIES_NAME::Hf) return "Hf";
  else if (speciesName == KIM::SPECIES_NAME::Ta) return "Ta";
  else if (speciesName == KIM::SPECIES_NAME::W) return "W";
  else if (speciesName == KIM::SPECIES_NAME::Re) return "Re";
  else if (speciesName == KIM::SPECIES_NAME::Os) return "Os";
  else if (speciesName == KIM::SPECIES_NAME::Ir) return "Ir";
  else if (speciesName == KIM::SPECIES_NAME::Pt) return "Pt";
  else if (speciesName == KIM::SPECIES_NAME::Au) return "Au";
  else if (speciesName == KIM::SPECIES_NAME::Hg) return "Hg";
  else if (speciesName == KIM::SPECIES_NAME::Tl) return "Tl";
  else if (speciesName == KIM::SPECIES_NAME::Pb) return "Pb";
  else if (speciesName == KIM::SPECIES_NAME::Bi) return "Bi";
  else if (speciesName == KIM::SPECIES_NAME::Po) return "Po";
  else if (speciesName == KIM::SPECIES_NAME::At) return "At";
  else if (speciesName == KIM::SPECIES_NAME::Rn) return "Rn";
  else if (speciesName == KIM::SPECIES_NAME::Fr) return "Fr";
  else if (speciesName == KIM::SPECIES_NAME::Ra) return "Ra";
  else if (speciesName == KIM::SPECIES_NAME::Ac) return "Ac";
  else if (speciesName == KIM::SPECIES_NAME::Th) return "Th";
  else if (speciesName == KIM::SPECIES_NAME::Pa) return "Pa";
  else if (speciesName == KIM::SPECIES_NAME::U) return "U";
  else if (speciesName == KIM::SPECIES_NAME::Np) return "Np";
  else if (speciesName == KIM::SPECIES_NAME::Pu) return "Pu";
  else if (speciesName == KIM::SPECIES_NAME::Am) return "Am";
  else if (speciesName == KIM::SPECIES_NAME::Cm) return "Cm";
  else if (speciesName == KIM::SPECIES_NAME::Bk) return "Bk";
  else if (speciesName == KIM::SPECIES_NAME::Cf) return "Cf";
  else if (speciesName == KIM::SPECIES_NAME::Es) return "Es";
  else if (speciesName == KIM::SPECIES_NAME::Fm) return "Fm";
  else if (speciesName == KIM::SPECIES_NAME::Md) return "Md";
  else if (speciesName == KIM::SPECIES_NAME::No) return "No";
  else if (speciesName == KIM::SPECIES_NAME::Lr) return "Lr";
  else if (speciesName == KIM::SPECIES_NAME::Rf) return "Rf";
  else if (speciesName == KIM::SPECIES_NAME::Db) return "Db";
  else if (speciesName == KIM::SPECIES_NAME::Sg) return "Sg";
  else if (speciesName == KIM::SPECIES_NAME::Bh) return "Bh";
  else if (speciesName == KIM::SPECIES_NAME::Hs) return "Hs";
  else if (speciesName == KIM::SPECIES_NAME::Mt) return "Mt";
  else if (speciesName == KIM::SPECIES_NAME::Ds) return "Ds";
  else if (speciesName == KIM::SPECIES_NAME::Rg) return "Rg";
  else if (speciesName == KIM::SPECIES_NAME::Cn) return "Cn";
  else if (speciesName == KIM::SPECIES_NAME::Uut) return "Uut";
  else if (speciesName == KIM::SPECIES_NAME::Fl) return "Fl";
  else if (speciesName == KIM::SPECIES_NAME::Uup) return "Uup";
  else if (speciesName == KIM::SPECIES_NAME::Lv) return "Lv";
  else if (speciesName == KIM::SPECIES_NAME::Uus) return "Uus";
  else if (speciesName == KIM::SPECIES_NAME::Uuo) return "Uuo";
  else if (speciesName == KIM::SPECIES_NAME::user01) return "user01";
  else if (speciesName == KIM::SPECIES_NAME::user02) return "user02";
  else if (speciesName == KIM::SPECIES_NAME::user03) return "user03";
  else if (speciesName == KIM::SPECIES_NAME::user04) return "user04";
  else if (speciesName == KIM::SPECIES_NAME::user05) return "user05";
  else if (speciesName == KIM::SPECIES_NAME::user06) return "user06";
  else if (speciesName == KIM::SPECIES_NAME::user07) return "user07";
  else if (speciesName == KIM::SPECIES_NAME::user08) return "user08";
  else if (speciesName == KIM::SPECIES_NAME::user09) return "user09";
  else if (speciesName == KIM::SPECIES_NAME::user10) return "user10";
  else if (speciesName == KIM::SPECIES_NAME::user11) return "user11";
  else if (speciesName == KIM::SPECIES_NAME::user12) return "user12";
  else if (speciesName == KIM::SPECIES_NAME::user13) return "user13";
  else if (speciesName == KIM::SPECIES_NAME::user14) return "user14";
  else if (speciesName == KIM::SPECIES_NAME::user15) return "user15";
  else if (speciesName == KIM::SPECIES_NAME::user16) return "user16";
  else if (speciesName == KIM::SPECIES_NAME::user17) return "user17";
  else if (speciesName == KIM::SPECIES_NAME::user18) return "user18";
  else if (speciesName == KIM::SPECIES_NAME::user19) return "user19";
  else if (speciesName == KIM::SPECIES_NAME::user20) return "user20";
  else return "";
}
}
