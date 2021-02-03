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
// Copyright (c) 2020--2021, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include "KIM_SimulatorHeaders.hpp"
#include "KIM_SupportedExtensions.hpp"
#include "KIM_Version.hpp"
#include "edn-cpp/edn.hpp"
#include <cstdio>
#include <cstdlib>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

void usage(std::string name)
{
  size_t beg = name.find_last_of("/\\");
  if (beg != std::string::npos) name = name.substr(beg + 1, std::string::npos);

  // Follows docopt.org format
  std::cerr << "Usage:\n"
            << "  " << name << " "
            << "<portable-model-name>\n"
            << "  " << name << " "
            << "--version\n";
  // note: this interface is likely to change in future kim-api releases
}


int main(int argc, char * argv[])
{
  KIM::Model * mdl;
  int requestedUnitsAccepted = 0;

  if ((argc != 2) || (std::string(argv[1]) == ""))
  {
    usage(argv[0]);
    return 1;
  }

  if ((argc == 2) && (std::string(argv[1]) == "--version"))
  {
    std::cout << KIM_VERSION_STRING << std::endl;
    return 0;
  }


  int error = KIM::Model::Create(KIM::NUMBERING::zeroBased,
                                 KIM::LENGTH_UNIT::m,
                                 KIM::ENERGY_UNIT::amu_A2_per_ps2,
                                 KIM::CHARGE_UNIT::statC,
                                 KIM::TEMPERATURE_UNIT::K,
                                 KIM::TIME_UNIT::ns,
                                 argv[1],
                                 &requestedUnitsAccepted,
                                 &mdl);

  if (error)
  {
    std::cerr << "Error creating model object" << std::endl;
    exit(1);
  }

  std::stringstream edn;
  edn << std::setprecision(16) << std::scientific;
  edn << "{"
      << "\"model-name\""
      << " \"" << argv[1] << "\" ";

  // Unit information
  edn << "\"units-request-accepted\""
      << " " << ((requestedUnitsAccepted) ? "true" : "false") << " "
      << "\"units-unused\""
      << " "
      << "[";
  KIM::LengthUnit length;
  KIM::EnergyUnit energy;
  KIM::ChargeUnit charge;
  KIM::TemperatureUnit temp;
  KIM::TimeUnit time;
  mdl->GetUnits(&length, &energy, &charge, &temp, &time);

  if (length == KIM::LENGTH_UNIT::unused)
    edn << "\"length\""
        << " ";
  if (energy == KIM::ENERGY_UNIT::unused)
    edn << "\"energy\""
        << " ";
  if (charge == KIM::CHARGE_UNIT::unused)
    edn << "\"charge\""
        << " ";
  if (temp == KIM::TEMPERATURE_UNIT::unused)
    edn << "\"temperature\""
        << " ";
  if (time == KIM::TIME_UNIT::unused)
    edn << "\"time\""
        << " ";

  edn << "]"
      << " ";

  // influence distance
  double influenceDistance = 0.0;
  mdl->GetInfluenceDistance(&influenceDistance);

  edn << "\"influence-distance\""
      << " "
      << "[ " << influenceDistance << " \"" << length.ToString() << "\" ]";

  // neighbor lists
  int numberOfNeighborLists = 0;
  double const * cutoffs = NULL;
  int const * nonContrib = NULL;
  mdl->GetNeighborListPointers(&numberOfNeighborLists, &cutoffs, &nonContrib);

  edn << "\"neighbor-lists\""
      << " "
      << "{ "
      << "\"number-of-lists\""
      << " " << numberOfNeighborLists << " "
      << "\"cutoffs\""
      << " [ ";
  for (int i = 0; i < numberOfNeighborLists; ++i) { edn << cutoffs[i] << " "; }
  edn << "] "
      << "\"model-will-not-request-neighbors-of-noncontributing-particles\""
      << " "
      << "[ ";
  for (int i = 0; i < numberOfNeighborLists; ++i)
  {
    edn << nonContrib[i] << " ";
  }
  edn << "] "
      << "} ";

  // Species support and code
  edn << "\"supported-species\""
      << " "
      << "[ ";
  int numberSpecies = 0;
  KIM::SPECIES_NAME::GetNumberOfSpeciesNames(&numberSpecies);
  for (int i = 0; i < numberSpecies; ++i)
  {
    KIM::SpeciesName species;
    KIM::SPECIES_NAME::GetSpeciesName(i, &species);
    int supported;
    int code;
    error = mdl->GetSpeciesSupportAndCode(species, &supported, &code);
    if (!error)
    {
      if (supported)
      {
        edn << "[ \"" << species.ToString() << "\" " << code << " ]"
            << " ";
      }
    }
  }
  edn << "] ";

  // Parameter information
  int numberOfParameters = 0;
  mdl->GetNumberOfParameters(&numberOfParameters);
  edn << "\"parameters\""
      << " "
      << "{ "
      << "\"number-of-parameters\""
      << " " << numberOfParameters << " "
      << "\"parameter-metadata\""
      << "[ ";
  for (int i = 0; i < numberOfParameters; ++i)
  {
    KIM::DataType dtype;
    int extent;
    std::string const * name;
    std::string const * description;
    error = mdl->GetParameterMetadata(i, &dtype, &extent, &name, &description);

    if (!error)
    {
      edn << "{ "
          << "\"data-type\""
          << " "
          << "\"" << dtype.ToString() << "\""
          << " "
          << "\"extent\""
          << " " << extent << " "
          << "\"name\""
          << " "
          << "\"" << *name << "\""
          << " "
          << "\"description\""
          << " "
          << "\"" << edn::escapeQuotes(*description) << "\""
          << " }"
          << " ";
    }
  }
  edn << "] "
      << "} ";

  // Present routines
  bool extensionPresent = false;
  int numberOfModelRoutineNames;
  KIM::MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames(
      &numberOfModelRoutineNames);

  edn << "\"routines-present\""
      << " "
      << "{ ";
  for (int i = 0; i < numberOfModelRoutineNames; ++i)
  {
    KIM::ModelRoutineName routineName;
    int present;
    int required;
    KIM::MODEL_ROUTINE_NAME::GetModelRoutineName(i, &routineName);
    error = mdl->IsRoutinePresent(routineName, &present, &required);

    if (!error)
    {
      if ((routineName == KIM::MODEL_ROUTINE_NAME::Extension) && present)
        extensionPresent = true;

      if (present)
        edn << "\"" << routineName.ToString() << "\""
            << " " << ((required) ? "required" : "optional") << " ";
    }
  }
  edn << "} ";

  // Extensions, if present
  edn << "\"extensions\""
      << " { ";

  if (extensionPresent)
  {
    KIM::SupportedExtensions extensionStruct;
    error = mdl->Extension(KIM_SUPPORTED_EXTENSIONS_ID, &extensionStruct);

    if (!error)
    {
      for (int i = 0; i < extensionStruct.numberOfSupportedExtensions; ++i)
      {
        edn << "\"" << extensionStruct.supportedExtensionID[i] << "\""
            << " "
            << ((extensionStruct.supportedExtensionRequired[i])
                    ? "\"required\""
                    : "\"optional\"")
            << " ";
      }
    }
  }
  edn << "} ";

  // Compute Arguments
  edn << "\"compute-arguments\""
      << " { ";

  KIM::ComputeArguments * computeArguments;
  error = mdl->ComputeArgumentsCreate(&computeArguments);

  if (!error)
  {
    KIM::ComputeArgumentName argumentName;
    int numberOfComputeArgumentNames = 0;
    KIM::COMPUTE_ARGUMENT_NAME::GetNumberOfComputeArgumentNames(
        &numberOfComputeArgumentNames);

    for (int i = 0; i < numberOfComputeArgumentNames; ++i)
    {
      KIM::COMPUTE_ARGUMENT_NAME::GetComputeArgumentName(i, &argumentName);
      KIM::SupportStatus supportStatus;
      error = computeArguments->GetArgumentSupportStatus(argumentName,
                                                         &supportStatus);
      if (!error)
      {
        edn << "\"" << argumentName.ToString() << "\""
            << " "
            << "\"" << supportStatus.ToString() << "\""
            << " ";
      }
    }
  }
  edn << "} ";

  // Compute Callbacks
  edn << "\"compute-callbacks\""
      << " { ";

  if (computeArguments != NULL)
  {
    KIM::ComputeCallbackName callbackName;
    int numberOfComputeCallbackNames = 0;
    KIM::COMPUTE_CALLBACK_NAME::GetNumberOfComputeCallbackNames(
        &numberOfComputeCallbackNames);

    for (int i = 0; i < numberOfComputeCallbackNames; ++i)
    {
      KIM::COMPUTE_CALLBACK_NAME::GetComputeCallbackName(i, &callbackName);
      KIM::SupportStatus supportStatus;
      error = computeArguments->GetCallbackSupportStatus(callbackName,
                                                         &supportStatus);
      if (!error)
      {
        edn << "\"" << callbackName.ToString() << "\""
            << " "
            << "\"" << supportStatus.ToString() << "\""
            << " ";
      }
    }
  }
  edn << "} ";

  edn << "}";

  // destroy the objects
  error = mdl->ComputeArgumentsDestroy(&computeArguments);
  KIM::Model::Destroy(&mdl);

  edn::EdnNode modelInfo;
  try
  {
    modelInfo = edn::read(edn.str());
  }
  catch (std::string e)
  {
    std::cerr << "Error reading edn string" << std::endl;
    exit(2);
  }

  std::cout << edn::pprint(modelInfo);

  return 0;
}
