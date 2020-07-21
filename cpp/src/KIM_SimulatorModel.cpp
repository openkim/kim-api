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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_SIMULATOR_MODEL_HPP_
#include "KIM_SimulatorModel.hpp"
#endif

#ifndef KIM_SIMULATOR_MODEL_IMPLEMENTATION_HPP_
#include "KIM_SimulatorModelImplementation.hpp"
#endif

namespace KIM
{
int SimulatorModel::Create(std::string const & simulatorModelName,
                           SimulatorModel ** const simulatorModel)
{
  *simulatorModel = new SimulatorModel();

  int error = SimulatorModelImplementation::Create(simulatorModelName,
                                                   &((*simulatorModel)->pimpl));
  if (error)
  {
    delete *simulatorModel;
    *simulatorModel = NULL;
    return true;
  }
  else
  {
    return false;
  }
}

void SimulatorModel::Destroy(SimulatorModel ** const simulatorModel)
{
  if (*simulatorModel != NULL)
  { SimulatorModelImplementation::Destroy(&((*simulatorModel)->pimpl)); }
  delete *simulatorModel;
  *simulatorModel = NULL;
}

void SimulatorModel::GetSimulatorNameAndVersion(
    std::string const ** const simulatorName,
    std::string const ** const simulatorVersion) const
{
  pimpl->GetSimulatorNameAndVersion(simulatorName, simulatorVersion);
}

void SimulatorModel::GetNumberOfSupportedSpecies(
    int * const numberOfSupportedSpecies) const
{
  pimpl->GetNumberOfSupportedSpecies(numberOfSupportedSpecies);
}

int SimulatorModel::GetSupportedSpecies(
    int const index, std::string const ** const speciesName) const
{
  return pimpl->GetSupportedSpecies(index, speciesName);
}

void SimulatorModel::OpenAndInitializeTemplateMap()
{
  pimpl->OpenAndInitializeTemplateMap();
}

int SimulatorModel::TemplateMapIsOpen() const
{
  return pimpl->TemplateMapIsOpen();
}

int SimulatorModel::AddTemplateMap(std::string const & key,
                                   std::string const & value)
{
  return pimpl->AddTemplateMap(key, value);
}

void SimulatorModel::CloseTemplateMap() { pimpl->CloseTemplateMap(); }

void SimulatorModel::GetNumberOfSimulatorFields(
    int * const numberOfSimulatorFields) const
{
  pimpl->GetNumberOfSimulatorFields(numberOfSimulatorFields);
}

int SimulatorModel::GetSimulatorFieldMetadata(
    int const fieldIndex,
    int * const extent,
    std::string const ** const fieldName) const
{
  return pimpl->GetSimulatorFieldMetadata(fieldIndex, extent, fieldName);
}

int SimulatorModel::GetSimulatorFieldLine(
    int const fieldIndex,
    int const lineIndex,
    std::string const ** const lineValue) const
{
  return pimpl->GetSimulatorFieldLine(fieldIndex, lineIndex, lineValue);
}

void SimulatorModel::GetParameterFileDirectoryName(
    std::string const ** const directoryName) const
{
  pimpl->GetParameterFileDirectoryName(directoryName);
}

void SimulatorModel::GetSpecificationFileName(
    std::string const ** const specificationFileName) const
{
  pimpl->GetSpecificationFileName(specificationFileName);
}

void SimulatorModel::GetNumberOfParameterFiles(
    int * const numberOfParameterFiles) const
{
  pimpl->GetNumberOfParameterFiles(numberOfParameterFiles);
}

int SimulatorModel::GetParameterFileName(
    int const index, std::string const ** const parameterFileName) const
{
  return pimpl->GetParameterFileName(index, parameterFileName);
}

int SimulatorModel::GetParameterFileBasename(
    int const index, std::string const ** const parameterFileBasename) const
{
  return pimpl->GetParameterFileBasename(index, parameterFileBasename);
}

void SimulatorModel::SetSimulatorBufferPointer(void * const ptr)
{
  pimpl->SetSimulatorBufferPointer(ptr);
}

void SimulatorModel::GetSimulatorBufferPointer(void ** const ptr) const
{
  pimpl->GetSimulatorBufferPointer(ptr);
}

std::string const & SimulatorModel::ToString() const
{
  return pimpl->ToString();
}

void SimulatorModel::SetLogID(std::string const & logID)
{
  pimpl->SetLogID(logID);
}

void SimulatorModel::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void SimulatorModel::PopLogVerbosity() { pimpl->PopLogVerbosity(); }

SimulatorModel::SimulatorModel() : pimpl(NULL) {}

SimulatorModel::~SimulatorModel() {}

}  // namespace KIM
