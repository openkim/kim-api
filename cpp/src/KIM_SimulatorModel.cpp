//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
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
  else { return false; }
}

void SimulatorModel::Destroy(SimulatorModel ** const simulatorModel)
{
  if (*simulatorModel != NULL)
  {
    SimulatorModelImplementation::Destroy(&((*simulatorModel)->pimpl));
  }
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
