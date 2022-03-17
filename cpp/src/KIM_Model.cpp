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
// Release: This file is part of the kim-api-2.3.0 package.
//


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_MODEL_ROUTINE_NAME_HPP_
#include "KIM_ModelRoutineName.hpp"
#endif

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

namespace KIM
{
int Model::Create(Numbering const numbering,
                  LengthUnit const requestedLengthUnit,
                  EnergyUnit const requestedEnergyUnit,
                  ChargeUnit const requestedChargeUnit,
                  TemperatureUnit const requestedTemperatureUnit,
                  TimeUnit const requestedTimeUnit,
                  std::string const & modelName,
                  int * const requestedUnitsAccepted,
                  Model ** const model)
{
  *model = new Model();

  int error = ModelImplementation::Create(numbering,
                                          requestedLengthUnit,
                                          requestedEnergyUnit,
                                          requestedChargeUnit,
                                          requestedTemperatureUnit,
                                          requestedTimeUnit,
                                          modelName,
                                          requestedUnitsAccepted,
                                          &((*model)->pimpl));
  if (error)
  {
    delete *model;
    *model = NULL;
    return true;
  }
  else
  {
    return false;
  }
}

void Model::Destroy(Model ** const model)
{
  if (*model != NULL) { ModelImplementation::Destroy(&((*model)->pimpl)); }
  delete *model;
  *model = NULL;
}

int Model::IsRoutinePresent(ModelRoutineName const modelRoutineName,
                            int * const present,
                            int * const required) const
{
  return pimpl->IsRoutinePresent(modelRoutineName, present, required);
}

void Model::GetInfluenceDistance(double * const influenceDistance) const
{
  pimpl->GetInfluenceDistance(influenceDistance);
}

void Model::GetNeighborListPointers(
    int * const numberOfNeighborLists,
    double const ** const cutoffs,
    int const ** const modelWillNotRequestNeighborsOfNoncontributingParticles)
    const
{
  pimpl->GetNeighborListPointers(
      numberOfNeighborLists,
      cutoffs,
      modelWillNotRequestNeighborsOfNoncontributingParticles);
}

void Model::GetUnits(LengthUnit * const lengthUnit,
                     EnergyUnit * const energyUnit,
                     ChargeUnit * const chargeUnit,
                     TemperatureUnit * const temperatureUnit,
                     TimeUnit * const timeUnit) const
{
  pimpl->GetUnits(
      lengthUnit, energyUnit, chargeUnit, temperatureUnit, timeUnit);
}

int Model::ComputeArgumentsCreate(
    ComputeArguments ** const computeArguments) const
{
  return pimpl->ComputeArgumentsCreate(computeArguments);
}

int Model::ComputeArgumentsDestroy(
    ComputeArguments ** const computeArguments) const
{
  return pimpl->ComputeArgumentsDestroy(computeArguments);
}

int Model::Compute(ComputeArguments const * const computeArguments) const
{
  return pimpl->Compute(computeArguments);
}

int Model::Extension(std::string const & extensionID,
                     void * const extensionStructure)
{
  return pimpl->Extension(extensionID, extensionStructure);
}

int Model::ClearThenRefresh() { return pimpl->ClearThenRefresh(); }

int Model::WriteParameterizedModel(std::string const & path,
                                   std::string const & modelName) const
{
  return pimpl->WriteParameterizedModel(path, modelName);
}

int Model::GetSpeciesSupportAndCode(SpeciesName const speciesName,
                                    int * const speciesIsSupported,
                                    int * const code) const
{
  return pimpl->GetSpeciesSupportAndCode(speciesName, speciesIsSupported, code);
}


void Model::GetNumberOfParameters(int * const numberOfParameters) const
{
  pimpl->GetNumberOfParameters(numberOfParameters);
}

int Model::GetParameterMetadata(int const parameterIndex,
                                DataType * const dataType,
                                int * const extent,
                                std::string const ** const name,
                                std::string const ** const description) const
{
  return pimpl->GetParameterMetadata(
      parameterIndex, dataType, extent, name, description);
}

int Model::GetParameter(int const parameterIndex,
                        int const arrayIndex,
                        int * const parameterValue) const
{
  return pimpl->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int Model::GetParameter(int const parameterIndex,
                        int const arrayIndex,
                        double * const parameterValue) const
{
  return pimpl->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int Model::SetParameter(int const parameterIndex,
                        int const arrayIndex,
                        int const parameterValue)
{
  return pimpl->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

int Model::SetParameter(int const parameterIndex,
                        int const arrayIndex,
                        double const parameterValue)
{
  return pimpl->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

void Model::SetSimulatorBufferPointer(void * const ptr)
{
  pimpl->SetSimulatorBufferPointer(ptr);
}

void Model::GetSimulatorBufferPointer(void ** const ptr) const
{
  pimpl->GetSimulatorBufferPointer(ptr);
}

std::string const & Model::ToString() const { return pimpl->ToString(); }

void Model::SetLogID(std::string const & logID) { pimpl->SetLogID(logID); }

void Model::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void Model::PopLogVerbosity() { pimpl->PopLogVerbosity(); }

Model::Model() : pimpl(NULL) {}

Model::~Model() {}

}  // namespace KIM
