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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


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

  return ModelImplementation::Create(numbering,
                                     requestedLengthUnit,
                                     requestedEnergyUnit,
                                     requestedChargeUnit,
                                     requestedTemperatureUnit,
                                     requestedTimeUnit,
                                     modelName,
                                     requestedUnitsAccepted,
                                     &((*model)->pimpl));
}

void Model::Destroy(Model ** const model)
{
  ModelImplementation::Destroy(&((*model)->pimpl));
  delete *model;
  *model = 0;
}

void Model::GetInfluenceDistance(double * const influenceDistance) const
{
  pimpl->GetInfluenceDistance(influenceDistance);
}

void Model::GetNeighborListCutoffsPointer(int * const numberOfCutoffs,
                                          double const ** cutoffs) const
{
  pimpl->GetNeighborListCutoffsPointer(numberOfCutoffs, cutoffs);
}

int Model::GetArgumentSupportStatus(ArgumentName const argumentName,
                                    SupportStatus * const supportStatus) const
{
  return pimpl->GetArgumentSupportStatus(argumentName, supportStatus);
}

int Model::GetCallbackSupportStatus(CallbackName const callbackName,
                                    SupportStatus * const supportStatus) const
{
  return pimpl->GetCallbackSupportStatus(callbackName, supportStatus);
}

void Model::GetUnits(LengthUnit * const lengthUnit,
                     EnergyUnit * const energyUnit,
                     ChargeUnit * const chargeUnit,
                     TemperatureUnit * const temperatureUnit,
                     TimeUnit * const timeUnit) const
{
  pimpl->GetUnits(lengthUnit, energyUnit, chargeUnit,temperatureUnit,
                  timeUnit);
}


int Model::SetArgumentPointer(ArgumentName const argumentName,
                              int const * const ptr)
{
  return pimpl->SetArgumentPointer(argumentName, ptr);
}

int Model::SetArgumentPointer(ArgumentName const argumentName,
                              double const * const ptr)
{
  return pimpl->SetArgumentPointer(argumentName, ptr);
}

int Model::SetCallbackPointer(CallbackName const callbackName,
                              LanguageName const languageName,
                              func * const fptr,
                              void const * const dataObject)
{
  return pimpl->SetCallbackPointer(callbackName, languageName, fptr,
                                   dataObject);
}

int Model::Compute() const
{
  return pimpl->Compute();
}

int Model::ClearInfluenceDistanceAndCutoffsThenRefreshModel()
{
  return pimpl->ClearInfluenceDistanceAndCutoffsThenRefreshModel();
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

int Model::GetParameterDataTypeExtentAndDescription(
    int const parameterIndex, DataType * const dataType, int * const extent,
    std::string const ** const description) const
{
  return pimpl->GetParameterDataTypeExtentAndDescription(
      parameterIndex, dataType, extent, description);
}

int Model::GetParameter(
    int const parameterIndex, int const arrayIndex,
    int * const parameterValue) const
{
  return pimpl->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int Model::GetParameter(
    int const parameterIndex, int const arrayIndex,
    double * const parameterValue) const
{
  return pimpl->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int Model::SetParameter(
    int const parameterIndex, int const arrayIndex,
    int const parameterValue)
{
  return pimpl->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

int Model::SetParameter(
    int const parameterIndex, int const arrayIndex,
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

std::string const & Model::String() const
{
  return pimpl->String();
}

void Model::SetLogID(std::string const & logID)
{
  pimpl->SetLogID(logID);
}

void Model::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void Model::PopLogVerbosity()
{
  pimpl->PopLogVerbosity();
}

Model::Model() : pimpl(0)
{
}

Model::~Model()
{
}

}  // namespace KIM
