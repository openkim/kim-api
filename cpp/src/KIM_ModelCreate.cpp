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


#ifndef KIM_MODEL_CREATE_HPP_
#include "KIM_ModelCreate.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#define CONVERT_POINTER ModelImplementation *pImpl      \
  = reinterpret_cast<ModelImplementation *>(pimpl)


namespace KIM
{
int ModelCreate::SetModelNumbering(Numbering const numbering)
{
  CONVERT_POINTER;

  return pImpl->SetModelNumbering(numbering);
}

void ModelCreate::SetInfluenceDistancePointer(
    double const * const influenceDistance)
{
  CONVERT_POINTER;

  pImpl->SetInfluenceDistancePointer(influenceDistance);
}

void ModelCreate::SetNeighborListPointers(
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles)
{
  CONVERT_POINTER;

  pImpl->SetNeighborListPointers(
      numberOfNeighborLists,
      cutoffs,
      modelWillNotRequestNeighborsOfNoncontributingParticles);
}

int ModelCreate::SetRefreshPointer(LanguageName const languageName,
                                   func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->SetRefreshPointer(languageName, fptr);
}

int ModelCreate::SetDestroyPointer(LanguageName const languageName,
                                   func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->SetDestroyPointer(languageName, fptr);
}

int ModelCreate::SetComputeArgumentsCreatePointer(
    LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->SetComputeArgumentsCreatePointer(languageName, fptr);
}

int ModelCreate::SetComputeArgumentsDestroyPointer(
    LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->SetComputeArgumentsDestroyPointer(languageName, fptr);
}

int ModelCreate::SetComputePointer(LanguageName const languageName,
                                   func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->SetComputePointer(languageName, fptr);
}

int ModelCreate::SetSpeciesCode(SpeciesName const speciesName,
                                int const code)
{
  CONVERT_POINTER;

  return pImpl->SetSpeciesCode(speciesName, code);
}

int ModelCreate::SetParameterPointer(int const extent, int * const ptr,
                                     std::string const & description)
{
  CONVERT_POINTER;

  return pImpl->SetParameterPointer(extent, ptr, description);
}

int ModelCreate::SetParameterPointer(int const extent,
                                     double * const ptr,
                                     std::string const & description)
{
  CONVERT_POINTER;

  return pImpl->SetParameterPointer(extent, ptr, description);
}

void ModelCreate::SetModelBufferPointer(void * const ptr)
{
  CONVERT_POINTER;

  pImpl->SetModelBufferPointer(ptr);
}


int ModelCreate::SetUnits(LengthUnit const lengthUnit,
                          EnergyUnit const energyUnit,
                          ChargeUnit const chargeUnit,
                          TemperatureUnit const temperatureUnit,
                          TimeUnit const timeUnit)
{
  CONVERT_POINTER;

  return pImpl->SetUnits(lengthUnit, energyUnit, chargeUnit, temperatureUnit,
                         timeUnit);
}


int ModelCreate::ConvertUnit(
    LengthUnit const fromLengthUnit,
    EnergyUnit const fromEnergyUnit,
    ChargeUnit const fromChargeUnit,
    TemperatureUnit const fromTemperatureUnit,
    TimeUnit const fromTimeUnit,
    LengthUnit const toLengthUnit,
    EnergyUnit const toEnergyUnit,
    ChargeUnit const toChargeUnit,
    TemperatureUnit const toTemperatureUnit,
    TimeUnit const toTimeUnit,
    double const lengthExponent,
    double const energyExponent,
    double const chargeExponent,
    double const temperatureExponent,
    double const timeExponent,
    double * const conversionFactor) const
{
  CONVERT_POINTER;

  return pImpl->ConvertUnit(
      fromLengthUnit,
      fromEnergyUnit,
      fromChargeUnit,
      fromTemperatureUnit,
      fromTimeUnit,
      toLengthUnit,
      toEnergyUnit,
      toChargeUnit,
      toTemperatureUnit,
      toTimeUnit,
      lengthExponent,
      energyExponent,
      chargeExponent,
      temperatureExponent,
      timeExponent,
      conversionFactor);
}

void ModelCreate::LogEntry(LogVerbosity const logVerbosity,
                           std::string const & message,
                           int const lineNumber,
                           std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelCreate::LogEntry(LogVerbosity const logVerbosity,
                           std::stringstream const & message,
                           int const lineNumber,
                           std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelCreate::String() const
{
  CONVERT_POINTER;

  return pImpl->String();
}

ModelCreate::ModelCreate() : pimpl(NULL)
{
}

ModelCreate::~ModelCreate()
{
}

}  // namespace KIM
