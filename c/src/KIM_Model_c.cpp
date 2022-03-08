//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2021, Regents of the University of Minnesota.
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
#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif
extern "C" {
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_ROUTINE_NAME_HPP_
#include "KIM_ModelRoutineName.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_ROUTINE_NAME_H_
#include "KIM_ModelRoutineName.h"
#endif
}  // extern "C"

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif
extern "C" {
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif
}  // extern "C"

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif
extern "C" {
#ifndef KIM_NUMBERING_H_
#include "KIM_Numbering.h"
#endif
}  // extern "C"

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif
extern "C" {
#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_H_
#include "KIM_Model.h"
#endif
}  // extern "C"


namespace KIM
{
// Forward declarations
class ComputeArguments;
}  // namespace KIM


struct KIM_ComputeArguments
{
  void * p;
};

struct KIM_Model
{
  void * p;
};

#define CONVERT_POINTER \
  KIM::Model * pModel = reinterpret_cast<KIM::Model *>(model->p)

namespace
{
KIM_DataType makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType typ;
  KIM_DataType * pTyp = (KIM_DataType *) &dataType;
  typ.dataTypeID = pTyp->dataTypeID;
  return typ;
}

KIM::ModelRoutineName
makeRoutineNameCpp(KIM_ModelRoutineName const modelRoutineName)
{
  return KIM::ModelRoutineName(modelRoutineName.modelRoutineNameID);
}

KIM::SpeciesName makeSpecNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesNameID);
}

KIM::LengthUnit makeLengthUnitCpp(KIM_LengthUnit lengthUnit)
{
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}

KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit energyUnit)
{
  return KIM::EnergyUnit(energyUnit.energyUnitID);
}

KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit chargeUnit)
{
  return KIM::ChargeUnit(chargeUnit.chargeUnitID);
}

KIM::TemperatureUnit makeTemperatureUnitCpp(KIM_TemperatureUnit temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}

KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
}

KIM::Numbering makeNumberingCpp(KIM_Numbering numbering)
{
  return KIM::Numbering(numbering.numberingID);
}

KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}
}  // namespace


extern "C" {
int KIM_Model_Create(KIM_Numbering const numbering,
                     KIM_LengthUnit const requestedLengthUnit,
                     KIM_EnergyUnit const requestedEnergyUnit,
                     KIM_ChargeUnit const requestedChargeUnit,
                     KIM_TemperatureUnit const requestedTemperatureUnit,
                     KIM_TimeUnit const requestedTimeUnit,
                     char const * const modelName,
                     int * const requestedUnitsAccepted,
                     KIM_Model ** const model)
{
  std::string modelNameC(modelName);
  KIM::Model * pModel;
  int error
      = KIM::Model::Create(makeNumberingCpp(numbering),
                           makeLengthUnitCpp(requestedLengthUnit),
                           makeEnergyUnitCpp(requestedEnergyUnit),
                           makeChargeUnitCpp(requestedChargeUnit),
                           makeTemperatureUnitCpp(requestedTemperatureUnit),
                           makeTimeUnitCpp(requestedTimeUnit),
                           modelNameC,
                           requestedUnitsAccepted,
                           &pModel);
  if (error)
  {
    *model = NULL;
    return true;
  }
  else
  {
    (*model) = new KIM_Model;
    (*model)->p = (void *) pModel;
    return false;
  }
}

void KIM_Model_Destroy(KIM_Model ** const model)
{
  if (*model != NULL)
  {
    KIM::Model * pModel = reinterpret_cast<KIM::Model *>((*model)->p);

    KIM::Model::Destroy(&pModel);
  }
  delete (*model);
  *model = NULL;
}

int KIM_Model_IsRoutinePresent(KIM_Model const * const model,
                               KIM_ModelRoutineName const modelRoutineName,
                               int * const present,
                               int * const required)
{
  CONVERT_POINTER;

  return pModel->IsRoutinePresent(
      makeRoutineNameCpp(modelRoutineName), present, required);
}

void KIM_Model_GetInfluenceDistance(KIM_Model const * const model,
                                    double * const influenceDistance)
{
  CONVERT_POINTER;

  pModel->GetInfluenceDistance(influenceDistance);
}

void KIM_Model_GetNeighborListPointers(
    KIM_Model const * const model,
    int * const numberOfNeighborLists,
    double const ** const cutoffs,
    int const ** const modelWillNotRequestNeighborsOfNoncontributingParticles)
{
  CONVERT_POINTER;

  pModel->GetNeighborListPointers(
      numberOfNeighborLists,
      cutoffs,
      modelWillNotRequestNeighborsOfNoncontributingParticles);
}

void KIM_Model_GetUnits(KIM_Model const * const model,
                        KIM_LengthUnit * const lengthUnit,
                        KIM_EnergyUnit * const energyUnit,
                        KIM_ChargeUnit * const chargeUnit,
                        KIM_TemperatureUnit * const temperatureUnit,
                        KIM_TimeUnit * const timeUnit)
{
  CONVERT_POINTER;

  pModel->GetUnits(reinterpret_cast<KIM::LengthUnit *>(lengthUnit),
                   reinterpret_cast<KIM::EnergyUnit *>(energyUnit),
                   reinterpret_cast<KIM::ChargeUnit *>(chargeUnit),
                   reinterpret_cast<KIM::TemperatureUnit *>(temperatureUnit),
                   reinterpret_cast<KIM::TimeUnit *>(timeUnit));
}

int KIM_Model_ComputeArgumentsCreate(
    KIM_Model const * const model,
    KIM_ComputeArguments ** const computeArguments)
{
  CONVERT_POINTER;

  KIM::ComputeArguments * pComputeArguments;

  int error = pModel->ComputeArgumentsCreate(&pComputeArguments);
  if (error)
  {
    *computeArguments = NULL;
    return true;
  }
  else
  {
    (*computeArguments) = new KIM_ComputeArguments;
    (*computeArguments)->p = (void *) pComputeArguments;
    return false;
  }
}

int KIM_Model_ComputeArgumentsDestroy(
    KIM_Model const * const model,
    KIM_ComputeArguments ** const computeArguments)
{
  CONVERT_POINTER;

  int error = false;
  if (*computeArguments != NULL)
  {
    KIM::ComputeArguments * pComputeArguments
        = reinterpret_cast<KIM::ComputeArguments *>((*computeArguments)->p);

    error = pModel->ComputeArgumentsDestroy(&pComputeArguments);
  }
  delete (*computeArguments);
  *computeArguments = NULL;
  return error;
}

int KIM_Model_Compute(KIM_Model const * const model,
                      KIM_ComputeArguments const * const computeArguments)
{
  CONVERT_POINTER;

  KIM::ComputeArguments const * const pComputeArguments
      = reinterpret_cast<KIM::ComputeArguments const *>(computeArguments->p);

  return pModel->Compute(pComputeArguments);
}

int KIM_Model_Extension(KIM_Model * const model,
                        char const * const extensionID,
                        void * const extensionStructure)

{
  CONVERT_POINTER;

  return pModel->Extension(extensionID, extensionStructure);
}

int KIM_Model_ClearThenRefresh(KIM_Model * const model)
{
  CONVERT_POINTER;

  return pModel->ClearThenRefresh();
}

int KIM_Model_WriteParameterizedModel(KIM_Model const * const model,
                                      char const * const path,
                                      char const * const modelName)
{
  CONVERT_POINTER;

  return pModel->WriteParameterizedModel(path, modelName);
}

int KIM_Model_GetSpeciesSupportAndCode(KIM_Model const * const model,
                                       KIM_SpeciesName const speciesName,
                                       int * const speciesIsSupported,
                                       int * const code)
{
  CONVERT_POINTER;

  return pModel->GetSpeciesSupportAndCode(
      makeSpecNameCpp(speciesName), speciesIsSupported, code);
}

void KIM_Model_GetNumberOfParameters(KIM_Model const * const model,
                                     int * const numberOfParameters)
{
  CONVERT_POINTER;

  pModel->GetNumberOfParameters(numberOfParameters);
}

int KIM_Model_GetParameterMetadata(KIM_Model const * const model,
                                   int const parameterIndex,
                                   KIM_DataType * const dataType,
                                   int * const extent,
                                   char const ** const name,
                                   char const ** const description)
{
  CONVERT_POINTER;
  KIM::DataType typ;

  KIM::DataType * pTyp;
  if (dataType == NULL)
    pTyp = NULL;
  else
    pTyp = &typ;

  std::string const * pStrName;
  std::string const ** ppStrName;
  if (name == NULL)
    ppStrName = NULL;
  else
    ppStrName = &pStrName;

  std::string const * pStrDesc;
  std::string const ** ppStrDesc;
  if (description == NULL)
    ppStrDesc = NULL;
  else
    ppStrDesc = &pStrDesc;

  int error = pModel->GetParameterMetadata(
      parameterIndex, pTyp, extent, ppStrName, ppStrDesc);

  if (error)
    return true;
  else
  {
    if (dataType != NULL) *dataType = makeDataTypeC(typ);
    if (name != NULL) *name = pStrName->c_str();
    if (description != NULL) *description = pStrDesc->c_str();
    return false;
  }
}

int KIM_Model_GetParameterInteger(KIM_Model const * const model,
                                  int const parameterIndex,
                                  int const arrayIndex,
                                  int * const parameterValue)
{
  CONVERT_POINTER;

  return pModel->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int KIM_Model_GetParameterDouble(KIM_Model const * const model,
                                 int const parameterIndex,
                                 int const arrayIndex,
                                 double * const parameterValue)
{
  CONVERT_POINTER;

  return pModel->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int KIM_Model_SetParameterInteger(KIM_Model * const model,
                                  int const parameterIndex,
                                  int const arrayIndex,
                                  int const parameterValue)
{
  CONVERT_POINTER;

  return pModel->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

int KIM_Model_SetParameterDouble(KIM_Model * const model,
                                 int const parameterIndex,
                                 int const arrayIndex,
                                 double const parameterValue)
{
  CONVERT_POINTER;

  return pModel->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

void KIM_Model_SetSimulatorBufferPointer(KIM_Model * const model,
                                         void * const ptr)
{
  CONVERT_POINTER;

  pModel->SetSimulatorBufferPointer(ptr);
}

void KIM_Model_GetSimulatorBufferPointer(KIM_Model const * const model,
                                         void ** const ptr)
{
  CONVERT_POINTER;

  pModel->GetSimulatorBufferPointer(ptr);
}

char const * KIM_Model_ToString(KIM_Model const * const model)
{
  CONVERT_POINTER;

  return pModel->ToString().c_str();
}

void KIM_Model_SetLogID(KIM_Model * const model, char const * const logID)
{
  CONVERT_POINTER;

  pModel->SetLogID(logID);
}

void KIM_Model_PushLogVerbosity(KIM_Model * const model,
                                KIM_LogVerbosity const logVerbosity)
{
  CONVERT_POINTER;

  pModel->PushLogVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_Model_PopLogVerbosity(KIM_Model * const model)
{
  CONVERT_POINTER;

  pModel->PopLogVerbosity();
}

}  // extern "C"
