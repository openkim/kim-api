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

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif
extern "C"
{
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif
extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif
}  // extern "C"

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif
extern "C"
{
#ifndef KIM_NUMBERING_H_
#include "KIM_Numbering.h"
#endif
}  // extern "C"

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif
extern "C"
{
#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif
}  // extern "C"

#ifndef KIM_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ComputeArguments.hpp"
#endif
extern "C"
{
#ifndef KIM_COMPUTE_ARGUMENTS_H_
#include "KIM_ComputeArguments.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_H_
#include "KIM_Model.h"
#endif
}  // extern "C"


struct KIM_ComputeArguments
{
  void * p;
};

struct KIM_Model
{
  void * p;
};

#define CONVERT_POINTER KIM::Model * pModel     \
  = reinterpret_cast<KIM::Model *>(model->p)

namespace
{
KIM_DataType
makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType typ;
  KIM_DataType * pTyp = (KIM_DataType *) &dataType;
  typ.dataTypeID = pTyp->dataTypeID;
  return typ;
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


extern "C"
{
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
  int err = KIM::Model::Create(
      makeNumberingCpp(numbering),
      makeLengthUnitCpp(requestedLengthUnit),
      makeEnergyUnitCpp(requestedEnergyUnit),
      makeChargeUnitCpp(requestedChargeUnit),
      makeTemperatureUnitCpp(requestedTemperatureUnit),
      makeTimeUnitCpp(requestedTimeUnit),
      modelNameC,
      requestedUnitsAccepted,
      &pModel);
  if (err)
  {
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
  KIM::Model * pModel = reinterpret_cast<KIM::Model *>((*model)->p);

  KIM::Model::Destroy(&pModel);
  delete (*model);
  *model = NULL;
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

  pModel->GetUnits(
      reinterpret_cast<KIM::LengthUnit *>(lengthUnit),
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

  int err = pModel->ComputeArgumentsCreate(&pComputeArguments);
  if (err)
  {
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

  KIM::ComputeArguments * pComputeArguments
      = reinterpret_cast<KIM::ComputeArguments *>
      ((*computeArguments)->p);

  int err = pModel->ComputeArgumentsDestroy(&pComputeArguments);
  if (err)
  {
    return true;
  }
  else
  {
    delete(*computeArguments);
    *computeArguments = NULL;
    return false;
  }
}

int KIM_Model_Compute(KIM_Model const * const model,
                      KIM_ComputeArguments const * const computeArguments)
{
  CONVERT_POINTER;

  KIM::ComputeArguments const * const pComputeArguments
      = reinterpret_cast<KIM::ComputeArguments const *>(computeArguments->p);

  return pModel->Compute(pComputeArguments);
}

int KIM_Model_ClearThenRefresh(KIM_Model * const model)
{
  CONVERT_POINTER;

  return pModel->ClearThenRefresh();
}

int KIM_Model_GetSpeciesSupportAndCode(KIM_Model const * const model,
                                       KIM_SpeciesName const speciesName,
                                       int * const speciesIsSupported,
                                       int * const code)
{
  CONVERT_POINTER;

  return pModel->GetSpeciesSupportAndCode(makeSpecNameCpp(speciesName),
                                          speciesIsSupported, code);
}

void KIM_Model_GetNumberOfParameters(KIM_Model const * const model,
                                     int * const numberOfParameters)
{
  CONVERT_POINTER;

  pModel->GetNumberOfParameters(numberOfParameters);
}

int KIM_Model_GetParameterDataTypeExtentAndDescription(
    KIM_Model const * const model, int const parameterIndex,
    KIM_DataType * const dataType, int * const extent,
    char const ** const description)
{
  CONVERT_POINTER;
  KIM::DataType typ;

  KIM::DataType * pTyp;
  if (dataType == NULL)
    pTyp = NULL;
  else
    pTyp = &typ;

  std::string const * pStr;
  std::string const ** ppStr;
  if (description == NULL)
    ppStr = NULL;
  else
    ppStr = &pStr;

  int error
      = pModel->GetParameterDataTypeExtentAndDescription(
          parameterIndex, pTyp, extent, ppStr);

  if (error)
    return true;
  else
  {
    if (dataType != NULL) *dataType = makeDataTypeC(typ);
    if (description != NULL) *description = pStr->c_str();
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

char const * KIM_Model_String(KIM_Model const * const model)
{
  CONVERT_POINTER;

  return pModel->String().c_str();
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
