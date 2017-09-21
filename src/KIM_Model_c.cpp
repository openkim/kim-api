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
// Copyright (c) 2016--2017, Regents of the University of Minnesota.
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

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif
extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif
}  // extern "C"

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C"
{
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
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

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif
extern "C"
{
#ifndef KIM_NUMBERING_H_
#include "KIM_Numbering.h"
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

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif
extern "C"
{
#ifndef KIM_ARGUMENT_NAME_H_
#include "KIM_ArgumentName.h"
#endif
}  // extern "C"

#ifndef KIM_CALLBACK_NAME_HPP_
#include "KIM_CallbackName.hpp"
#endif
extern "C"
{
#ifndef KIM_CALLBACK_NAME_H_
#include "KIM_CallbackName.h"
#endif
}  // extern "C"

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif
extern "C"
{
#ifndef KIM_SUPPORT_STATUS_H_
#include "KIM_SupportStatus.h"
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

KIM::LanguageName
makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageNameID);
}

KIM::SpeciesName makeSpecNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesNameID);
}

KIM::ArgumentName makeArgumentNameCpp(KIM_ArgumentName const argumentName)
{
  return KIM::ArgumentName(argumentName.argumentNameID);
}

KIM::CallbackName makeCallbackNameCpp(KIM_CallbackName const callbackName)
{
  return KIM::CallbackName(callbackName.callbackNameID);
}

KIM_SupportStatus const makeSupportStatusC(KIM::SupportStatus supportStatus)
{
  KIM_SupportStatus supportStatusC = {supportStatus.supportStatusID};
  return supportStatusC;
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
  KIM::Model * pmodel;
  int err = KIM::Model::Create(
      makeNumberingCpp(numbering),
      makeLengthUnitCpp(requestedLengthUnit),
      makeEnergyUnitCpp(requestedEnergyUnit),
      makeChargeUnitCpp(requestedChargeUnit),
      makeTemperatureUnitCpp(requestedTemperatureUnit),
      makeTimeUnitCpp(requestedTimeUnit),
      modelNameC,
      requestedUnitsAccepted,
      &pmodel);
  if (err)
  {
    return true;
  }
  else
  {
    (*model) = new KIM_Model;
    (*model)->p = (void *) pmodel;
    return false;
  }
}

void KIM_Model_Destroy(KIM_Model ** const model)
{
  KIM::Model * pmodel = (KIM::Model *) (*model)->p;
  KIM::Model::Destroy(&pmodel);
  delete (*model);
  *model = 0;
}

void KIM_Model_GetInfluenceDistance(KIM_Model const * const model,
                                    double * const influenceDistance)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->GetInfluenceDistance(influenceDistance);
}

void KIM_Model_GetNeighborListCutoffsPointer(KIM_Model const * const model,
                                             int * const numberOfCutoffs,
                                             double const ** const cutoffs)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->GetNeighborListCutoffsPointer(numberOfCutoffs, cutoffs);
}

int KIM_Model_GetArgumentSupportStatus(KIM_Model const * const model,
                                       KIM_ArgumentName const argumentName,
                                       KIM_SupportStatus * const supportStatus)
{
  KIM::SupportStatus supportStatusCpp;
  KIM::Model * pModel = (KIM::Model *) model->p;
  int error = pModel->GetArgumentSupportStatus(
      makeArgumentNameCpp(argumentName),
      &supportStatusCpp);
  if (error)
    return true;
  else
  {
    *supportStatus = makeSupportStatusC(supportStatusCpp);
    return false;
  }
}

int KIM_Model_GetCallbackSupportStatus(KIM_Model const * const model,
                                       KIM_CallbackName const callbackName,
                                       KIM_SupportStatus * const supportStatus)
{
  KIM::SupportStatus supportStatusCpp;
  KIM::Model * pModel = (KIM::Model *) model->p;
  int error = pModel->GetCallbackSupportStatus(
      makeCallbackNameCpp(callbackName),
      &supportStatusCpp);
  if (error)
    return true;
  else
  {
    *supportStatus = makeSupportStatusC(supportStatusCpp);
    return false;
  }
}

void KIM_Model_GetUnits(KIM_Model const * const model,
                        KIM_LengthUnit * const lengthUnit,
                        KIM_EnergyUnit * const energyUnit,
                        KIM_ChargeUnit * const chargeUnit,
                        KIM_TemperatureUnit * const temperatureUnit,
                        KIM_TimeUnit * const timeUnit)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  pModel->GetUnits(
      reinterpret_cast<KIM::LengthUnit *>(lengthUnit),
      reinterpret_cast<KIM::EnergyUnit *>(energyUnit),
      reinterpret_cast<KIM::ChargeUnit *>(chargeUnit),
      reinterpret_cast<KIM::TemperatureUnit *>(temperatureUnit),
      reinterpret_cast<KIM::TimeUnit *>(timeUnit));
}


// *data functions
int KIM_Model_SetArgumentPointerInteger(KIM_Model * const model,
                                        KIM_ArgumentName const argumentName,
                                        int const * const ptr)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  KIM::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pModel->SetArgumentPointer(argN, ptr);
}

int KIM_Model_SetArgumentPointerDouble(KIM_Model * const model,
                                       KIM_ArgumentName const argumentName,
                                       double const * const ptr)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  KIM::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pModel->SetArgumentPointer(argN, ptr);
}

int KIM_Model_SetCallbackPointer(KIM_Model * const model,
                                 KIM_CallbackName const callbackName,
                                 KIM_LanguageName const languageName,
                                 func * const fptr,
                                 void const * const dataObject)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  KIM::CallbackName callbackNameCpp = makeCallbackNameCpp(callbackName);
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModel->SetCallbackPointer(callbackNameCpp, langN, fptr, dataObject);
}

int KIM_Model_Compute(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->Compute();
}

int KIM_Model_ClearInfluenceDistanceAndCutoffsThenRefreshModel(
    KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->ClearInfluenceDistanceAndCutoffsThenRefreshModel();
}

int KIM_Model_GetSpeciesSupportAndCode(KIM_Model const * const model,
                                       KIM_SpeciesName const speciesName,
                                       int * const speciesIsSupported,
                                       int * const code)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->GetSpeciesSupportAndCode(makeSpecNameCpp(speciesName),
                                          speciesIsSupported, code);
}

void KIM_Model_GetNumberOfParameters(KIM_Model const * const model,
                                     int * const numberOfParameters)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->GetNumberOfParameters(numberOfParameters);
}

int KIM_Model_GetParameterDataTypeExtentAndDescription(
    KIM_Model const * const model, int const parameterIndex,
    KIM_DataType * const dataType, int * const extent,
    char const ** const description)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::DataType typ;
  static std::string str;

  KIM::DataType *pTyp;
  if (dataType == 0)
    pTyp = 0;
  else
    pTyp = &typ;

  std::string * pStr;
  if (description == 0)
    pStr = 0;
  else
    pStr = &str;

  int error
      = pmodel->GetParameterDataTypeExtentAndDescription(
          parameterIndex, pTyp, extent, pStr);

  if (error)
    return true;
  else
  {
    if (dataType != 0) *dataType = makeDataTypeC(typ);
    if (description != 0) *description = str.c_str();
    return false;
  }
}

int KIM_Model_GetParameterInteger(KIM_Model const * const model,
                                  int const parameterIndex,
                                  int const arrayIndex,
                                  int * const parameterValue)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;

  return pmodel->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int KIM_Model_GetParameterDouble(KIM_Model const * const model,
                                 int const parameterIndex,
                                 int const arrayIndex,
                                 double * const parameterValue)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;

  return pmodel->GetParameter(parameterIndex, arrayIndex, parameterValue);
}

int KIM_Model_SetParameterInteger(KIM_Model * const model,
                                  int const parameterIndex,
                                  int const arrayIndex,
                                  int const parameterValue)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;

  return pmodel->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

int KIM_Model_SetParameterDouble(KIM_Model * const model,
                                 int const parameterIndex,
                                 int const arrayIndex,
                                 double const parameterValue)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;

  return pmodel->SetParameter(parameterIndex, arrayIndex, parameterValue);
}

void KIM_Model_SetSimulatorBufferPointer(KIM_Model * const model,
                                         void * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->SetSimulatorBufferPointer(ptr);
}

void KIM_Model_GetSimulatorBufferPointer(KIM_Model const * const model,
                                         void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->GetSimulatorBufferPointer(ptr);
}

char const * const KIM_Model_String(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;

  static std::string modelString;
  modelString = pmodel->String();
  return modelString.c_str();
}

void KIM_Model_SetLogID(KIM_Model * const model, char const * const logID)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->SetLogID(logID);
}

void KIM_Model_PushLogVerbosity(KIM_Model * const model,
                                KIM_LogVerbosity const logVerbosity)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->PushLogVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_Model_PopLogVerbosity(KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->PopLogVerbosity();
}

}  // extern "C"
