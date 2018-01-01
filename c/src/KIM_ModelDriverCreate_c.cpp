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

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif
extern "C"
{
#ifndef KIM_SUPPORT_STATUS_H_
#include "KIM_SupportStatus.h"
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

#ifndef KIM_MODEL_DRIVER_CREATE_HPP_
#include "KIM_ModelDriverCreate.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_DRIVER_CREATE_H_
#include "KIM_ModelDriverCreate.h"
#endif
}  // extern "C"


struct KIM_ModelDriverCreate
{
  void * p;
};

#define CONVERT_POINTER                                         \
  KIM::ModelDriverCreate * pModelDriverCreate                   \
  = reinterpret_cast<KIM::ModelDriverCreate *>(modelCreate->p)

namespace
{
KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity const logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}

KIM::Numbering makeNumberingCpp(KIM_Numbering const numbering)
{
  return KIM::Numbering(numbering.numberingID);
}

KIM::SupportStatus makeSupportStatusCpp(KIM_SupportStatus const supportStatus)
{
  return KIM::SupportStatus(supportStatus.supportStatusID);
}

KIM::ArgumentName makeArgumentNameCpp(KIM_ArgumentName const argumentName)
{
  return KIM::ArgumentName(argumentName.argumentNameID);
}

KIM::CallbackName makeCallbackNameCpp(KIM_CallbackName const callbackName)
{
  return KIM::CallbackName(callbackName.callbackNameID);
}

KIM::LengthUnit makeLengthUnitCpp(KIM_LengthUnit const lengthUnit)
{
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}

KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  return KIM::EnergyUnit(energyUnit.energyUnitID);
}

KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  return KIM::ChargeUnit(chargeUnit.chargeUnitID);
}

KIM::TemperatureUnit makeTemperatureUnitCpp(
    KIM_TemperatureUnit const temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}

KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
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
}  // namespace

extern "C"
{
void KIM_ModelDriverCreate_GetNumberOfParameterFiles(
    KIM_ModelDriverCreate * const modelCreate,
    int * const numberOfParameterFiles)
{
  CONVERT_POINTER;

  pModelDriverCreate->GetNumberOfParameterFiles(
      numberOfParameterFiles);
}

int KIM_ModelDriverCreate_GetParameterFileName(
    KIM_ModelDriverCreate * const modelCreate,
    int const index,
    char const ** const parameterFileName)
{
  CONVERT_POINTER;

  static std::string str;
  std::string * pStr;
  if (parameterFileName == 0)
    pStr = 0;
  else
    pStr = &str;

  int error
      = pModelDriverCreate->GetParameterFileName(index, pStr);

  if (error)
    return true;
  else
  {
    if (parameterFileName != 0) *parameterFileName = str.c_str();
    return false;
  }
}

int KIM_ModelDriverCreate_SetModelNumbering(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_Numbering const numbering)
{
  CONVERT_POINTER;

  return pModelDriverCreate
      ->SetModelNumbering(makeNumberingCpp(numbering));
}

void KIM_ModelDriverCreate_SetInfluenceDistancePointer(
    KIM_ModelDriverCreate * const modelCreate,
    double * const influenceDistance)
{
  CONVERT_POINTER;

  pModelDriverCreate->SetInfluenceDistancePointer(influenceDistance);
}

void KIM_ModelDriverCreate_SetNeighborListCutoffsPointer(
    KIM_ModelDriverCreate * const modelCreate,
    int const numberOfCutoffs, double const * const cutoffs)
{
  CONVERT_POINTER;

  pModelDriverCreate->SetNeighborListCutoffsPointer(numberOfCutoffs, cutoffs);
}

int KIM_ModelDriverCreate_SetRefreshPointer(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetRefreshPointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetDestroyPointer(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetDestroyPointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetComputePointer(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetComputePointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetSpeciesCode(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_SpeciesName const speciesName, int const code)
{
  CONVERT_POINTER;

  return pModelDriverCreate
      ->SetSpeciesCode(makeSpecNameCpp(speciesName), code);
}

int KIM_ModelDriverCreate_SetArgumentSupportStatus(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_ArgumentName const argumentName, KIM_SupportStatus const supportStatus)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetArgumentSupportStatus(
      makeArgumentNameCpp(argumentName), makeSupportStatusCpp(supportStatus));
}

int KIM_ModelDriverCreate_SetCallbackSupportStatus(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_CallbackName const callbackName, KIM_SupportStatus const supportStatus)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetCallbackSupportStatus(
      makeCallbackNameCpp(callbackName), makeSupportStatusCpp(supportStatus));
}

int KIM_ModelDriverCreate_SetParameterPointerInteger(
    KIM_ModelDriverCreate * const modelCreate,
    int const extent, int * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetParameterPointer(
      extent, ptr, description);
}

int KIM_ModelDriverCreate_SetParameterPointerDouble(
    KIM_ModelDriverCreate * const modelCreate,
    int const extent, double * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetParameterPointer(
      extent, ptr, description);
}

void KIM_ModelDriverCreate_SetModelBufferPointer(
    KIM_ModelDriverCreate * const modelCreate, void * const ptr)
{
  CONVERT_POINTER;

  pModelDriverCreate->SetModelBufferPointer(ptr);
}

int KIM_ModelDriverCreate_SetUnits(
    KIM_ModelDriverCreate * const modelCreate,
    KIM_LengthUnit const lengthUnit,
    KIM_EnergyUnit const energyUnit,
    KIM_ChargeUnit const chargeUnit,
    KIM_TemperatureUnit const temperatureUnit,
    KIM_TimeUnit const timeUnit)
{
  CONVERT_POINTER;

  KIM::LengthUnit lengthU = makeLengthUnitCpp(lengthUnit);
  KIM::EnergyUnit energyU = makeEnergyUnitCpp(energyUnit);
  KIM::ChargeUnit chargeU = makeChargeUnitCpp(chargeUnit);
  KIM::TemperatureUnit temperatureU = makeTemperatureUnitCpp(temperatureUnit);
  KIM::TimeUnit timeU = makeTimeUnitCpp(timeUnit);

  return pModelDriverCreate->SetUnits(lengthU, energyU, chargeU,
                                      temperatureU, timeU);
}

int KIM_ModelDriverCreate_ConvertUnit(
    KIM_ModelDriverCreate const * const modelCreate,
    KIM_LengthUnit const fromLengthUnit,
    KIM_EnergyUnit const fromEnergyUnit,
    KIM_ChargeUnit const fromChargeUnit,
    KIM_TemperatureUnit const fromTemperatureUnit,
    KIM_TimeUnit const fromTimeUnit,
    KIM_LengthUnit const toLengthUnit,
    KIM_EnergyUnit const toEnergyUnit,
    KIM_ChargeUnit const toChargeUnit,
    KIM_TemperatureUnit const toTemperatureUnit,
    KIM_TimeUnit const toTimeUnit,
    double const lengthExponent,
    double const energyExponent,
    double const chargeExponent,
    double const temperatureExponent,
    double const timeExponent,
    double * const conversionFactor)
{
  CONVERT_POINTER;

  return pModelDriverCreate->ConvertUnit(
      makeLengthUnitCpp(fromLengthUnit),
      makeEnergyUnitCpp(fromEnergyUnit),
      makeChargeUnitCpp(fromChargeUnit),
      makeTemperatureUnitCpp(fromTemperatureUnit),
      makeTimeUnitCpp(fromTimeUnit),
      makeLengthUnitCpp(toLengthUnit),
      makeEnergyUnitCpp(toEnergyUnit),
      makeChargeUnitCpp(toChargeUnit),
      makeTemperatureUnitCpp(toTemperatureUnit),
      makeTimeUnitCpp(toTimeUnit),
      lengthExponent,
      energyExponent,
      chargeExponent,
      temperatureExponent,
      timeExponent,
      conversionFactor);
}

void KIM_ModelDriverCreate_Log(
    KIM_ModelDriverCreate const * const modelCreate,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelDriverCreate->Log(makeLogVerbosityCpp(logVerbosity), message,
                          lineNumber, fileName);
}

char const * const KIM_ModelDriverCreate_String(
    KIM_ModelDriverCreate const * const modelCreate)
{
  CONVERT_POINTER;
  static std::string result;
  result = pModelDriverCreate->String();

  return result.c_str();
}

}  // extern "C"
