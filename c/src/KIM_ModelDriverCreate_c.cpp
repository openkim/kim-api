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

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif
extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
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
  = reinterpret_cast<KIM::ModelDriverCreate *>(modelDriverCreate->p)

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

KIM::SpeciesName makeSpeciesNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesNameID);
}
}  // namespace

extern "C"
{
void KIM_ModelDriverCreate_GetNumberOfParameterFiles(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int * const numberOfParameterFiles)
{
  CONVERT_POINTER;

  pModelDriverCreate->GetNumberOfParameterFiles(
      numberOfParameterFiles);
}

int KIM_ModelDriverCreate_GetParameterFileName(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const index,
    char const ** const parameterFileName)
{
  CONVERT_POINTER;

  std::string const * pStr;
  std::string const ** ppStr;
  if (parameterFileName == NULL)
    ppStr = NULL;
  else
    ppStr = &pStr;

  int error
      = pModelDriverCreate->GetParameterFileName(index, ppStr);

  if (error)
    return true;
  else
  {
    if (parameterFileName != NULL) *parameterFileName = pStr->c_str();
    return false;
  }
}

int KIM_ModelDriverCreate_SetModelNumbering(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_Numbering const numbering)
{
  CONVERT_POINTER;

  return pModelDriverCreate
      ->SetModelNumbering(makeNumberingCpp(numbering));
}

void KIM_ModelDriverCreate_SetInfluenceDistancePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    double * const influenceDistance)
{
  CONVERT_POINTER;

  pModelDriverCreate->SetInfluenceDistancePointer(influenceDistance);
}

void KIM_ModelDriverCreate_SetNeighborListPointers(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles)
{
  CONVERT_POINTER;

  pModelDriverCreate->SetNeighborListPointers(
      numberOfNeighborLists,
      cutoffs,
      modelWillNotRequestNeighborsOfNoncontributingParticles);
}

int KIM_ModelDriverCreate_SetRefreshPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetRefreshPointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetDestroyPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetDestroyPointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetComputeArgumentsCreatePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetComputeArgumentsCreatePointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetComputeArgumentsDestroyPointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetComputeArgumentsDestroyPointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetComputePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetComputePointer(langN, fptr);
}

int KIM_ModelDriverCreate_SetSpeciesCode(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_SpeciesName const speciesName, int const code)
{
  CONVERT_POINTER;

  return pModelDriverCreate
      ->SetSpeciesCode(makeSpeciesNameCpp(speciesName), code);
}

int KIM_ModelDriverCreate_SetParameterPointerInteger(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent, int * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetParameterPointer(
      extent, ptr, description);
}

int KIM_ModelDriverCreate_SetParameterPointerDouble(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent, double * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetParameterPointer(
      extent, ptr, description);
}

void KIM_ModelDriverCreate_SetModelBufferPointer(
    KIM_ModelDriverCreate * const modelDriverCreate, void * const ptr)
{
  CONVERT_POINTER;

  pModelDriverCreate->SetModelBufferPointer(ptr);
}

int KIM_ModelDriverCreate_SetUnits(
    KIM_ModelDriverCreate * const modelDriverCreate,
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
    KIM_ModelDriverCreate const * const modelDriverCreate,
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

void KIM_ModelDriverCreate_LogEntry(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelDriverCreate->LogEntry(makeLogVerbosityCpp(logVerbosity), message,
                               lineNumber, fileName);
}

char const * KIM_ModelDriverCreate_String(
    KIM_ModelDriverCreate const * const modelDriverCreate)
{
  CONVERT_POINTER;

  return pModelDriverCreate->String().c_str();
}

}  // extern "C"
