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

#ifndef KIM_MODEL_CREATE_HPP_
#include "KIM_ModelCreate.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_CREATE_H_
#include "KIM_ModelCreate.h"
#endif
}  // extern "C"


struct KIM_ModelCreate
{
  void * p;
};

#define CONVERT_POINTER KIM::ModelCreate * pModelCreate         \
  = reinterpret_cast<KIM::ModelCreate *>(modelCreate->p)

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

KIM::SpeciesName makeSpecNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesNameID);
}
}  // namespace

extern "C"
{
int KIM_ModelCreate_SetModelNumbering(
    KIM_ModelCreate * const modelCreate,
    KIM_Numbering const numbering)
{
  CONVERT_POINTER;

  return pModelCreate->SetModelNumbering(makeNumberingCpp(numbering));
}

void KIM_ModelCreate_SetInfluenceDistancePointer(
    KIM_ModelCreate * const modelCreate,
    double * const influenceDistance)
{
  CONVERT_POINTER;

  pModelCreate->SetInfluenceDistancePointer(influenceDistance);
}

void KIM_ModelCreate_SetNeighborListPointers(
    KIM_ModelCreate * const modelCreate,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles)
{
  CONVERT_POINTER;

  pModelCreate->SetNeighborListPointers(
      numberOfNeighborLists,
      cutoffs,
      modelWillNotRequestNeighborsOfNoncontributingParticles);
}

int KIM_ModelCreate_SetRefreshPointer(
    KIM_ModelCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelCreate->SetRefreshPointer(langN, fptr);
}

int KIM_ModelCreate_SetDestroyPointer(
    KIM_ModelCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelCreate->SetDestroyPointer(langN, fptr);
}

int KIM_ModelCreate_SetComputeArgumentsCreatePointer(
    KIM_ModelCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelCreate->SetComputeArgumentsCreatePointer(langN, fptr);
}

int KIM_ModelCreate_SetComputeArgumentsDestroyPointer(
    KIM_ModelCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelCreate->SetComputeArgumentsDestroyPointer(langN, fptr);
}

int KIM_ModelCreate_SetComputePointer(
    KIM_ModelCreate * const modelCreate,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelCreate->SetComputePointer(langN, fptr);
}

int KIM_ModelCreate_SetSpeciesCode(
    KIM_ModelCreate * const modelCreate,
    KIM_SpeciesName const speciesName, int const code)
{
  CONVERT_POINTER;

  return pModelCreate->SetSpeciesCode(makeSpecNameCpp(speciesName),
                                      code);
}

int KIM_ModelCreate_SetParameterPointerInteger(
    KIM_ModelCreate * const modelCreate,
    int const extent, int * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelCreate->SetParameterPointer(extent, ptr, description);
}

int KIM_ModelCreate_SetParameterPointerDouble(
    KIM_ModelCreate * const modelCreate,
    int const extent, double * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelCreate->SetParameterPointer(extent, ptr, description);
}

void KIM_ModelCreate_SetModelBufferPointer(
    KIM_ModelCreate * const modelCreate, void * const ptr)
{
  CONVERT_POINTER;

  pModelCreate->SetModelBufferPointer(ptr);
}

int KIM_ModelCreate_SetUnits(
    KIM_ModelCreate * const modelCreate,
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

  return pModelCreate->SetUnits(lengthU, energyU, chargeU,
                                temperatureU, timeU);
}

int KIM_ModelCreate_ConvertUnit(
    KIM_ModelCreate const * const modelCreate,
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

  return pModelCreate->ConvertUnit(
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

void KIM_ModelCreate_LogEntry(
    KIM_ModelCreate const * const modelCreate,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelCreate->LogEntry(makeLogVerbosityCpp(logVerbosity), message,
                         lineNumber, fileName);
}

char const * KIM_ModelCreate_String(KIM_ModelCreate const * const modelCreate)
{
  CONVERT_POINTER;

  return pModelCreate->String().c_str();
}

}  // extern "C"
