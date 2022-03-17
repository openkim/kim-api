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
#include <string>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif
extern "C" {
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C" {
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
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

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif
extern "C" {
#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_DRIVER_CREATE_HPP_
#include "KIM_ModelDriverCreate.hpp"
#endif
extern "C" {
#ifndef KIM_MODEL_DRIVER_CREATE_H_
#include "KIM_ModelDriverCreate.h"
#endif
}  // extern "C"


struct KIM_ModelDriverCreate
{
  void * p;
};

#define CONVERT_POINTER                       \
  KIM::ModelDriverCreate * pModelDriverCreate \
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

KIM::TemperatureUnit
makeTemperatureUnitCpp(KIM_TemperatureUnit const temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}

KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
}

KIM::LanguageName makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageNameID);
}

KIM::ModelRoutineName
makeRoutineNameCpp(KIM_ModelRoutineName const modelRoutineName)
{
  return KIM::ModelRoutineName(modelRoutineName.modelRoutineNameID);
}

KIM::SpeciesName makeSpeciesNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesNameID);
}
}  // namespace

extern "C" {
void KIM_ModelDriverCreate_GetParameterFileDirectoryName(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    char const ** const directoryName)
{
  CONVERT_POINTER;

  std::string const * pStr = NULL;
  pModelDriverCreate->GetParameterFileDirectoryName(&pStr);
  *directoryName = pStr->c_str();
}

void KIM_ModelDriverCreate_GetNumberOfParameterFiles(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int * const numberOfParameterFiles)
{
  CONVERT_POINTER;

  pModelDriverCreate->GetNumberOfParameterFiles(numberOfParameterFiles);
}

int KIM_ModelDriverCreate_GetParameterFileName(
    KIM_ModelDriverCreate const * const modelDriverCreate,
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

  int error = pModelDriverCreate->GetParameterFileName(index, ppStr);

  if (error)
    return true;
  else
  {
    if (parameterFileName != NULL) *parameterFileName = pStr->c_str();
    return false;
  }
}

int KIM_ModelDriverCreate_GetParameterFileBasename(
    KIM_ModelDriverCreate const * const modelDriverCreate,
    int const index,
    char const ** const parameterFileBasename)
{
  CONVERT_POINTER;

  std::string const * pStr;
  std::string const ** ppStr;
  if (parameterFileBasename == NULL)
    ppStr = NULL;
  else
    ppStr = &pStr;

  int error = pModelDriverCreate->GetParameterFileBasename(index, ppStr);

  if (error)
    return true;
  else
  {
    if (parameterFileBasename != NULL) *parameterFileBasename = pStr->c_str();
    return false;
  }
}

int KIM_ModelDriverCreate_SetModelNumbering(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_Numbering const numbering)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetModelNumbering(makeNumberingCpp(numbering));
}

void KIM_ModelDriverCreate_SetInfluenceDistancePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    double const * const influenceDistance)
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

int KIM_ModelDriverCreate_SetRoutinePointer(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_ModelRoutineName const modelRoutineName,
    KIM_LanguageName const languageName,
    int const required,
    KIM_Function * const fptr)
{
  CONVERT_POINTER;

  KIM::ModelRoutineName routN = makeRoutineNameCpp(modelRoutineName);
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverCreate->SetRoutinePointer(
      routN, langN, required, reinterpret_cast<KIM::Function *>(fptr));
}

int KIM_ModelDriverCreate_SetSpeciesCode(
    KIM_ModelDriverCreate * const modelDriverCreate,
    KIM_SpeciesName const speciesName,
    int const code)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetSpeciesCode(makeSpeciesNameCpp(speciesName),
                                            code);
}

int KIM_ModelDriverCreate_SetParameterPointerInteger(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent,
    int * const ptr,
    char const * const name,
    char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetParameterPointer(
      extent, ptr, name, description);
}

int KIM_ModelDriverCreate_SetParameterPointerDouble(
    KIM_ModelDriverCreate * const modelDriverCreate,
    int const extent,
    double * const ptr,
    char const * const name,
    char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverCreate->SetParameterPointer(
      extent, ptr, name, description);
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

  return pModelDriverCreate->SetUnits(
      lengthU, energyU, chargeU, temperatureU, timeU);
}

int KIM_ModelDriverCreate_ConvertUnit(
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
  return KIM::ModelDriverCreate::ConvertUnit(
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
    KIM_LogVerbosity const logVerbosity,
    char const * const message,
    int const lineNumber,
    char const * const fileName)
{
  CONVERT_POINTER;

  pModelDriverCreate->LogEntry(
      makeLogVerbosityCpp(logVerbosity), message, lineNumber, fileName);
}

char const * KIM_ModelDriverCreate_ToString(
    KIM_ModelDriverCreate const * const modelDriverCreate)
{
  CONVERT_POINTER;

  return pModelDriverCreate->ToString().c_str();
}

}  // extern "C"
