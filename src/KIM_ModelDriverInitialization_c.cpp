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

#ifndef KIM_ATTRIBUTE_HPP_
#include "KIM_Attribute.hpp"
#endif
extern "C"
{
#ifndef KIM_ATTRIBUTE_H_
#include "KIM_Attribute.h"
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

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif
extern "C"
{
#ifndef KIM_CALL_BACK_NAME_H_
#include "KIM_CallBackName.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_DRIVER_INITIALIZATION_HPP_
#include "KIM_ModelDriverInitialization.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_DRIVER_INITIALIZATION_H_
#include "KIM_ModelDriverInitialization.h"
#endif
}  // extern "C"


#define CONVERT_POINTER                                                 \
  KIM::ModelDriverInitialization *pModelDriverInitialization            \
  = reinterpret_cast<KIM::ModelDriverInitialization *>(modelInitialization->p)


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

KIM::Attribute makeAttributeCpp(KIM_Attribute const attribute)
{
  return KIM::Attribute(attribute.attributeID);
}

KIM::ArgumentName makeArgumentNameCpp(KIM_ArgumentName const argumentName)
{
  return KIM::ArgumentName(argumentName.argumentNameID);
}

KIM::CallBackName makeCallBackNameCpp(KIM_CallBackName const callBackName)
{
  return KIM::CallBackName(callBackName.callBackNameID);
}

KIM::LengthUnit makeLengthUnitCpp(KIM_LengthUnit const lengthUnit)
{
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}
KIM_LengthUnit makeLengthUnitC(KIM::LengthUnit const lengthUnit)
{
  KIM_LengthUnit lengthU = {lengthU.lengthUnitID};
  return lengthU;
}

KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  return KIM::EnergyUnit(energyUnit.energyUnitID);
}
KIM_EnergyUnit makeEnergyUnitC(KIM::EnergyUnit const energyUnit)
{
  KIM_EnergyUnit energyU = {energyUnit.energyUnitID};
  return energyU;
}

KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  return KIM::ChargeUnit(chargeUnit.chargeUnitID);
}
KIM_ChargeUnit makeChargeUnitC(KIM::ChargeUnit const chargeUnit)
{
  KIM_ChargeUnit chargeU = {chargeUnit.chargeUnitID};
  return chargeU;
}

KIM::TemperatureUnit makeTemperatureUnitCpp(
    KIM_TemperatureUnit const temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}
KIM_TemperatureUnit makeTemperatureUnitC(
    KIM::TemperatureUnit const temperatureUnit)
{
  KIM_TemperatureUnit temperatureU = {temperatureUnit.temperatureUnitID};
  return temperatureU;
}

KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
}
KIM_TimeUnit makeTimeUnitC(KIM::TimeUnit const timeUnit)
{
  KIM_TimeUnit timeU = {timeUnit.timeUnitID};
  return timeU;
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

KIM_SpeciesName makeSpecNameC(KIM::SpeciesName const speciesName)
{
  KIM_SpeciesName speciesN = {speciesName.speciesNameID};
  return speciesN;
}
}  // namespace

extern "C"
{
void KIM_ModelDriverInitialization_get_number_of_parameter_files(
    KIM_ModelDriverInitialization * const modelInitialization,
    int * const numberOfParameterFiles)
{
  CONVERT_POINTER;

  pModelDriverInitialization->get_number_of_parameter_files(
      numberOfParameterFiles);
}

int KIM_ModelDriverInitialization_get_parameter_file_name(
    KIM_ModelDriverInitialization * const modelInitialization,
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
      = pModelDriverInitialization->get_parameter_file_name(index, pStr);

  if (error)
    return true;
  else
  {
    if (parameterFileName != 0) *parameterFileName = str.c_str();
    return false;
  }
}

int KIM_ModelDriverInitialization_set_model_numbering(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_Numbering const numbering)
{
  CONVERT_POINTER;

  return pModelDriverInitialization
      ->set_model_numbering(makeNumberingCpp(numbering));
}

void KIM_ModelDriverInitialization_set_influence_distance(
    KIM_ModelDriverInitialization * const modelInitialization,
    double * const influenceDistance)
{
  CONVERT_POINTER;

  pModelDriverInitialization->set_influence_distance(influenceDistance);
}

void KIM_ModelDriverInitialization_set_cutoffs(
    KIM_ModelDriverInitialization * const modelInitialization,
    int const numberOfCutoffs, double const * const cutoffs)
{
  CONVERT_POINTER;

  pModelDriverInitialization->set_cutoffs(numberOfCutoffs, cutoffs);
}

int KIM_ModelDriverInitialization_set_reinit(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverInitialization->set_reinit(langN, fptr);
}

int KIM_ModelDriverInitialization_set_destroy(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverInitialization->set_destroy(langN, fptr);
}

int KIM_ModelDriverInitialization_set_compute_func(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_LanguageName const languageName, func * const fptr)
{
  CONVERT_POINTER;

  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModelDriverInitialization->set_compute_func(langN, fptr);
}

int KIM_ModelDriverInitialization_set_species_code(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_SpeciesName const speciesName, int const code)
{
  CONVERT_POINTER;

  return pModelDriverInitialization
      ->set_species_code(makeSpecNameCpp(speciesName), code);
}

int KIM_ModelDriverInitialization_set_argument_attribute(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_ArgumentName const argumentName, KIM_Attribute const attribute)
{
  CONVERT_POINTER;

  return pModelDriverInitialization->set_argument_attribute(
      makeArgumentNameCpp(argumentName), makeAttributeCpp(attribute));
}

int KIM_ModelDriverInitialization_set_call_back_attribute(
    KIM_ModelDriverInitialization * const modelInitialization,
    KIM_CallBackName const callBackName, KIM_Attribute const attribute)
{
  CONVERT_POINTER;

  return pModelDriverInitialization->set_call_back_attribute(
      makeCallBackNameCpp(callBackName), makeAttributeCpp(attribute));
}

int KIM_ModelDriverInitialization_set_parameter_int(
    KIM_ModelDriverInitialization * const modelInitialization,
    int const extent, int * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverInitialization->set_parameter(extent, ptr, description);
}

int KIM_ModelDriverInitialization_set_parameter_double(
    KIM_ModelDriverInitialization * const modelInitialization,
    int const extent, double * const ptr, char const * const description)
{
  CONVERT_POINTER;

  return pModelDriverInitialization->set_parameter(extent, ptr, description);
}

void KIM_ModelDriverInitialization_set_model_buffer(
    KIM_ModelDriverInitialization * const modelInitialization, void * const ptr)
{
  CONVERT_POINTER;

  pModelDriverInitialization->set_model_buffer(ptr);
}

int KIM_ModelDriverInitialization_set_units(
    KIM_ModelDriverInitialization * const modelInitialization,
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

  return pModelDriverInitialization->set_units(lengthU, energyU, chargeU,
                                               temperatureU, timeU);
}

int KIM_ModelDriverInitialization_convert_unit(
    KIM_ModelDriverInitialization const * const modelInitialization,
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

  return pModelDriverInitialization->convert_unit(
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

void KIM_ModelDriverInitialization_Log(
    KIM_ModelDriverInitialization const * const modelInitialization,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName)
{
  CONVERT_POINTER;

  pModelDriverInitialization->Log(makeLogVerbosityCpp(logVerbosity), message,
                                  lineNumber, fileName);
}

char const * const KIM_ModelDriverInitialization_string(
    KIM_ModelDriverInitialization const * const modelInitialization)
{
  CONVERT_POINTER;

  return (pModelDriverInitialization->string()).c_str();
}

}  // extern "C"
