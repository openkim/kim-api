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

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_PARAMETER_HPP_
#include "KIM_Parameter.hpp"
#endif

#ifndef KIM_SIMULATOR_HPP_
#include "KIM_Simulator.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif

extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif

#ifndef KIM_PARAMETER_H_
#include "KIM_Parameter.h"
#endif

#ifndef KIM_SIMULATOR_H_
#include "KIM_Simulator.h"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_COMPUTE_ArgumentName.h"
#endif

#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif

namespace
{
static KIM::LengthUnit makeLengthUnitCpp(KIM_LengthUnit const lengthUnit)
{
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}
static KIM_LengthUnit makeLengthUnitC(KIM::LengthUnit const lengthUnit)
{
  KIM_LengthUnit lengthU;
  KIM_LengthUnit * pLengthU = (KIM_LengthUnit *) &lengthUnit;
  lengthU.lengthUnitID = pLengthU->lengthUnitID;
  return lengthU;
}

static KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit const energyUnit)
{
  return KIM::EnergyUnit(energyUnit.energyUnitID);
}
static KIM_EnergyUnit makeEnergyUnitC(KIM::EnergyUnit const energyUnit)
{
  KIM_EnergyUnit energyU;
  KIM_EnergyUnit * pEnergyU = (KIM_EnergyUnit *) &energyUnit;
  energyU.energyUnitID = pEnergyU->energyUnitID;
  return energyU;
}

static KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit const chargeUnit)
{
  return KIM::ChargeUnit(chargeUnit.chargeUnitID);
}
static KIM_ChargeUnit makeChargeUnitC(KIM::ChargeUnit const chargeUnit)
{
  KIM_ChargeUnit chargeU;
  KIM_ChargeUnit * pChargeU = (KIM_ChargeUnit *) &chargeUnit;
  chargeU.chargeUnitID = pChargeU->chargeUnitID;
  return chargeU;
}

static KIM::TemperatureUnit makeTemperatureUnitCpp(
    KIM_TemperatureUnit const temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}
static KIM_TemperatureUnit makeTemperatureUnitC(
    KIM::TemperatureUnit const temperatureUnit)
{
  KIM_TemperatureUnit temperatureU;
  KIM_TemperatureUnit * plU = (KIM_TemperatureUnit *) &temperatureUnit;
  temperatureU.temperatureUnitID = plU->temperatureUnitID;
  return temperatureU;
}

static KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit const timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
}
static KIM_TimeUnit makeTimeUnitC(KIM::TimeUnit const timeUnit)
{
  KIM_TimeUnit timeU;
  KIM_TimeUnit * plU = (KIM_TimeUnit *) &timeUnit;
  timeU.timeUnitID = plU->timeUnitID;
  return timeU;
}

//static KIM::COMPUTE::ArgumentName
//makeArgumentNameCpp(KIM_COMPUTE_ArgumentName const argumentName)
//{
//  return KIM::COMPUTE::ArgumentName(argumentName.argumentID);
//}

static KIM::LanguageName
makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageID);
}

//static KIM_LanguageName
//makeLanguageNameC(KIM::LanguageName const languageName)
//{
//  KIM_LanguageName langN;
//  KIM_LanguageName * pLN = (KIM_LanguageName*) & languageName;
//  langN.languageID = pLN->languageID;
//  return langN;
//}

static KIM::SpeciesName makeSpecNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesID);
}

static KIM_SpeciesName makeSpecNameC(KIM::SpeciesName const speciesName)
{
  KIM_SpeciesName specN;
  KIM_SpeciesName *pSN = (KIM_SpeciesName*) & speciesName;
  specN.speciesID = pSN->speciesID;
  return specN;
}
}  // namespace

void KIM_Simulator_set_influence_distance(KIM_Simulator * const simulator,
                                          double * const influenceDistance)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->set_influence_distance(influenceDistance);
}

void KIM_Simulator_set_cutoffs(KIM_Simulator * const simulator,
                               int const numberOfCutoffs,
                               double const * const cutoffs)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->set_cutoffs(numberOfCutoffs, cutoffs);
}

// *method functions
void KIM_Simulator_set_reinit(KIM_Simulator * const simulator,
                              KIM_LanguageName const languageName,
                              func * const fptr)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  psimulator->set_reinit(langN, fptr);
}

void KIM_Simulator_set_destroy(KIM_Simulator * const simulator,
                               KIM_LanguageName const languageName,
                               func * const fptr)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  psimulator->set_destroy(langN, fptr);
}

void KIM_Simulator_set_compute_func(KIM_Simulator * const simulator,
                                    KIM_LanguageName const languageName,
                                    func * const fptr)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  psimulator->set_compute_func(langN, fptr);
}

void KIM_Simulator_print(KIM_Simulator const * const simulator)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->print();
}

void KIM_Simulator_get_num_model_species(KIM_Simulator const * const simulator,
                                         int * const numberOfSpecies)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->get_num_model_species(numberOfSpecies);
}

int KIM_Simulator_get_model_species(KIM_Simulator const * const simulator,
                                    int const index,
                                    KIM_SpeciesName * const speciesName)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::SpeciesName specN;
  return psimulator->get_model_species(index, &specN);
  (*speciesName) = makeSpecNameC(specN);
}

// @@@ these will go away
void KIM_Simulator_get_num_sim_species(KIM_Simulator const * const simulator,
                                       int * const numberOfSpecies)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->get_num_sim_species(numberOfSpecies);
}

int KIM_Simulator_get_sim_species(KIM_Simulator const * const simulator,
                                  int const index,
                                  KIM_SpeciesName * const speciesName)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::SpeciesName specN;
  int err = psimulator->get_sim_species(index, &specN);
  (*speciesName) = makeSpecNameC(specN);
  return err;
}

int KIM_Simulator_get_species_code(KIM_Simulator const * const simulator,
                                   KIM_SpeciesName const speciesName,
                                   int * const code)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  return psimulator->get_species_code(makeSpecNameCpp(speciesName), code);
}

// @@@ do we keep this mechanism, make it mandatory, or remove?
int KIM_Simulator_set_species_code(KIM_Simulator * const simulator,
                                   KIM_SpeciesName const speciesName,
                                   int const code)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  return psimulator->set_species_code(makeSpecNameCpp(speciesName), code);
}

int KIM_Simulator_set_parameter(KIM_Simulator * const simulator,
                                int const index, int const extent,
                                void * const ptr)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  return psimulator->set_parameter(index, extent, ptr);
}

int KIM_Simulator_set_parameter_description(KIM_Simulator * const simulator,
                                            int const index,
                                            char const * const description)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  return psimulator->set_parameter_description(index, description);
}

void KIM_Simulator_set_model_buffer(KIM_Simulator * const simulator,
                                    void const * const ptr)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->set_model_buffer(ptr);
}

void KIM_Simulator_get_model_buffer(KIM_Simulator const * const simulator,
                                    void ** const ptr)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  psimulator->get_model_buffer(ptr);
}

//Unit_Handling related routines
int KIM_Simulator_get_unit_handling(KIM_Simulator const * const simulator,
                                    int * const flag)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  return psimulator->get_unit_handling(flag);
}

void KIM_Simulator_get_unit_length(KIM_Simulator const * const simulator,
                                   KIM_LengthUnit * const length)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::LengthUnit lengthU;
  psimulator->get_unit_length(&lengthU);
  *length = makeLengthUnitC(lengthU);
}

void KIM_Simulator_get_unit_energy(KIM_Simulator const * const simulator,
                                   KIM_EnergyUnit * const energy)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::EnergyUnit energyU;
  psimulator->get_unit_energy(&energyU);
  *energy = makeEnergyUnitC(energyU);
}

void KIM_Simulator_get_unit_charge(KIM_Simulator const * const simulator,
                                   KIM_ChargeUnit * const charge)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::ChargeUnit chargeU;
  psimulator->get_unit_charge(&chargeU);
  *charge = makeChargeUnitC(chargeU);
}

void KIM_Simulator_get_unit_temperature(KIM_Simulator const * const simulator,
                                        KIM_TemperatureUnit * const temperature)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::TemperatureUnit temperatureU;
  psimulator->get_unit_temperature(&temperatureU);
  *temperature = makeTemperatureUnitC(temperatureU);
}

void KIM_Simulator_get_unit_time(KIM_Simulator const * const simulator,
                                 KIM_TimeUnit * const time)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  KIM::TimeUnit timeU;
  psimulator->get_unit_time(&timeU);
  *time = makeTimeUnitC(timeU);
}

int KIM_Simulator_convert_to_act_unit(KIM_Simulator const * const simulator,
                                      KIM_LengthUnit const length,
                                      KIM_EnergyUnit const energy,
                                      KIM_ChargeUnit const charge,
                                      KIM_TemperatureUnit const temperature,
                                      KIM_TimeUnit const time,
                                      double const length_exponent,
                                      double const energy_exponent,
                                      double const charge_exponent,
                                      double const temperature_exponent,
                                      double const time_exponent,
                                      double * const factor)
{
  KIM::Simulator * psimulator = (KIM::Simulator *) simulator->p;
  return psimulator->convert_to_act_unit(
      makeLengthUnitCpp(length),
      makeEnergyUnitCpp(energy),
      makeChargeUnitCpp(charge),
      makeTemperatureUnitCpp(temperature),
      makeTimeUnitCpp(time),
      length_exponent,
      energy_exponent,
      charge_exponent,
      temperature_exponent,
      time_exponent,
      factor);
}

}  // extern "C"
