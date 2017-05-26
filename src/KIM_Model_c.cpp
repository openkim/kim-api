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

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif

#ifndef KIM_COMPUTE_HPP_
#include "KIM_Compute.hpp"
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

#ifndef KIM_MODEL_H_
#include "KIM_Model.h"
#endif

#ifndef KIM_COMPUTE_H_
#include "KIM_Compute.h"
#endif

#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif

static KIM::COMPUTE::ArgumentName
makeArgumentNameCpp(KIM_COMPUTE_ArgumentName const argumentName)
{
  return KIM::COMPUTE::ArgumentName(argumentName.argumentID);
}

static KIM_ParameterDataType
makeParameterDataTypeC(KIM::ParameterDataType const dataType)
{
  KIM_ParameterDataType typ;
  KIM_ParameterDataType * pTyp = (KIM_ParameterDataType *) &dataType;
  typ.dataTypeID = pTyp->dataTypeID;
  return typ;
}

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

static KIM::COMPUTE::LanguageName
makeLanguageNameCpp(KIM_COMPUTE_LanguageName const languageName)
{
  return KIM::COMPUTE::LanguageName(languageName.languageID);
}

static KIM_COMPUTE_LanguageName
makeLanguageNameC(KIM::COMPUTE::LanguageName const languageName)
{
  KIM_COMPUTE_LanguageName langN;
  KIM_COMPUTE_LanguageName * pLN = (KIM_COMPUTE_LanguageName*) & languageName;
  langN.languageID = pLN->languageID;
  return langN;
}

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

int KIM_Model_create(char const * const simulatorString,
                     char const * const modelName,
                     KIM_Model ** const model)
{
  std::string sim(simulatorString);
  std::string mod(modelName);
  KIM::Model * pmodel;
  int err = KIM::Model::create(sim, mod, &pmodel);
  (*model) = new KIM_Model;
  (*model)->p = (void *) pmodel;
  return err;
}

void KIM_Model_destroy(KIM_Model ** const model)
{
  KIM::Model * pmodel = (KIM::Model *) (*model)->p;
  KIM::Model::destroy(&pmodel);
  delete (*model);
  *model = 0;
}

void KIM_Model_get_influence_distance(KIM_Model const * const model,
                                      double * const influenceDistance)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_influence_distance(influenceDistance);
}

void KIM_Model_set_influence_distance(KIM_Model * const model,
                                      double * const influenceDistance)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->set_influence_distance(influenceDistance);
}

void KIM_Model_get_cutoffs(KIM_Model const * const model,
                           int * const numberOfCutoffs,
                           double const ** const cutoffs)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_cutoffs(numberOfCutoffs, cutoffs);
}

void KIM_Model_set_cutoffs(KIM_Model * const model, int const numberOfCutoffs,
                           double const * const cutoffs)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->set_cutoffs(numberOfCutoffs, cutoffs);
}

// *data functions
int KIM_Model_get_data(KIM_Model const * const model,
                       KIM_COMPUTE_ArgumentName const argumentName,
                       void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_data(makeArgumentNameCpp(argumentName), ptr);
}

int KIM_Model_set_data(KIM_Model * const model,
                       KIM_COMPUTE_ArgumentName const argumentName,
                       int const extent, void const * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pmodel->set_data(argN, extent, ptr);
}

// *method functions
int KIM_Model_get_method(KIM_Model const * const model,
                         KIM_COMPUTE_ArgumentName const argumentName,
                         KIM_COMPUTE_LanguageName * const languageName,
                         func ** const fptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  KIM::COMPUTE::LanguageName langN;
  return pmodel->get_method(argN, &langN, fptr);
  *languageName = makeLanguageNameC(langN);
}

int KIM_Model_set_method(KIM_Model * const model,
                         KIM_COMPUTE_ArgumentName const argumentName,
                         int const extent,
                         KIM_COMPUTE_LanguageName const languageName,
                         func const * const fptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  KIM::COMPUTE::LanguageName langN = makeLanguageNameCpp(languageName);
  return pmodel->set_method(argN, extent, langN, fptr);
}

// *compute functions
int KIM_Model_get_compute(KIM_Model const * const model,
                          KIM_COMPUTE_ArgumentName const argumentName,
                          int * const flag)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pmodel->get_compute(argN, flag);
}

int KIM_Model_set_compute(KIM_Model * const model,
                          KIM_COMPUTE_ArgumentName const argumentName, int flag)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pmodel->set_compute(argN, flag);
}

int KIM_Model_get_size(KIM_Model const * const model,
                       KIM_COMPUTE_ArgumentName const argumentName,
                       int * const size)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pmodel->get_size(argN, size);
}

void KIM_Model_print(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->print();
}

int KIM_Model_compute(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->compute();
}

int KIM_Model_get_neigh(KIM_Model const * const model,
                        int const neighborListIndex,
                        int const particleNumber,
                        int * const numberOfNeighbors,
                        int const ** const neighborsOfParticle)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_neigh(neighborListIndex, particleNumber, numberOfNeighbors,
                           neighborsOfParticle);
}

int KIM_Model_init(KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  int err = pmodel->init();
  return err;
}

int KIM_Model_reinit(KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->reinit();
}

int KIM_Model_destroy_model(KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->destroy_model();
}

void KIM_Model_get_num_model_species(KIM_Model const * const model,
                                     int * const numberOfSpecies)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_num_model_species(numberOfSpecies);
}

int KIM_Model_get_model_species(KIM_Model const * const model,
                                int const index,
                                KIM_SpeciesName * const speciesName)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::SpeciesName specN;
  return pmodel->get_model_species(index, &specN);
  (*speciesName) = makeSpecNameC(specN);
}

// @@@ these will go away
void KIM_Model_get_num_sim_species(KIM_Model const * const model,
                                   int * const numberOfSpecies)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_num_sim_species(numberOfSpecies);
}

int KIM_Model_get_sim_species(KIM_Model const * const model, int const index,
                              KIM_SpeciesName * const speciesName)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::SpeciesName specN;
  int err = pmodel->get_sim_species(index, &specN);
  (*speciesName) = makeSpecNameC(specN);
  return err;
}

int KIM_Model_get_species_code(KIM_Model const * const model,
                               KIM_SpeciesName const speciesName,
                               int * const code)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_species_code(makeSpecNameCpp(speciesName), code);
}

// @@@ do we keep this mechanism, make it mandatory, or remove?
int KIM_Model_set_species_code(KIM_Model * const model,
                               KIM_SpeciesName const speciesName,
                               int const code)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->set_species_code(makeSpecNameCpp(speciesName), code);
}

void KIM_Model_get_num_params(KIM_Model const * const model,
                              int * const numberOfParameters)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_num_params(numberOfParameters);
}

int KIM_Model_get_parameter_data_type(KIM_Model const * const model,
                                      int const index,
                                      KIM_ParameterDataType * const dataType)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::ParameterDataType typ;
  int err = pmodel->get_parameter_data_type(index, &typ);
  if (err) return err;

  *dataType = makeParameterDataTypeC(typ);
  return err;
}

int KIM_Model_get_parameter(KIM_Model * const model, int const index,
                            int * const extent, void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_parameter(index, extent, ptr);
}

int KIM_Model_set_parameter(KIM_Model * const model, int const index,
                            int const extent, void * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->set_parameter(index, extent, ptr);
}

int KIM_Model_get_parameter_description(KIM_Model const * const model,
                                        int const index,
                                        char const ** const description)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  static std::string str;
  int err = pmodel->get_parameter_description(index, &str);
  if (err) return err;
  *description = str.c_str();
  return err;
}

int KIM_Model_set_parameter_description(KIM_Model * const model,
                                        int const index,
                                        char const * const description)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->set_parameter_description(index, description);
}

void KIM_Model_set_model_buffer(KIM_Model * const model,
                                void const * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->set_model_buffer(ptr);
}

void KIM_Model_get_model_buffer(KIM_Model const * const model,
                                void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_model_buffer(ptr);
}

void KIM_Model_set_sim_buffer(KIM_Model * const model, void const * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->set_sim_buffer(ptr);
}

void KIM_Model_get_sim_buffer(KIM_Model const * const model, void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_sim_buffer(ptr);
}

int KIM_Model_get_model_kim_string(char const * const modelName,
                                   char const ** const kimString)
{
  static std::string kimSTR;
  int err = KIM::Model::get_model_kim_string(modelName, &kimSTR);
  *kimString = kimSTR.c_str();
  return err;
}

int KIM_Model_get_model_kim_string_length(char const * const modelName,
                                          int * const kimStringLength)
{
  std::string kimSTR;
  int err = KIM::Model::get_model_kim_string(modelName, &kimSTR);
  if (err) return err;

  *kimStringLength = kimSTR.length();
  return err;
}


int KIM_Model_process_dEdr(KIM_Model const * const model, double const de,
                           double const r, double const * const dx, int const i,
                           int const j)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->process_dEdr(de, r, dx, i, j);
}

int KIM_Model_process_d2Edr2(KIM_Model const * const model, double const de,
                             double const * const r, double const * const dx,
                             int const * const i, int const * const j)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->process_d2Edr2(de, r, dx, i, j);
}

//Unit_Handling related routines
int KIM_Model_get_unit_handling(KIM_Model const * const model,
                                int * const flag)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_unit_handling(flag);
}

void KIM_Model_get_unit_length(KIM_Model const * const model,
                               KIM_LengthUnit * const length)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::LengthUnit lengthU;
  pmodel->get_unit_length(&lengthU);
  *length = makeLengthUnitC(lengthU);
}

void KIM_Model_get_unit_energy(KIM_Model const * const model,
                               KIM_EnergyUnit * const energy)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::EnergyUnit energyU;
  pmodel->get_unit_energy(&energyU);
  *energy = makeEnergyUnitC(energyU);
}

void KIM_Model_get_unit_charge(KIM_Model const * const model,
                               KIM_ChargeUnit * const charge)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::ChargeUnit chargeU;
  pmodel->get_unit_charge(&chargeU);
  *charge = makeChargeUnitC(chargeU);
}

void KIM_Model_get_unit_temperature(KIM_Model const * const model,
                                    KIM_TemperatureUnit * const temperature)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::TemperatureUnit temperatureU;
  pmodel->get_unit_temperature(&temperatureU);
  *temperature = makeTemperatureUnitC(temperatureU);
}

void KIM_Model_get_unit_time(KIM_Model const * const model,
                             KIM_TimeUnit * const time)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::TimeUnit timeU;
  pmodel->get_unit_time(&timeU);
  *time = makeTimeUnitC(timeU);
}

int KIM_Model_convert_to_act_unit(KIM_Model const * const model,
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
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->convert_to_act_unit(
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
