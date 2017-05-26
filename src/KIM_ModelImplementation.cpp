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

#include <sstream>
#include <iomanip>
#include <cstring>
#include <cstdlib>
#include <cmath>

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#ifndef KIM_MODEL_LIBRARY_HPP_
#include "KIM_ModelLibrary.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_H_
extern "C"
{
#include "KIM_UnitSystem.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_INITIALIZATION_H_
extern "C"
{
#include "KIM_ModelInitialization.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_DRIVER_INITIALIZATION_H_
extern "C"
{
#include "KIM_ModelDriverInitialization.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_REINITIALIZATION_H_
extern "C"
{
#include "KIM_ModelReinitialization.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_COMPUTE_H_
extern "C"
{
#include "KIM_ModelCompute.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_DESTROY_H_
extern "C"
{
#include "KIM_ModelDestroy.h"
}  // extern "C"
#endif


namespace KIM
{
namespace ARGUMENT_NAME
{
extern std::vector<ArgumentName> const mandatoryArguments;
}  // namespace ARGUMENT_NAME

namespace CALL_BACK_NAME
{
extern std::vector<CallBackName> const mandatoryCallBacks;
}  // namespace CALL_BACK_NAME
}  // namespace KIM

namespace
{
KIM_LengthUnit makeLengthUnitC(KIM::LengthUnit const lengthUnit)
{
  KIM_LengthUnit lengthUnitC = {lengthUnit.lengthUnitID};
  return lengthUnitC;
}

KIM_EnergyUnit makeEnergyUnitC(KIM::EnergyUnit const energyUnit)
{
  KIM_EnergyUnit energyUnitC = {energyUnit.energyUnitID};
  return energyUnitC;
}

KIM_ChargeUnit makeChargeUnitC(KIM::ChargeUnit const chargeUnit)
{
  KIM_ChargeUnit chargeUnitC = {chargeUnit.chargeUnitID};
  return chargeUnitC;
}

KIM_TemperatureUnit makeTemperatureUnitC(
    KIM::TemperatureUnit const temperatureUnit)
{
  KIM_TemperatureUnit temperatureUnitC = {temperatureUnit.temperatureUnitID};
  return temperatureUnitC;
}

KIM_TimeUnit makeTimeUnitC(KIM::TimeUnit const timeUnit)
{
  KIM_TimeUnit timeUnitC = {timeUnit.timeUnitID};
  return timeUnitC;
}
}  // namespace

namespace KIM
{
// Forward declarations
class ModelInitialization;
class ModelDriverInitialization;
class ModelReinitialization;
class ModelCompute;
class ModelDestroy;

int ModelImplementation::create(
    Numbering const numbering,
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit,
    std::string const & modelName,
    int * const requestedUnitsAccepted,
    ModelImplementation ** const modelImplementation)
{
  ModelImplementation * pModelImplementation;
  pModelImplementation = new ModelImplementation(new ModelLibrary());

  int error = pModelImplementation->ModelInitialization(
      numbering, requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
      requestedTemperatureUnit, requestedTimeUnit, modelName);
  if (error)
  {
    delete pModelImplementation;
    return true;
  }

  LengthUnit finalLengthUnit;
  EnergyUnit finalEnergyUnit;
  ChargeUnit finalChargeUnit;
  TemperatureUnit finalTemperatureUnit;
  TimeUnit finalTimeUnit;
  pModelImplementation->get_units(&finalLengthUnit, &finalEnergyUnit,
                                  &finalChargeUnit, &finalTemperatureUnit,
                                  &finalTimeUnit);

  if (((finalLengthUnit == LENGTH_UNIT::unused) ||
       (finalLengthUnit == requestedLengthUnit))
      &&
      ((finalEnergyUnit == ENERGY_UNIT::unused) ||
       (finalEnergyUnit == requestedEnergyUnit))
      &&
      ((finalChargeUnit == CHARGE_UNIT::unused) ||
       (finalChargeUnit == requestedChargeUnit))
      &&
      ((finalTemperatureUnit == TEMPERATURE_UNIT::unused) ||
       (finalTemperatureUnit == requestedTemperatureUnit))
      &&
      ((finalTimeUnit == TIME_UNIT::unused) ||
       (finalTimeUnit == requestedTimeUnit)))
  {
    *requestedUnitsAccepted = true;
  }
  else
  {
    *requestedUnitsAccepted = false;
  }

  *modelImplementation = pModelImplementation;

  return false;
}


void ModelImplementation::destroy(
    ModelImplementation ** const modelImplementation)
{

  int error = (*modelImplementation)->ModelDestroy();
  if (error)
    ;// @@@@@ log message

  delete *modelImplementation;
  *modelImplementation = 0;
}

void ModelImplementation::set_influence_distance(
    double const * const influenceDistance)
{
  influenceDistance_ = influenceDistance;
}
void ModelImplementation::get_influence_distance(
    double * const influenceDistance) const
{
  *influenceDistance = *influenceDistance_;
}


void ModelImplementation::set_cutoffs(
    int const numberOfCutoffs, double const * const cutoffs)
{
  numberOfCutoffs_ = numberOfCutoffs;
  cutoffs_ = cutoffs;
}

// allows NULL as value of cutoffs (to get just numberOfCutoffs)
void ModelImplementation::get_cutoffs(int * const numberOfCutoffs,
                                      double const ** const cutoffs) const
{
  *numberOfCutoffs = numberOfCutoffs_;
  *cutoffs = cutoffs_;
}


int ModelImplementation::set_reinit(LanguageName const languageName,
                                    func * const fptr)
{
  reinitializationLanguage_ = languageName;
  reinitializationFunction_ = fptr;

  return false;
}

int ModelImplementation::set_destroy(LanguageName const languageName,
                                     func * const fptr)
{
  destroyLanguage_ = languageName;
  destroyFunction_ = fptr;

  return false;
}

int ModelImplementation::set_compute_func(LanguageName const languageName,
                                          func * const fptr)
{
  computeLanguage_ = languageName;
  computeFunction_ = fptr;

  return false;
}


int ModelImplementation::set_species_code(SpeciesName const speciesName,
                                          int const code)
{
  supportedSpecies_[speciesName] = code;

  return false;
}

int ModelImplementation::get_species_support_and_code(
    KIM::SpeciesName const speciesName,
    int * const speciesIsSupported,
    int * const code) const
{
  auto result = supportedSpecies_.find(speciesName);

  if (result == supportedSpecies_.end())
  {
    *speciesIsSupported = false;
  }
  else
  {
    *speciesIsSupported = true;
    *code = result->second;
  }

  return false;
}


int ModelImplementation::set_argument_attribute(ArgumentName const argumentName,
                                                Attribute const attribute)
{
  argumentAttribute_[argumentName] = attribute;

  return false;
}

int ModelImplementation::get_argument_attribute(ArgumentName const argumentName,
                                                Attribute * const attribute)
    const
{
  auto result = argumentAttribute_.find(argumentName);

  if (result == argumentAttribute_.end())
  {
    return true;
  }
  else
  {
    *attribute = result->second;
    return false;
  }
}


int ModelImplementation::set_call_back_attribute(
    CallBackName const callBackName,
    Attribute const attribute)
{
  callBackAttribute_[callBackName] = attribute;

  return false;
}

int ModelImplementation::get_call_back_attribute(
    CallBackName const callBackName,
    Attribute * const attribute) const
{
  auto result = callBackAttribute_.find(callBackName);

  if (result == callBackAttribute_.end())
  {
    return true;
  }
  else
  {
    *attribute = result->second;
    return false;
  }
}


int ModelImplementation::set_model_numbering(Numbering const numbering)
{
  modelNumbering_ = numbering;

  return false;
}

int ModelImplementation::set_simulator_numbering(Numbering const numbering)
{
  simulatorNumbering_ = numbering;
  return false;
}


int ModelImplementation::set_units(LengthUnit const lengthUnit,
                                   EnergyUnit const energyUnit,
                                   ChargeUnit const chargeUnit,
                                   TemperatureUnit const temperatureUnit,
                                   TimeUnit const timeUnit)
{
  lengthUnit_ = lengthUnit;
  energyUnit_ = energyUnit;
  chargeUnit_ = chargeUnit;
  temperatureUnit_ = temperatureUnit;
  timeUnit_ = timeUnit;

  return false;
}

void ModelImplementation::get_units(LengthUnit * const lengthUnit,
                                    EnergyUnit * const energyUnit,
                                    ChargeUnit * const chargeUnit,
                                    TemperatureUnit * const temperatureUnit,
                                    TimeUnit * const timeUnit) const
{
  *lengthUnit = lengthUnit_;
  *energyUnit = energyUnit_;
  *chargeUnit = chargeUnit_;
  *temperatureUnit = temperatureUnit_;
  *timeUnit = timeUnit_;
}


int ModelImplementation::get_number_of_parameter_files(
    int * const numberOfParameterFiles) const
{
  if (modelType_ != ModelLibrary::ITEM_TYPE::PARAMETERIZED_MODEL) return true;

  *numberOfParameterFiles = numberOfParameterFiles_;
  return false;
}

int ModelImplementation::get_parameter_file_name(
    int const index, std::string * const parameterFileName) const
{
  if (modelType_ != ModelLibrary::ITEM_TYPE::PARAMETERIZED_MODEL) return true;

  if ((index < 0) || (index >= numberOfParameterFiles_))
  {
    //@@@@ log bad index
    return true;
  }

  *parameterFileName = parameterFileNames_[index];
  return false;
}

int ModelImplementation::set_parameter(int const extent, int * const ptr,
                                       std::string const & description)
{
  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Integer);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  return false;
}

int ModelImplementation::set_parameter(int const extent, double * const ptr,
                                       std::string const & description)
{
  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Double);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  return false;
}

void ModelImplementation::get_num_params(int * const numberOfParameters) const
{
  *numberOfParameters = parameterPointer_.size();
}

int ModelImplementation::get_parameter_data_type_and_description(
    int const index, DataType * const dataType,
    std::string * const description) const
{
  *dataType = parameterDataType_[index];
  *description = parameterDescription_[index];

  return false;
}

int ModelImplementation::get_parameter_extent_and_pointer(
    int const index, int * extent, int ** const ptr)
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<int *>(parameterPointer_[index]);

  return false;
}

int ModelImplementation::get_parameter_extent_and_pointer(
    int const index, int * extent, int const ** const ptr) const
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<int const *>(parameterPointer_[index]);

  return false;
}

int ModelImplementation::get_parameter_extent_and_pointer(
    int const index, int * extent, double ** const ptr)
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<double *>(parameterPointer_[index]);

  return false;
}

int ModelImplementation::get_parameter_extent_and_pointer(
    int const index, int * extent, double const ** const ptr) const
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<double const *>(parameterPointer_[index]);

  return false;
}


int ModelImplementation::set_data(ArgumentName const argumentName,
                                  int const * const ptr)
{
  argumentPointer_[argumentName]
      = reinterpret_cast<void *>(const_cast<int *>(ptr));

  return false;
}

int ModelImplementation::set_data(ArgumentName const argumentName,
                                  double const * const ptr)
{
  argumentPointer_[argumentName]
      = reinterpret_cast<void *>(const_cast<double *>(ptr));

  return false;
}

int ModelImplementation::get_data(ArgumentName const argumentName,
                                  int const ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    return false;
  }
  else
  {
    *ptr = reinterpret_cast<int const *>(result->second);
    return false;
  }
}

int ModelImplementation::get_data(ArgumentName const argumentName,
                                  int ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    return false;
  }
  else
  {
    *ptr = reinterpret_cast<int *>(result->second);
    return false;
  }
}

int ModelImplementation::get_data(ArgumentName const argumentName,
                                  double const ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    return false;
  }
  else
  {
    *ptr = reinterpret_cast<double const *>(result->second);
    return false;
  }
}

int ModelImplementation::get_data(ArgumentName const argumentName,
                                  double ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    return false;
  }
  else
  {
    *ptr = reinterpret_cast<double *>(result->second);
    return false;
  }
}



int ModelImplementation::set_call_back(CallBackName const callBackName,
                                       LanguageName const languageName,
                                       func * const fptr,
                                       void const * const dataObject)
{
  auto result = callBackAttribute_.find(callBackName);

  if ((result == callBackAttribute_.end())
      ||
      (result->second == ATTRIBUTE::notSupported))
  {
    return true;
  }
  else
  {
    callBackLanguage_[callBackName] = languageName;
    callBackFunctionPointer_[callBackName] = fptr;
    callBackDataObjectPointer_[callBackName] = dataObject;
    return false;
  }
}

int ModelImplementation::is_call_back_present(
    CallBackName const callBackName, int * const present) const
{
  auto result = callBackFunctionPointer_.find(callBackName);

  if ((result == callBackFunctionPointer_.end())
      ||
      (result->second == 0))
  {
    *present = false;
    return false;
  }
  else
  {
    *present = true;
    return false;
  }
}


int ModelImplementation::compute() const
{
  typedef int ModelComputeCpp(KIM::ModelCompute * const);
  ModelComputeCpp * CppCompute
      = reinterpret_cast<ModelComputeCpp *>(computeFunction_);
  typedef int ModelComputeC(KIM_ModelCompute * const);
  ModelComputeC * CCompute
      = reinterpret_cast<ModelComputeC *>(computeFunction_);
  typedef void ModelComputeF(KIM_ModelCompute * const, int * const);
  ModelComputeF * FCompute
      = reinterpret_cast<ModelComputeF *>(computeFunction_);

  int error;
  struct Mdl {void const * p;};
  Mdl M;
  M.p = this;
  if (computeLanguage_ == LANGUAGE_NAME::Cpp)
  {
    error = CppCompute(reinterpret_cast<KIM::ModelCompute *>(&M));
  }
  else if (computeLanguage_ == LANGUAGE_NAME::C)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    error = CCompute(&cM);
  }
  else if (computeLanguage_ == LANGUAGE_NAME::Fortran)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    FCompute(&cM, &error);
  }
  else
  {
    return true;
  }

  if (error)
    return true;
  else
    return false;
}

int ModelImplementation::ClearInfluenceDistanceAndCutoffsThenReinitializeModel()
{
  influenceDistance_ = 0;
  numberOfCutoffs_ = 0;
  cutoffs_ = 0;

  typedef int ModelReinitializationCpp(KIM::ModelReinitialization * const);
  ModelReinitializationCpp * CppReinitialization
      = reinterpret_cast<ModelReinitializationCpp *>(reinitializationFunction_);
  typedef int ModelReinitializationC(KIM_ModelReinitialization * const);
  ModelReinitializationC * CReinitialization
      = reinterpret_cast<ModelReinitializationC *>(reinitializationFunction_);
  typedef void ModelReinitializationF(KIM_ModelReinitialization * const,
                                      int * const);
  ModelReinitializationF * FReinitialization
      = reinterpret_cast<ModelReinitializationF *>(reinitializationFunction_);

  int error;
  struct Mdl {void * p;};
  Mdl M;
  M.p = this;
  if (reinitializationLanguage_ == LANGUAGE_NAME::Cpp)
  {
    error = CppReinitialization(
        reinterpret_cast<KIM::ModelReinitialization *>(&M));
  }
  else if (reinitializationLanguage_ == LANGUAGE_NAME::C)
  {
    KIM_ModelReinitialization cM;
    cM.p = &M;
    error = CReinitialization(&cM);
  }
  else if (reinitializationLanguage_ == LANGUAGE_NAME::Fortran)
  {
    KIM_ModelReinitialization cM;
    cM.p = &M;
    FReinitialization(&cM, &error);
  }
  else
  {
    return true;
  }

  if (error)
    return true;
  else
    return false;
}

int ModelImplementation::get_neigh(int const neighborListIndex,
                                   int const particleNumber,
                                   int * const numberOfNeighbors,
                                   int const ** const neighborsOfParticle)
    const
{
  auto languageResult = callBackLanguage_.find(CALL_BACK_NAME::get_neigh);
  if (languageResult == callBackLanguage_.end())
  {
    // @@@@ log message
    return true;
  }
  LanguageName languageName = languageResult->second;
  void const * dataObject
      = (callBackDataObjectPointer_.find(CALL_BACK_NAME::get_neigh))->second;

  func * functionPointer
      = (callBackFunctionPointer_.find(CALL_BACK_NAME::get_neigh))->second;
  typedef int get_NeighCpp(void const * const dataObject,
                           int const neighborListIndex,
                           int const particleNumber,
                           int * const numberOfNeighbors,
                           int const ** const neighborsOfParticle);
  get_NeighCpp * CppGet_Neigh
      = reinterpret_cast<get_NeighCpp *>(functionPointer);
  typedef int get_NeighC(void const * const dataObject,
                         int const neighborListIndex,
                         int const particleNumber,
                         int * const numberOfNeighbors,
                         int const ** const neighborsOfParticle);
  get_NeighC * CGet_Neigh = reinterpret_cast<get_NeighC *>(functionPointer);
  typedef void get_NeighF(void const * const dataObject,
                          int const neighborListIndex,
                          int const particleNumber,
                          int * const numberOfNeighbors,
                          int const ** const neighborsOfParticle,
                          int * const ierr);
  get_NeighF * FGet_Neigh = reinterpret_cast<get_NeighF *>(functionPointer);


  int simulatorParticleNumber = particleNumber +
      ((simulatorNumbering_ == modelNumbering_) ? 0 : -numberingOffset_);
  int const * simulatorNeighborsOfParticle;
  int error;
  if (languageName == LANGUAGE_NAME::Cpp)
  {
    error = CppGet_Neigh(dataObject, neighborListIndex,
                         simulatorParticleNumber, numberOfNeighbors,
                         &simulatorNeighborsOfParticle);
  }
  else if (languageName == LANGUAGE_NAME::C)
  {
    error = CGet_Neigh(dataObject, neighborListIndex, simulatorParticleNumber,
                       numberOfNeighbors, &simulatorNeighborsOfParticle);
  }
  else if (languageName == LANGUAGE_NAME::Fortran)
  {
    FGet_Neigh(dataObject, neighborListIndex+1, simulatorParticleNumber,
               numberOfNeighbors, &simulatorNeighborsOfParticle, &error);
  }
  else
  {
    return true;
  }

  if (error) return true;

  // account for numbering differences if needed
  if (simulatorNumbering_ != modelNumbering_)
  {
    std::vector<int> & list = getNeighborListStorage_[neighborListIndex];
    list.resize(*numberOfNeighbors);
    for (int i=0; i<*numberOfNeighbors; ++i)
      list[i] = simulatorNeighborsOfParticle[i] + numberingOffset_;

    *neighborsOfParticle = list.data();
  }
  else
  {
    *neighborsOfParticle = simulatorNeighborsOfParticle;
  }

  return false;
}

int ModelImplementation::process_dEdr(double const de, double const r,
                                      double const * const dx,
                                      int const i, int const j) const
{
  auto languageResult = callBackLanguage_.find(CALL_BACK_NAME::process_dEdr);
  if (languageResult == callBackLanguage_.end())
  {
    // @@@@ log message
    return true;
  }
  LanguageName languageName = languageResult->second;
  void const * dataObject
      = (callBackDataObjectPointer_.find(CALL_BACK_NAME::process_dEdr))->second;

  func * functionPointer
      = (callBackFunctionPointer_.find(CALL_BACK_NAME::process_dEdr))->second;
  typedef int process_dEdrCpp(void const * const dataObject, double const de,
                              double const r, double const * const dx,
                              int const i, int const j);
  process_dEdrCpp * CppProcess_dEdr
      = reinterpret_cast<process_dEdrCpp *>(functionPointer);
  typedef int process_dEdrC(void const * const dataObject, double const de,
                            double const r, double const * const dx,
                            int const i, int const j);
  process_dEdrC * CProcess_dEdr
      = reinterpret_cast<process_dEdrC *>(functionPointer);
  typedef void process_dEdrF(void const * const dataObject, double const de,
                             double const r, double const * const dx,
                             int const i, int const j, int * const ierr);
  process_dEdrF * FProcess_dEdr
      = reinterpret_cast<process_dEdrF *>(functionPointer);

  int error;
  if (languageName == LANGUAGE_NAME::Cpp)
  {
    error = CppProcess_dEdr(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::C)
  {
    error = CProcess_dEdr(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::Fortran)
  {
    FProcess_dEdr(dataObject, de, r, dx, i, j, &error);
  }
  else
  {
    return true;
  }

  if (error)
    return true;
  else
    return false;
}

int ModelImplementation::process_d2Edr2(double const de, double const * const r,
                                        double const * const dx,
                                        int const * const i,
                                        int const * const j) const
{
  auto languageResult = callBackLanguage_.find(CALL_BACK_NAME::process_d2Edr2);
  if (languageResult == callBackLanguage_.end())
  {
    // @@@@ log message
    return true;
  }
  LanguageName languageName = languageResult->second;
  void const * dataObject = (callBackDataObjectPointer_
                             .find(CALL_BACK_NAME::process_d2Edr2))->second;

  func * functionPointer
      = (callBackFunctionPointer_.find(CALL_BACK_NAME::process_d2Edr2))->second;
  typedef int process_d2Edr2Cpp(void const * const dataObject, double const de,
                                double const * const r, double const * const dx,
                                int const * const i, int const * const j);
  process_d2Edr2Cpp * CppProcess_d2Edr2
      = reinterpret_cast<process_d2Edr2Cpp *>(functionPointer);
  typedef int process_d2Edr2C(void const * const dataObject, double const de,
                              double const * const r, double const * const dx,
                              int const * const i, int const * const j);
  process_d2Edr2C * CProcess_d2Edr2
      = reinterpret_cast<process_d2Edr2C *>(functionPointer);
  typedef void process_d2Edr2F(void const * const dataObject, double const de,
                               double const * const r, double const * const dx,
                               int const * const i, int const * const j,
                               int * const ierr);
  process_d2Edr2F * FProcess_d2Edr2
      = reinterpret_cast<process_d2Edr2F *>(functionPointer);

  int error;
  if (languageName == LANGUAGE_NAME::Cpp)
  {
    error = CppProcess_d2Edr2(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::C)
  {
    error = CProcess_d2Edr2(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::Fortran)
  {
    FProcess_d2Edr2(dataObject, de, r, dx, i, j, &error);
  }
  else
  {
    return true;
  }

  if (error)
    return true;
  else
    return false;
}

void ModelImplementation::set_model_buffer(void * const ptr)
{
  modelBuffer_ = ptr;
}

void ModelImplementation::get_model_buffer(void ** const ptr) const
{
  *ptr = modelBuffer_;
}


void ModelImplementation::set_sim_buffer(void * const ptr)
{
  simulatorBuffer_ = ptr;
}

void ModelImplementation::get_sim_buffer(void ** const ptr) const
{
  *ptr = simulatorBuffer_;
}


int ModelImplementation::convert_unit(
    LengthUnit const fromLengthUnit,
    EnergyUnit const fromEnergyUnit,
    ChargeUnit const fromChargeUnit,
    TemperatureUnit const fromTemperatureUnit,
    TimeUnit const fromTimeUnit,
    LengthUnit const toLengthUnit,
    EnergyUnit const toEnergyUnit,
    ChargeUnit const toChargeUnit,
    TemperatureUnit const toTemperatureUnit,
    TimeUnit const toTimeUnit,
    double const lengthExponent,
    double const energyExponent,
    double const chargeExponent,
    double const temperatureExponent,
    double const timeExponent,
    double * const conversionFactor) const
{
  static std::unordered_map<LengthUnit const, double>
      lengthConvertToSI =
      {
        std::pair<LengthUnit const, double>(LENGTH_UNIT::A,
                                            1.0e-10),
        std::pair<LengthUnit const, double>(LENGTH_UNIT::Bohr,
                                            5.291772109217171e-11),
        std::pair<LengthUnit const, double>(LENGTH_UNIT::cm,
                                            1.0e-2),
        std::pair<LengthUnit const, double>(LENGTH_UNIT::m,
                                            1.0),
        std::pair<LengthUnit const, double>(LENGTH_UNIT::nm,
                                            1.0e-9)
      };

  static std::unordered_map<EnergyUnit const, double>
      energyConvertToSI =
      {
        std::pair<EnergyUnit const, double>(ENERGY_UNIT::amu_A2_per_ps2,
                                            1.66053886e-23),
        std::pair<EnergyUnit const, double>(ENERGY_UNIT::erg,
                                            1.0e7),
        std::pair<EnergyUnit const, double>(ENERGY_UNIT::eV,
                                            1.60217646e-19),
        std::pair<EnergyUnit const, double>(ENERGY_UNIT::Hartree,
                                            4.3597439422e-18),
        std::pair<EnergyUnit const, double>(ENERGY_UNIT::J,
                                            1.0),
        std::pair<EnergyUnit const, double>(ENERGY_UNIT::kcal_mol,
                                            6.9477e-21)
      };

  static std::unordered_map<ChargeUnit const, double>
      chargeConvertToSI =
      {
        std::pair<ChargeUnit const, double>(CHARGE_UNIT::C,
                                            1.0),
        std::pair<ChargeUnit const, double>(CHARGE_UNIT::e,
                                            1.602e-19),
        std::pair<ChargeUnit const, double>(CHARGE_UNIT::statC,
                                            2.99792458e-9)
      };

  static std::unordered_map<TemperatureUnit const, double>
      temperatureConvertToSI =
      {
        std::pair<TemperatureUnit const, double>(TEMPERATURE_UNIT::K,
                                                 1.0)
      };

  static std::unordered_map<TimeUnit const, double>
      timeConvertToSI =
      {
        std::pair<TimeUnit const, double>(TIME_UNIT::fs, 1.0e-15),
        std::pair<TimeUnit const, double>(TIME_UNIT::ps, 1.0e-12),
        std::pair<TimeUnit const, double>(TIME_UNIT::ns, 1.0e-9),
        std::pair<TimeUnit const, double>(TIME_UNIT::s, 1.0)
      };


  double const lengthConversion
      = lengthConvertToSI[toLengthUnit]/lengthConvertToSI[fromLengthUnit];
  double const energyConversion
      = energyConvertToSI[toEnergyUnit]/energyConvertToSI[fromEnergyUnit];
  double const chargeConversion
      = chargeConvertToSI[toChargeUnit]/chargeConvertToSI[fromChargeUnit];
  double const temperatureConversion
      = temperatureConvertToSI[toTemperatureUnit]
      /temperatureConvertToSI[fromTemperatureUnit];
  double const timeConversion
      = timeConvertToSI[toTimeUnit]/timeConvertToSI[fromTimeUnit];

  *conversionFactor
      = pow(lengthConversion, lengthExponent)
      * pow(energyConversion, energyExponent)
      * pow(chargeConversion, chargeExponent)
      * pow(temperatureConversion, temperatureExponent)
      * pow(timeConversion, timeExponent);

  return false;
}

void ModelImplementation::SetLogID(std::string const & logID)
{
  //@@@@@ do nothing for now
}

void ModelImplementation::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  //@@@@@ do nothing for now
}

void ModelImplementation::PopLogVerbosity()
{
  //@@@@@ do nothing for now
}

void ModelImplementation::Log(LogVerbosity const logVerbosity,
                              std::string const & message,
                              int const lineNumber,
                              std::string const & fileName) const
{
  KIM::Log(logVerbosity, message, lineNumber, fileName);
}

std::string ModelImplementation::string() const
{
  std::stringstream ss;
  ss << std::setprecision(10) << std::scientific << std::left;
  ss <<
      "====================================================================="
      "===========\n\n";

  ss << "Model Name : " << modelName_ << "\n";
  if (modelType_ == ModelLibrary::ITEM_TYPE::PARAMETERIZED_MODEL)
  {
    ss << "Model Driver Name : " << modelDriverName_ << "\n";
  }
  ss << "\n";

  ss << "Model Supplied Functions :\n"
     << "\t" << std::setw(25) << "Function Name"
     << std::setw(10) << "Language"
     << std::setw(15) << "Pointer"
     << "\n"
     << "\t" << std::setw(25) << "-------------------------"
     << std::setw(10) << "----------"
     << std::setw(15) << "---------------"
     << "\n\n"
     << "\t"
     << std::setw(25) << "Reinitialization"
     << std::setw(10) << reinitializationLanguage_.string()
     << std::setw(15) << (void *) reinitializationFunction_
     << "\n"
     << "\t"
     << std::setw(25) << "Destroy"
     << std::setw(10) << destroyLanguage_.string()
     << std::setw(15) << (void *) destroyFunction_
     << "\n"
     << "\t"
     << std::setw(25) << "Compute"
     << std::setw(10) << computeLanguage_.string()
     << std::setw(15) << (void *) computeFunction_
     << "\n\n";

  ss << "Numbering : " << modelNumbering_.string() << "\n\n";

  ss << "Units : \n"
      "\tLength Unit      : " << lengthUnit_.string() << "\n"
      "\tEnergy Unit      : " << energyUnit_.string() << "\n"
      "\tCharge Unit      : " << chargeUnit_.string() << "\n"
      "\tTemperature Unit : " << temperatureUnit_.string() << "\n"
      "\tTime Unit        : " << timeUnit_.string() << "\n\n";

  ss << "Influence Distance : " << *influenceDistance_ << "\n\n";

  ss << "Number Of Cutoffs : " << numberOfCutoffs_ << "\n";
  ss << "Cutoffs :\n";
  for (int i=0; i<numberOfCutoffs_; ++i)
  {
    ss << "\t" << i << " : " << cutoffs_[i] << "\n";
  }
  ss << "\n\n";

  ss << "Supported Species :\n";
  int const specWidth = 10;
  ss << "\t" << std::setw(specWidth) << "Species" << std::setw(specWidth)
     << "Code" << "\n";
  ss << "\t" << std::setw(specWidth) << "----------" << std::setw(specWidth)
     << "----------" << "\n\n";
  for (auto spec = supportedSpecies_.begin();
       spec != supportedSpecies_.end();
       ++spec)
  {
    ss << "\t" << std::setw(specWidth) << (spec->first).string()
       << std::setw(specWidth) << spec->second << "\n";
  }
  ss << "\n";

  ss << "Compute Arguments :\n";
  int const argW = 25;
  ss << "\t" << std::setw(argW) << "Argument Name"
     << std::setw(argW) << "Attribute"
     << std::setw(argW) << "Pointer"
     << "\n";
  ss << "\t" << std::setw(argW) << "-------------------------"
     << std::setw(argW) << "-------------------------"
     << std::setw(argW) << "-------------------------"
     << "\n\n";
  for (auto argName = argumentAttribute_.begin();
       argName != argumentAttribute_.end();
       ++argName)
  {
    ss << "\t" << std::setw(argW) << (argName->first).string()
       << std::setw(argW) << (argName->second).string();

    if ((argName->second) != ATTRIBUTE::notSupported)
    {
      auto ptr = argumentPointer_.find(argName->first);
      if (ptr != argumentPointer_.end())
      {
        ss << std::setw(argW) << (void *) ptr->second;
      }
      else
      {
        ss << std::setw(argW) << "Not Set";
      }
    }
    else
    {
      ss << std::setw(argW) << "N/A";
    }
    ss << "\n";
  }
  ss << "\n";


  ss << "Compute Callbacks :\n";
  int const cbW = 25;
  ss << "\t" << std::setw(cbW) << "Callback Name"
     << std::setw(cbW) << "Attribute"
     << std::setw(cbW) << "Language"
     << std::setw(cbW) << "Function Pointer"
     << std::setw(cbW) << "Data Pointer"
     << "\n";
  ss << "\t" << std::setw(cbW) << "-------------------------"
     << std::setw(cbW) << "-------------------------"
     << std::setw(cbW) << "-------------------------"
     << std::setw(cbW) << "-------------------------"
     << std::setw(cbW) << "-------------------------"
     << "\n\n";
  for (auto cbName = callBackAttribute_.begin();
       cbName != callBackAttribute_.end();
       ++cbName)
  {
    ss << "\t" << std::setw(cbW) << (cbName->first).string()
       << std::setw(cbW) << (cbName->second).string();

    if ((cbName->second) != ATTRIBUTE::notSupported)
    {
      auto ptr = callBackLanguage_.find(cbName->first);
      if (ptr != callBackLanguage_.end())
      {
        ss << std::setw(cbW) << (ptr->second).string();
        auto ptr2 = callBackFunctionPointer_.find(cbName->first);
        ss << std::setw(cbW) << (void *) ptr2->second;
        auto ptr3 = callBackDataObjectPointer_.find(cbName->first);
        ss << std::setw(cbW) << (void *) ptr3->second;
      }
      else
      {
        ss << std::setw(cbW) << "Not Set";
      }
    }
    else
    {
      ss << std::setw(cbW) << "N/A";
    }
    ss << "\n";
  }
  ss << "\n";


  int numberOfParameters = parameterPointer_.size();
  ss << "Number Of Prameters : " << numberOfParameters << "\n";
  ss << "\t" << std::setw(8) << "index"
     << std::setw(10) << "Data Type"
     << std::setw(10) << "Extent"
     << std::setw(15) << "Pointer"
     << "Description"
     << "\n";
  ss << "\t" << std::setw(8) << "--------"
     << std::setw(10) << "----------"
     << std::setw(10) << "----------"
     << std::setw(15) << "---------------"
     << "-------------------------"
     << "\n\n";
  for (int i=0; i<numberOfParameters; ++i)
  {
    ss << "\t" << std::setw(8) << i
       << std::setw(10) << parameterDataType_[i].string()
       << std::setw(10) << parameterExtent_[i]
       << std::setw(15) << (void *) parameterPointer_[i]
       << parameterDescription_[i]
       << "\n";
  }
  ss << "\n";


  ss <<
      "====================================================================="
      "===========\n";
  return ss.str();
}


ModelImplementation::ModelImplementation(ModelLibrary * const modelLibrary) :
    modelLibrary_(modelLibrary),
    influenceDistance_(0),
    numberOfCutoffs_(0),
    cutoffs_(0),
    reinitializationFunction_(0),
    destroyFunction_(0),
    computeFunction_(0),
    modelBuffer_(0),
    simulatorBuffer_(0)
{
  // populate Arguments
  int numberOfArguments;
  ARGUMENT_NAME::get_number_of_arguments(&numberOfArguments);
  for (int i=0; i<numberOfArguments; ++i)
  {
    ArgumentName argumentName;
    ARGUMENT_NAME::get_argument_name(i, &argumentName);
    argumentAttribute_[argumentName] = ATTRIBUTE::notSupported;
  }
  // populate mandatory Arguments
  for (auto mandatoryArgument = ARGUMENT_NAME::mandatoryArguments.begin();
       mandatoryArgument != ARGUMENT_NAME::mandatoryArguments.end();
       ++mandatoryArgument)
  {
    argumentAttribute_[*mandatoryArgument] = ATTRIBUTE::mandatory;
  }

  // populate CallBacks
  int numberOfCallBacks;
  CALL_BACK_NAME::get_number_of_call_backs(&numberOfCallBacks);
  for (int i=0; i<numberOfCallBacks; ++i)
  {
    CallBackName callBackName;
    CALL_BACK_NAME::get_call_back_name(i, &callBackName);
    callBackAttribute_[callBackName] = ATTRIBUTE::notSupported;
  }
  // populate mandatory CallBacks
  for (auto mandatoryCallBack = CALL_BACK_NAME::mandatoryCallBacks.begin();
       mandatoryCallBack != CALL_BACK_NAME::mandatoryCallBacks.end();
       ++mandatoryCallBack)
  {
    callBackAttribute_[*mandatoryCallBack] = ATTRIBUTE::mandatory;
  }
}

ModelImplementation::~ModelImplementation()
{
  delete modelLibrary_;
}

int ModelImplementation::ModelInitialization(
    Numbering const numbering,
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit,
    std::string const & modelName)
{
  modelName_ = modelName;

  int error = set_simulator_numbering(numbering);
  if (error) return true;

  error = modelLibrary_->open(true, modelName);
  if (error) return true;

  error = modelLibrary_->getModelType(&modelType_);
  switch (modelType_)
  {
    case ModelLibrary::ITEM_TYPE::STAND_ALONE_MODEL:
      error = InitializeStandAloneModel(requestedLengthUnit,
                                        requestedEnergyUnit,
                                        requestedChargeUnit,
                                        requestedTemperatureUnit,
                                        requestedTimeUnit);
      if (error)
      {
        //@@@@ log error
        return true;
      }
      break;
    case ModelLibrary::ITEM_TYPE::PARAMETERIZED_MODEL:
      error = InitializeParameterizedModel(requestedLengthUnit,
                                           requestedEnergyUnit,
                                           requestedChargeUnit,
                                           requestedTemperatureUnit,
                                           requestedTimeUnit);
      if (error)
      {
        //@@@@ log error
        return true;
      }
      break;
    case ModelLibrary::ITEM_TYPE::MODEL_DRIVER:
      // @@@@@ log error
      return true;
      break;
    case ModelLibrary::ITEM_TYPE::SIMULATOR_MODEL:
      // @@@@@ log error
      return true;
      break;
    default:
      // @@@@ log error
      return true;
  }

  // set numberingOffset_
  if (simulatorNumbering_ == modelNumbering_)
    numberingOffset_ = 0;
  else if (simulatorNumbering_ == NUMBERING::zeroBased)
    numberingOffset_ = 1;
  else
    numberingOffset_ = -1;

  // resize getNeighborListStorage_
  if (simulatorNumbering_ != modelNumbering_)
    getNeighborListStorage_.resize(numberOfCutoffs_);

  return false;
}

int ModelImplementation::ModelDestroy()
{
  typedef int ModelDestroyCpp(KIM::ModelDestroy * const);
  ModelDestroyCpp * CppDestroy
      = reinterpret_cast<ModelDestroyCpp *>(destroyFunction_);
  typedef int ModelDestroyC(KIM_ModelDestroy * const);
  ModelDestroyC * CDestroy
      = reinterpret_cast<ModelDestroyC *>(destroyFunction_);
  typedef void ModelDestroyF(KIM_ModelDestroy * const, int * const);
  ModelDestroyF * FDestroy
      = reinterpret_cast<ModelDestroyF *>(destroyFunction_);

  int error;
  struct Mdl {void * p;};
  Mdl M;
  M.p = this;
  if (destroyLanguage_ == LANGUAGE_NAME::Cpp)
  {
    error = CppDestroy(reinterpret_cast<KIM::ModelDestroy *>(&M));
  }
  else if (destroyLanguage_ == LANGUAGE_NAME::C)
  {
    KIM_ModelDestroy cM;
    cM.p = &M;
    error = CDestroy(&cM);
  }
  else if (destroyLanguage_ == LANGUAGE_NAME::Fortran)
  {
    KIM_ModelDestroy cM;
    cM.p = &M;
    FDestroy(&cM, &error);
  }
  else
  {
    return true;
  }

  if (error)
    return true;
  else
    return false;
}

int ModelImplementation::InitializeStandAloneModel(
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit)
{
  LanguageName languageName;
  func * functionPointer = 0;
  int error = modelLibrary_->getModelInitializationFunctionPointer(
      &languageName, &functionPointer);
  if (error) return true;

  typedef int ModelInitializationCpp(
      KIM::ModelInitialization * const modelInitialization,
      LengthUnit const requestedLengthUnit,
      EnergyUnit const requestedEnergyUnit,
      ChargeUnit const requestedChargeUnit,
      TemperatureUnit const requestedTemperatureUnit,
      TimeUnit const requestedTimeUnit);
  ModelInitializationCpp * CppInitialization
      = reinterpret_cast<ModelInitializationCpp *>(functionPointer);
  typedef int ModelInitializationC(
      KIM_ModelInitialization * const modelInitialization,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit);
  ModelInitializationC * CInitialization
      = reinterpret_cast<ModelInitializationC *>(functionPointer);
  typedef void ModelInitializationF(
      KIM_ModelInitialization * const modelInitialization,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit,
      int * const);
  ModelInitializationF * FInitialization
      = reinterpret_cast<ModelInitializationF *>(functionPointer);

  struct Mdl {void * p;};
  Mdl M;
  M.p = this;
  KIM_LengthUnit requestedLengthUnitC
      = makeLengthUnitC(requestedLengthUnit);
  KIM_EnergyUnit requestedEnergyUnitC
      = makeEnergyUnitC(requestedEnergyUnit);
  KIM_ChargeUnit requestedChargeUnitC
      = makeChargeUnitC(requestedChargeUnit);
  KIM_TemperatureUnit requestedTemperatureUnitC
      = makeTemperatureUnitC(requestedTemperatureUnit);
  KIM_TimeUnit requestedTimeUnitC = makeTimeUnitC(requestedTimeUnit);
  if (languageName == LANGUAGE_NAME::Cpp)
  {
    error = CppInitialization(
        reinterpret_cast<KIM::ModelInitialization *>(&M),
        requestedLengthUnit, requestedEnergyUnit,
        requestedChargeUnit, requestedTemperatureUnit,
        requestedTimeUnit);
  }
  else if (languageName == LANGUAGE_NAME::C)
  {
    KIM_ModelInitialization cM;
    cM.p = &M;
    error = CInitialization(&cM, requestedLengthUnitC, requestedEnergyUnitC,
                            requestedChargeUnitC, requestedTemperatureUnitC,
                            requestedTimeUnitC);
  }
  else if (languageName == LANGUAGE_NAME::Fortran)
  {
    KIM_ModelInitialization cM;
    cM.p = &M;
    FInitialization(&cM, requestedLengthUnitC, requestedEnergyUnitC,
                    requestedChargeUnitC, requestedTemperatureUnitC,
                    requestedTimeUnitC, &error);
  }
  else
  {
    return true;
  }
  if (error)
  {
    //@@@@@ log error
    return true;
  }
  else
  {
    return false;
  }
}

int ModelImplementation::InitializeParameterizedModel(
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit)
{
  // get driver name
  int error = modelLibrary_->getModelDriverName(&modelDriverName_);
  if (error)
  {
    //@@@@ log error
    return true;
  }

  // write parameter files to scratch space
  error = WriteParameterFiles();
  if (error)
  {
    //@@@ log error
    return true;
  }

  // close model and open driver
  error = modelLibrary_->close();
  if (error)
  {
    //@@@@ log error
    return true;
  }
  error = modelLibrary_->open(false, modelDriverName_);
  if (error)
  {
    //@@@@ log error
    return true;
  }
  // check that it is a driver
  ModelLibrary::ITEM_TYPE itemType;
  error = modelLibrary_->getModelType(&itemType);
  if ((error) || (itemType != ModelLibrary::ITEM_TYPE::MODEL_DRIVER))
  {
    //@@@@@ log error
    return true;
  }

  LanguageName languageName;
  func * functionPointer = 0;
  error = modelLibrary_->getModelInitializationFunctionPointer(
      &languageName, &functionPointer);
  if (error)
  {
    //@@@@ log error
    return true;
  }

  typedef int ModelDriverInitializationCpp(
      KIM::ModelDriverInitialization * const modelDriverInitialization,
      LengthUnit const requestedLengthUnit,
      EnergyUnit const requestedEnergyUnit,
      ChargeUnit const requestedChargeUnit,
      TemperatureUnit const requestedTemperatureUnit,
      TimeUnit const requestedTimeUnit);
  ModelDriverInitializationCpp * CppInitialization
      = reinterpret_cast<ModelDriverInitializationCpp *>(functionPointer);
  typedef int ModelDriverInitializationC(
      KIM_ModelDriverInitialization * const modelDriverInitialization,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit);
  ModelDriverInitializationC * CInitialization
      = reinterpret_cast<ModelDriverInitializationC *>(functionPointer);
  typedef void ModelDriverInitializationF(
      KIM_ModelDriverInitialization * const modelDriverInitialization,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit,
      int * const);
  ModelDriverInitializationF * FInitialization
      = reinterpret_cast<ModelDriverInitializationF *>(functionPointer);


  struct Mdl {void * p;};
  Mdl M;
  M.p = this;
  KIM_LengthUnit requestedLengthUnitC
      = makeLengthUnitC(requestedLengthUnit);
  KIM_EnergyUnit requestedEnergyUnitC
      = makeEnergyUnitC(requestedEnergyUnit);
  KIM_ChargeUnit requestedChargeUnitC
      = makeChargeUnitC(requestedChargeUnit);
  KIM_TemperatureUnit requestedTemperatureUnitC
      = makeTemperatureUnitC(requestedTemperatureUnit);
  KIM_TimeUnit requestedTimeUnitC = makeTimeUnitC(requestedTimeUnit);
  if (languageName == LANGUAGE_NAME::Cpp)
  {
    error = CppInitialization(
        reinterpret_cast<KIM::ModelDriverInitialization *>(&M),
        requestedLengthUnit, requestedEnergyUnit,
        requestedChargeUnit, requestedTemperatureUnit,
        requestedTimeUnit);
  }
  else if (languageName == LANGUAGE_NAME::C)
  {
    KIM_ModelDriverInitialization cM;
    cM.p = &M;
    error = CInitialization(&cM, requestedLengthUnitC, requestedEnergyUnitC,
                            requestedChargeUnitC, requestedTemperatureUnitC,
                            requestedTimeUnitC);
  }
  else if (languageName == LANGUAGE_NAME::Fortran)
  {
    KIM_ModelDriverInitialization cM;
    cM.p = &M;
    FInitialization(&cM, requestedLengthUnitC, requestedEnergyUnitC,
                    requestedChargeUnitC, requestedTemperatureUnitC,
                    requestedTimeUnitC, &error);
  }
  else
  {
    return true;
  }
  if (error)
  {
    //@@@@@ log error
    return true;
  }

  // remove parameter files
  for (int i=0; i<numberOfParameterFiles_; ++i)
  {
    remove(parameterFileNames_[i].c_str());
  }
  // clear out parameter file stuff
  numberOfParameterFiles_ = -1;
  parameterFileNames_.clear();

  return false;
}



int ModelImplementation::WriteParameterFiles()
{
  modelLibrary_->getNumberOfParameterFiles(&numberOfParameterFiles_);
  std::vector<unsigned char const *> parameterFileStrings;
  std::vector<unsigned int> parameterFileStringLengths;
  for (int i=0; i<numberOfParameterFiles_; ++i)
  {
    unsigned char const * strPtr;
    unsigned int length;
    int error = modelLibrary_->getParameterFileString(i, &length , &strPtr);
    if (error) return true;
    parameterFileStrings.push_back(strPtr);
    parameterFileStringLengths.push_back(length);
  }

  static char const fileNameString[] = "kim-model-parameter-file-XXXXXXXXXXXX";
  for (int i=0; i<numberOfParameterFiles_; ++i)
  {
    std::stringstream templateString;
    templateString << P_tmpdir << fileNameString;
    char * cstr = strdup(templateString.str().c_str());
    int fileid = mkstemp(cstr);
    if (fileid == -1)
    {
      // @@@@@ log error
      free(cstr);
      return true;
    }
    parameterFileNames_.push_back(cstr);
    free(cstr);

    FILE* fl = fdopen(fileid,"w");
    fwrite(parameterFileStrings[i], parameterFileStringLengths[i], 1, fl);
    fclose(fl);  // also closed the fileid
  }

  return false;
}

}  // namespace KIM
