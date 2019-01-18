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
// Copyright (c) 2016--2019, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <sstream>

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIMHDR_OLD_KIM_API_DIRS_H
#include "old_KIM_API_DIRS.h"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#include "KIM_ComputeArgumentsImplementation.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_H_
extern "C" {
#include "KIM_UnitSystem.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_CREATE_H_
extern "C" {
#include "KIM_ModelCreate.h"

struct KIM_ModelCreate
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_DRIVER_CREATE_H_
extern "C" {
#include "KIM_ModelDriverCreate.h"

struct KIM_ModelDriverCreate
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_CREATE_H_
extern "C" {
#include "KIM_ModelComputeArgumentsCreate.h"

struct KIM_ModelComputeArgumentsCreate
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_REFRESH_H_
extern "C" {
#include "KIM_ModelRefresh.h"

struct KIM_ModelRefresh
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_COMPUTE_H_
extern "C" {
#include "KIM_ModelCompute.h"

struct KIM_ModelCompute
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_EXTENSION_H_
extern "C" {
#include "KIM_ModelExtension.h"

struct KIM_ModelExtension
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_WRITE_PARAMETERIZED_MODEL_H_
extern "C" {
#include "KIM_ModelWriteParameterizedModel.h"

struct KIM_ModelWriteParameterizedModel
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_H_
extern "C" {
#include "KIM_ModelComputeArguments.h"

struct KIM_ModelComputeArguments
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_DESTROY_H_
extern "C" {
#include "KIM_ModelComputeArgumentsDestroy.h"

struct KIM_ModelComputeArgumentsDestroy
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_MODEL_DESTROY_H_
extern "C" {
#include "KIM_ModelDestroy.h"

struct KIM_ModelDestroy
{
  void * p;
};
}  // extern "C"
#endif

#ifndef KIM_FUNCTION_TYPES_H_
extern "C" {
#include "KIM_FunctionTypes.h"
}
#endif

namespace KIM
{
namespace MODEL_ROUTINE_NAME
{
extern std::vector<ModelRoutineName> const requiredByAPI_ModelRoutines;
}  // namespace MODEL_ROUTINE_NAME
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

KIM_TemperatureUnit
makeTemperatureUnitC(KIM::TemperatureUnit const temperatureUnit)
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

// log helpers
#define SNUM(x) \
  static_cast<std::ostringstream &>(std::ostringstream() << std::dec << x).str()
#define SPTR(x)                                                      \
  static_cast<std::ostringstream &>(std::ostringstream()             \
                                    << static_cast<void const *>(x)) \
      .str()
#define SFUNC(x)                                                             \
  static_cast<std::ostringstream &>(std::ostringstream()                     \
                                    << reinterpret_cast<KIM::Function *>(x)) \
      .str()


#include "KIM_LogMacros.hpp"
#define KIM_LOGGER_OBJECT_NAME this
namespace KIM
{
// Forward declarations
class ModelCreate;
class ModelDriverCreate;
class ModelRefresh;
class ModelCompute;
class ModelComputeArguments;
class ModelDestroy;
class ModelComputeArgumentsCreate;
class ModelComputeArgumentsDestroy;

int ModelImplementation::Create(
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
  // error checking of arguments performed as part of ModelCreate()

  Log * pLog;
  int error = Log::Create(&pLog);
  if (error) { return true; }

  ModelImplementation * pModelImplementation;
  pModelImplementation = new ModelImplementation(new SharedLibrary(pLog), pLog);
#if DEBUG_VERBOSITY
  std::string const callString
      = "Create(" + numbering.ToString() + ", " + requestedLengthUnit.ToString()
        + ", " + requestedEnergyUnit.ToString() + ", "
        + requestedChargeUnit.ToString() + ", "
        + requestedTemperatureUnit.ToString() + ", "
        + requestedTimeUnit.ToString() + ", '" + modelName + "', "
        + SPTR(requestedUnitsAccepted) + ", " + SPTR(modelImplementation)
        + ").";
  pModelImplementation->LogEntry(
      LOG_VERBOSITY::debug,
      "Created Log and ModelImplementation objects after enter " + callString,
      __LINE__,
      __FILE__);
#endif

  error = pModelImplementation->ModelCreate(numbering,
                                            requestedLengthUnit,
                                            requestedEnergyUnit,
                                            requestedChargeUnit,
                                            requestedTemperatureUnit,
                                            requestedTimeUnit,
                                            modelName);
  if (error)
  {
#if DEBUG_VERBOSITY
    pModelImplementation->LogEntry(
        LOG_VERBOSITY::debug,
        "Destroying ModelImplementation object and exit " + callString,
        __LINE__,
        __FILE__);
#endif
    delete pModelImplementation;  // also deletes pLog

    return true;
  }

  LengthUnit finalLengthUnit;
  EnergyUnit finalEnergyUnit;
  ChargeUnit finalChargeUnit;
  TemperatureUnit finalTemperatureUnit;
  TimeUnit finalTimeUnit;
  pModelImplementation->GetUnits(&finalLengthUnit,
                                 &finalEnergyUnit,
                                 &finalChargeUnit,
                                 &finalTemperatureUnit,
                                 &finalTimeUnit);

  if (((finalLengthUnit == LENGTH_UNIT::unused)
       || (finalLengthUnit == requestedLengthUnit))
      && ((finalEnergyUnit == ENERGY_UNIT::unused)
          || (finalEnergyUnit == requestedEnergyUnit))
      && ((finalChargeUnit == CHARGE_UNIT::unused)
          || (finalChargeUnit == requestedChargeUnit))
      && ((finalTemperatureUnit == TEMPERATURE_UNIT::unused)
          || (finalTemperatureUnit == requestedTemperatureUnit))
      && ((finalTimeUnit == TIME_UNIT::unused)
          || (finalTimeUnit == requestedTimeUnit)))
  {
    pModelImplementation->LogEntry(
        LOG_VERBOSITY::debug, "Accepted requested units.", __LINE__, __FILE__);
    *requestedUnitsAccepted = true;
  }
  else
  {
    pModelImplementation->LogEntry(
        LOG_VERBOSITY::debug, "Rejected requested units.", __LINE__, __FILE__);
    *requestedUnitsAccepted = false;
  }

  *modelImplementation = pModelImplementation;
#if DEBUG_VERBOSITY
  (*modelImplementation)
      ->LogEntry(
          LOG_VERBOSITY::debug, "Exit 0=" + callString, __LINE__, __FILE__);
#endif
  return false;
}


void ModelImplementation::Destroy(
    ModelImplementation ** const modelImplementation)
{
#if DEBUG_VERBOSITY
  std::string callString = "Destroy(" + SPTR(modelImplementation) + ").";
  (*modelImplementation)
      ->LogEntry(
          LOG_VERBOSITY::debug, "Enter  " + callString, __LINE__, __FILE__);
#endif

  (*modelImplementation)->ModelDestroy();

#if DEBUG_VERBOSITY
  (*modelImplementation)
      ->LogEntry(LOG_VERBOSITY::debug,
                 "Destroying ModelImplementation object and exit " + callString,
                 __LINE__,
                 __FILE__);
#endif
  delete *modelImplementation;  // also deletes Log object
  *modelImplementation = NULL;
}

int ModelImplementation::IsRoutinePresent(
    ModelRoutineName const modelRoutineName,
    int * const present,
    int * const required) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "IsRoutinePresent("
                                 + modelRoutineName.ToString() + ", "
                                 + SNUM(present) + ", " + SNUM(required) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = !modelRoutineName.Known();
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator result
      = routineFunction_.find(modelRoutineName);
  if (result->second == NULL)
  {
    if (present != NULL) *present = false;
    if (required != NULL) *required = false;
  }
  else
  {
    if (present != NULL) *present = true;
    if (required != NULL)
    {
      std::map<ModelRoutineName const, int, MODEL_ROUTINE_NAME::Comparator>::
          const_iterator requiredResult
          = routineRequired_.find(modelRoutineName);
      *required = requiredResult->second;
    }
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::ComputeArgumentsCreate(
    ComputeArguments ** const computeArguments) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "ComputeArgumentsCreate(" + SPTR(computeArguments) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *computeArguments = new ComputeArguments();

  int error
      = ComputeArgumentsImplementation::Create(modelName_,
                                               log_->GetID(),
                                               modelNumbering_,
                                               simulatorNumbering_,
                                               numberingOffset_,
                                               &((*computeArguments)->pimpl));
  if (error)
  {
    delete *computeArguments;
    *computeArguments = NULL;

    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  error = ModelComputeArgumentsCreate(*computeArguments);
  if (error)
  {
    delete *computeArguments;
    *computeArguments = NULL;

    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // No further error checking needed

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::ComputeArgumentsDestroy(
    ComputeArguments ** const computeArguments) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "ComputeArgumentsDestroy(" + SPTR(computeArguments) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (modelName_ != (*computeArguments)->pimpl->modelName_)
  {
    LOG_ERROR("ComputeArguments object for Model '"
              + (*computeArguments)->pimpl->modelName_
              + "' cannot be Destroyed with the "
                "ModelDestroy() routine of Model '"
              + modelName_ + "'.");

    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  int error = ModelComputeArgumentsDestroy(*computeArguments);
  if (error)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  ComputeArgumentsImplementation::Destroy(&((*computeArguments)->pimpl));

  delete *computeArguments;
  *computeArguments = NULL;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void ModelImplementation::SetInfluenceDistancePointer(
    double const * const influenceDistance)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetInfluenceDistancePointer(" + SPTR(influenceDistance) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (influenceDistance == NULL)
    LOG_ERROR("Null pointer provided for InfluenceDistancePotiner.");
#endif

  influenceDistance_ = influenceDistance;

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::GetInfluenceDistance(
    double * const influenceDistance) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetInfluenceDistance(" + SPTR(influenceDistance) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *influenceDistance = *influenceDistance_;

  LOG_DEBUG("Exit   " + callString);
}


void ModelImplementation::SetNeighborListPointers(
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetNeighborListPointers(" + SNUM(numberOfNeighborLists) + ", "
        + SPTR(cutoffs) + ", "
        + SPTR(modelWillNotRequestNeighborsOfNoncontributingParticles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (numberOfNeighborLists < 1)
    LOG_ERROR("Number of neighbor lists, " + SNUM(numberOfNeighborLists)
              + ", must be >= 1.");
  if (cutoffs == NULL) LOG_ERROR("Null pointer provided for cutoffs.");
  if (modelWillNotRequestNeighborsOfNoncontributingParticles == NULL)
    LOG_ERROR("Null pointer provided for "
              "modelWillNotRequestNeighborsOfNoncontributingParticles.");
#endif

  numberOfNeighborLists_ = numberOfNeighborLists;
  cutoffs_ = cutoffs;
  modelWillNotRequestNeighborsOfNoncontributingParticles_
      = modelWillNotRequestNeighborsOfNoncontributingParticles;

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::GetNeighborListPointers(
    int * const numberOfNeighborLists,
    double const ** const cutoffs,
    int const ** const modelWillNotRequestNeighborsOfNoncontributingParticles)
    const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNeighborListPointers(" + SPTR(numberOfNeighborLists) + ", "
        + SPTR(cutoffs) + ", "
        + SPTR(modelWillNotRequestNeighborsOfNoncontributingParticles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (numberOfNeighborLists != NULL)
    *numberOfNeighborLists = numberOfNeighborLists_;
  if (cutoffs != NULL) *cutoffs = cutoffs_;
  if (modelWillNotRequestNeighborsOfNoncontributingParticles != NULL)
    *modelWillNotRequestNeighborsOfNoncontributingParticles
        = modelWillNotRequestNeighborsOfNoncontributingParticles_;

  LOG_DEBUG("Exit   " + callString);
}

int ModelImplementation::SetRoutinePointer(
    ModelRoutineName const modelRoutineName,
    LanguageName const languageName,
    int const required,
    Function * const fptr)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetRoutinePointer("
                                 + modelRoutineName.ToString() + ", "
                                 + languageName.ToString() + ", "
                                 + SNUM(required) + ", " + SFUNC(fptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = (!modelRoutineName.Known()) || (!languageName.Known());
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (!required)
  {
    using namespace KIM::MODEL_ROUTINE_NAME;
    for (unsigned int i = 0; i < requiredByAPI_ModelRoutines.size(); ++i)
    {
      if (requiredByAPI_ModelRoutines.at(i) == modelRoutineName)
      {
        LOG_ERROR(modelRoutineName.ToString() + " routine must be required.");
        LOG_DEBUG("Exit 1=" + callString);
        return true;
      }
    }
  }
#endif

  routineLanguage_[modelRoutineName] = languageName;
  routineRequired_[modelRoutineName] = bool(required);
  routineFunction_[modelRoutineName] = fptr;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetSpeciesCode(SpeciesName const speciesName,
                                        int const code)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetSpeciesCode(" + speciesName.ToString() + ", " + SNUM(code) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = !speciesName.Known();
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  supportedSpecies_[speciesName] = code;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::GetSpeciesSupportAndCode(
    KIM::SpeciesName const speciesName,
    int * const speciesIsSupported,
    int * const code) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSpeciesSupportAndCode(" + speciesName.ToString() + ", "
        + SPTR(speciesIsSupported) + ", " + SPTR(code) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = !speciesName.Known();
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  std::map<SpeciesName const, int, SPECIES_NAME::Comparator>::const_iterator
      result
      = supportedSpecies_.find(speciesName);
  if (result == supportedSpecies_.end())
  {
    LOG_DEBUG("Species is not supported.");
    *speciesIsSupported = false;
  }
  else
  {
    LOG_DEBUG("Species is supported.");
    *speciesIsSupported = true;
    if (code != NULL) *code = result->second;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetModelNumbering(Numbering const numbering)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetModelNumbering(" + numbering.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = !numbering.Known();
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  modelNumbering_ = numbering;
  numberingHasBeenSet_ = true;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetSimulatorNumbering(Numbering const numbering)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetSimulatorNumbering(" + numbering.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = !numbering.Known();
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  simulatorNumbering_ = numbering;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetUnits(LengthUnit const lengthUnit,
                                  EnergyUnit const energyUnit,
                                  ChargeUnit const chargeUnit,
                                  TemperatureUnit const temperatureUnit,
                                  TimeUnit const timeUnit)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetUnits(" + lengthUnit.ToString() + ", " + energyUnit.ToString()
        + ", " + chargeUnit.ToString() + ", " + temperatureUnit.ToString()
        + ", " + timeUnit.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = (!lengthUnit.Known()) || (!energyUnit.Known())
              || (!chargeUnit.Known()) || (!temperatureUnit.Known())
              || (!timeUnit.Known());
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (lengthUnit == LENGTH_UNIT::unused)
  {
    LOG_ERROR("Models cannot specify 'unused' for LengthUnit");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (energyUnit == ENERGY_UNIT::unused)
  {
    LOG_ERROR("Models cannot specify 'unused' for EnergyUnit");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  lengthUnit_ = lengthUnit;
  energyUnit_ = energyUnit;
  chargeUnit_ = chargeUnit;
  temperatureUnit_ = temperatureUnit;
  timeUnit_ = timeUnit;

  unitsHaveBeenSet_ = true;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void ModelImplementation::GetUnits(LengthUnit * const lengthUnit,
                                   EnergyUnit * const energyUnit,
                                   ChargeUnit * const chargeUnit,
                                   TemperatureUnit * const temperatureUnit,
                                   TimeUnit * const timeUnit) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetUnits(" + SPTR(lengthUnit) + ", "
                                 + SPTR(energyUnit) + ", " + SPTR(chargeUnit)
                                 + ", " + SPTR(temperatureUnit) + ", "
                                 + SPTR(timeUnit) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (lengthUnit != NULL) *lengthUnit = lengthUnit_;
  if (energyUnit != NULL) *energyUnit = energyUnit_;
  if (chargeUnit != NULL) *chargeUnit = chargeUnit_;
  if (temperatureUnit != NULL) *temperatureUnit = temperatureUnit_;
  if (timeUnit != NULL) *timeUnit = timeUnit_;

  LOG_DEBUG("Exit   " + callString);
}

int ModelImplementation::GetNumberOfParameterFiles(
    int * const numberOfParameterFiles) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNumberOfParameterFiles(" + SPTR(numberOfParameterFiles) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (itemType_ != SharedLibrary::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("Only parameterized models have parameter files.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  *numberOfParameterFiles = numberOfParameterFiles_;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::GetParameterFileName(
    int const index, std::string const ** const parameterFileName) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameterFileName(" + SNUM(index) + ", "
                                 + SPTR(parameterFileName) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (itemType_ != SharedLibrary::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("Only parameterized models have parameter files.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if ((index < 0) || (index >= numberOfParameterFiles_))
  {
    LOG_ERROR("Invalid parameter file index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (parameterFileName == NULL)
  {
    LOG_ERROR("Null pointer provided for parameterFileName.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  *parameterFileName = &(parameterFileNames_[index]);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void ModelImplementation::SetParameterFileName(
    std::string const & filename) const
{
  cmakelists_ << "    \"" << filename << "\"\n";
}

int ModelImplementation::SetParameterPointer(int const extent,
                                             int * const ptr,
                                             std::string const & name,
                                             std::string const & description)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetParameterPointer(" + SNUM(extent) + ", "
                                 + SPTR(ptr) + ", '" + name + "', '"
                                 + description + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (extent == 0)
  {
    LOG_ERROR("Extent, " + SNUM(extent) + ", must be > 0.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (ptr == NULL)
  {
    LOG_ERROR("Null pointer provided for parameter.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (!IsCIdentifier(name))
  {
    LOG_ERROR("Name '" + name + "' is not a valid C identifier.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (std::find(parameterName_.begin(), parameterName_.end(), name)
      != parameterName_.end())
  {
    LOG_ERROR("Name '" + name
              + "' is already associated with another "
                "parameter.  Parameter names must be unique.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  parameterName_.push_back(name);
  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Integer);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetParameterPointer(int const extent,
                                             double * const ptr,
                                             std::string const & name,
                                             std::string const & description)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetParameterPointer(" + SNUM(extent) + ", "
                                 + SPTR(ptr) + ", '" + name + "', '"
                                 + description + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (extent == 0)
  {
    LOG_ERROR("Extent, " + SNUM(extent) + ", must be > 0.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (ptr == NULL)
  {
    LOG_ERROR("Null pointer provided for parameter.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (!IsCIdentifier(name))
  {
    LOG_ERROR("Name '" + name + "' is not a valid C identifier.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (std::find(parameterName_.begin(), parameterName_.end(), name)
      != parameterName_.end())
  {
    LOG_ERROR("Name '" + name
              + "' is already associated with another "
                "parameter.  Parameter names must be unique.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  parameterName_.push_back(name);
  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Double);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void ModelImplementation::GetNumberOfParameters(
    int * const numberOfParameters) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetNumberOfParameters(" + SPTR(numberOfParameters) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *numberOfParameters = parameterPointer_.size();

  LOG_DEBUG("Exit   " + callString);
}

int ModelImplementation::GetParameterMetadata(
    int const parameterIndex,
    DataType * const dataType,
    int * const extent,
    std::string const ** const name,
    std::string const ** const description) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameterMetadata(" + SNUM(parameterIndex)
                                 + ", " + SPTR(dataType) + ", " + SPTR(extent)
                                 + ", " + SPTR(name) + ", " + SPTR(description)
                                 + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((parameterIndex < 0)
      || (static_cast<unsigned int>(parameterIndex)
          >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  if (dataType != NULL) *dataType = parameterDataType_[parameterIndex];
  if (extent != NULL) *extent = parameterExtent_[parameterIndex];
  if (name != NULL) *name = &(parameterName_[parameterIndex]);
  if (description != NULL)
    *description = &(parameterDescription_[parameterIndex]);

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::GetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      int * const parameterValue) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameter(" + SNUM(parameterIndex) + ", "
                                 + SNUM(arrayIndex) + ", "
                                 + SPTR(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((parameterIndex < 0)
      || (static_cast<unsigned int>(parameterIndex)
          >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  *parameterValue = reinterpret_cast<int const *>(
      parameterPointer_[parameterIndex])[arrayIndex];

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::GetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      double * const parameterValue) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetParameter(" + SNUM(parameterIndex) + ", "
                                 + SNUM(arrayIndex) + ", "
                                 + SPTR(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((parameterIndex < 0)
      || (static_cast<unsigned int>(parameterIndex)
          >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  *parameterValue = reinterpret_cast<double const *>(
      parameterPointer_[parameterIndex])[arrayIndex];

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      int const parameterValue)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetParameter(" + SNUM(parameterIndex) + ", "
                                 + SNUM(arrayIndex) + ", "
                                 + SNUM(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((parameterIndex < 0)
      || (static_cast<unsigned int>(parameterIndex)
          >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  reinterpret_cast<int *>(parameterPointer_[parameterIndex])[arrayIndex]
      = parameterValue;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::SetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      double const parameterValue)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetParameter(" + SNUM(parameterIndex) + ", "
                                 + SNUM(arrayIndex) + ", "
                                 + SNUM(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if ((parameterIndex < 0)
      || (static_cast<unsigned int>(parameterIndex)
          >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  reinterpret_cast<double *>(parameterPointer_[parameterIndex])[arrayIndex]
      = parameterValue;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::Compute(
    ComputeArguments const * const computeArguments) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "Compute(" + SPTR(computeArguments) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  // Check that computeArguments is for this model
  if (modelName_ != computeArguments->pimpl->modelName_)
  {
    LOG_ERROR("ComputeArguments object for Model '"
              + computeArguments->pimpl->modelName_
              + "' cannot be used with the "
                "ModelCompute() routine of Model '"
              + modelName_ + "'.");

    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // Check that computeArguments is ready...
#if ERROR_VERBOSITY
  int isReady;
  computeArguments->AreAllRequiredArgumentsAndCallbacksPresent(&isReady);
  if (!isReady)
  {
    LOG_ERROR("Not all required ComputeArguments and ComputeCallbacks "
              "are present in ComputeArguments object.");

    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  // Set cutoffs data within computeArguments
  computeArguments->pimpl->inModelComputeRoutine_ = true;
  computeArguments->pimpl->numberOfNeighborLists_ = numberOfNeighborLists_;
  computeArguments->pimpl->cutoffs_ = cutoffs_;
  // Resize computeArguments storage if needed
  if (simulatorNumbering_ != modelNumbering_)
  {
    computeArguments->pimpl->getNeighborListStorage_.resize(
        numberOfNeighborLists_);
  }

  // Call the Model supplied compute routine
  int error = ModelCompute(computeArguments);

  // Unset cutoffs data within computeArguments
  computeArguments->pimpl->inModelComputeRoutine_ = false;
  computeArguments->pimpl->numberOfNeighborLists_ = 0;
  computeArguments->pimpl->cutoffs_ = NULL;

  if (error)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

void ModelImplementation::GetExtensionID(
    std::string const ** const extensionID) const
{
  *extensionID = &extensionID_;
}

int ModelImplementation::Extension(std::string const & extensionID,
                                   void * const extensionStructure)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "Extension(\"" + extensionID + "\", " + SPTR(extensionStructure) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int present = false;
  IsRoutinePresent(KIM::MODEL_ROUTINE_NAME::Extension, &present, NULL);
  if (!present)
  {
    LOG_ERROR("The Model does not provide the Extension routine.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  extensionID_ = extensionID;
  int error = ModelExtension(extensionStructure);
  extensionID_ = "";

  if (error)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ClearThenRefresh()
{
#if DEBUG_VERBOSITY
  std::string const callString = "ClearThenRefresh().";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (parameterPointer_.size() == 0)
  {
    LOG_ERROR("ClearThenRefresh() called but no parameter pointers have been "
              "set (and no Refresh routine has been provided).");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  influenceDistance_ = NULL;
  numberOfNeighborLists_ = 0;
  cutoffs_ = NULL;
  modelWillNotRequestNeighborsOfNoncontributingParticles_ = NULL;

  // Call Model supplied Refresh routine
  int error = ModelRefresh();

  if (error)
  {
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
#if ERROR_VERBOSITY
    // error checking
    if (influenceDistance_ == NULL)
    {
      LOG_ERROR("Model supplied Refresh() routine did not set "
                "influenceDistance.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    if (numberOfNeighborLists_ < 1)
    {
      LOG_ERROR("Number of neighbor lists, " + SNUM(numberOfNeighborLists_)
                + ", must be >= 1.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    if (cutoffs_ == NULL)
    {
      LOG_ERROR("Model supplied Refresh() routine did not "
                "set cutoffs.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    double maxCutoff = 0.0;
    for (int i = 0; i < numberOfNeighborLists_; ++i)
    {
      if (maxCutoff < cutoffs_[i]) maxCutoff = cutoffs_[i];
    }
    if (maxCutoff > *influenceDistance_)
    {
      LOG_ERROR("Model max(cutoffs) > influenceDistance.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    if (modelWillNotRequestNeighborsOfNoncontributingParticles_ == NULL)
    {
      LOG_ERROR("Model supplied Refresh() routine did not "
                "set modelWillNotRequestNeighborsOfNoncontributingParticles.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
#endif
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

void ModelImplementation::GetPath(std::string const ** const path) const
{
  *path = &writePath_;
}

void ModelImplementation::GetModelName(
    std::string const ** const modelName) const
{
  *modelName = &writeModelName_;
}

int ModelImplementation::WriteParameterizedModel(
    std::string const & path, std::string const & modelName) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "WriteParameterizedModel(\"" + path + "\", \"" + modelName + "\").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  if (itemType_ != SharedLibrary::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("Only parameterized models can implement the "
              "WritePrameterizedModel() routine.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  int present = false;
  IsRoutinePresent(
      KIM::MODEL_ROUTINE_NAME::WriteParameterizedModel, &present, NULL);
  if (!present)
  {
    LOG_ERROR(
        "The Model does not provide the WriteParameterizedModel routine.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (!IsCIdentifier(modelName))
  {
    LOG_ERROR("modelName '" + modelName + "' is not a valid C identifier.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  cmakelists_.str("");
  cmakelists_
      << "# CMakeLists.txt -- auto-generated by "
         "KIM::Model::WriteParameterizedModel()\n"
         "\n"
         "# Required preamble\n"
         "\n"
         "cmake_minimum_required(VERSION 3.4)\n"
         "list(APPEND CMAKE_PREFIX_PATH $ENV{KIM_API_V2_CMAKE_PREFIX_DIR})\n"
         "find_package(KIM-API-V2 2.0 REQUIRED CONFIG)\n"
         "if(NOT TARGET kim-api)\n"
         "  enable_testing()\n"
         "  project(\"${KIM_API_PROJECT_NAME}\" VERSION "
         "\"${KIM_API_VERSION}\"\n"
         "    LANGUAGES CXX C Fortran)\n"
         "endif()\n"
         "\n"
         "# End preamble\n"
         "\n"
         "add_kim_api_model_library(\n"
         "  NAME \""
      << modelName << "\"\n"
      << "  DRIVER_NAME \"" << modelDriverName_
      << "\"\n"
         "  PARAMETER_FILES\n";

  writePath_ = path;
  writeModelName_ = modelName;
  int error = ModelWriteParameterizedModel();

  if (error)
  {
    LOG_ERROR(
        "Model supplied WriteParameterizedModel() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  cmakelists_ << "  )\n";
  std::ofstream cmakeFile((path + "/CMakeLists.txt").c_str(),
                          std::ios::out | std::ios::trunc);
  if (cmakeFile.is_open())
  {
    cmakeFile << cmakelists_.str();
    cmakeFile.close();
  }
  else
  {
    LOG_ERROR("Unable to open CMakeLists.txt file for write.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void ModelImplementation::SetModelBufferPointer(void * const ptr)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetModelBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  modelBuffer_ = ptr;

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::GetModelBufferPointer(void ** const ptr) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "GetModelBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *ptr = modelBuffer_;

  LOG_DEBUG("Exit   " + callString);
}


void ModelImplementation::SetSimulatorBufferPointer(void * const ptr)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "SetSimulatorBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  simulatorBuffer_ = ptr;

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::GetSimulatorBufferPointer(void ** const ptr) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "GetSimulatorBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  *ptr = simulatorBuffer_;

  LOG_DEBUG("Exit   " + callString);
}

namespace
{
typedef std::map<LengthUnit const, double, LENGTH_UNIT::Comparator> LengthMap;

LengthMap const GetLengthMap()
{
  LengthMap mm;
  mm[LENGTH_UNIT::A] = 1.0e-10;
  mm[LENGTH_UNIT::Bohr] = 5.291772109217171e-11;
  mm[LENGTH_UNIT::cm] = 1.0e-2;
  mm[LENGTH_UNIT::m] = 1.0;
  mm[LENGTH_UNIT::nm] = 1.0e-9;
  return mm;
}

typedef std::map<EnergyUnit const, double, ENERGY_UNIT::Comparator> EnergyMap;

EnergyMap const GetEnergyMap()
{
  EnergyMap m;
  m[ENERGY_UNIT::amu_A2_per_ps2] = 1.66053886e-23;
  m[ENERGY_UNIT::erg] = 1.0e7;
  m[ENERGY_UNIT::eV] = 1.60217646e-19;
  m[ENERGY_UNIT::Hartree] = 4.3597439422e-18;
  m[ENERGY_UNIT::J] = 1.0;
  m[ENERGY_UNIT::kcal_mol] = 6.9477e-21;
  return m;
}

typedef std::map<ChargeUnit const, double, CHARGE_UNIT::Comparator> ChargeMap;

ChargeMap const GetChargeMap()
{
  ChargeMap m;
  m[CHARGE_UNIT::C] = 1.0;
  m[CHARGE_UNIT::e] = 1.602e-19;
  m[CHARGE_UNIT::statC] = 2.99792458e-9;
  return m;
}

typedef std::map<TemperatureUnit const, double, TEMPERATURE_UNIT::Comparator>
    TemperatureMap;

TemperatureMap const GetTemperatureMap()
{
  TemperatureMap m;
  m[TEMPERATURE_UNIT::K] = 1.0;
  return m;
}

typedef std::map<TimeUnit const, double, TIME_UNIT::Comparator> TimeMap;

TimeMap const GetTimeMap()
{
  TimeMap m;
  m[TIME_UNIT::fs] = 1.0e-15;
  m[TIME_UNIT::ps] = 1.0e-12;
  m[TIME_UNIT::ns] = 1.0e-9;
  m[TIME_UNIT::s] = 1.0;
  return m;
}
}  // namespace

int ModelImplementation::ConvertUnit(LengthUnit const fromLengthUnit,
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
                                     double * const conversionFactor)
{
  // No debug logging for ConvertUnit: no log object available
  //
  // #if DEBUG_VERBOSITY
  //   std::string const callString = "ConvertUnit("
  //       + fromLengthUnit.ToString() + ", "
  //       + fromEnergyUnit.ToString() + ", "
  //       + fromChargeUnit.ToString() + ", "
  //       + fromTemperatureUnit.ToString() + ", "
  //       + fromTimeUnit.ToString() + ", "
  //       + toLengthUnit.ToString() + ", "
  //       + toEnergyUnit.ToString() + ", "
  //       + toChargeUnit.ToString() + ", "
  //       + toTemperatureUnit.ToString() + ", "
  //       + toTimeUnit.ToString() + ", "
  //       + SNUM(lengthExponent) + ", "
  //       + SNUM(energyExponent) + ", "
  //       + SNUM(chargeExponent) + ", "
  //       + SNUM(temperatureExponent) + ", "
  //       + SNUM(timeExponent) + ", "
  //       + SPTR(conversionFactor) + ").";
  // #endif
  //   LOG_DEBUG("Enter  " + callString);

  static LengthMap const lengthConvertToSI = GetLengthMap();
  static EnergyMap const energyConvertToSI = GetEnergyMap();
  static ChargeMap const chargeConvertToSI = GetChargeMap();
  static TemperatureMap const temperatureConvertToSI = GetTemperatureMap();
  static TimeMap const timeConvertToSI = GetTimeMap();

#if ERROR_VERBOSITY
  int error = (!fromLengthUnit.Known()) || (!fromEnergyUnit.Known())
              || (!fromChargeUnit.Known()) || (!fromTemperatureUnit.Known())
              || (!fromTimeUnit.Known()) || (!toLengthUnit.Known())
              || (!toEnergyUnit.Known()) || (!toChargeUnit.Known())
              || (!toTemperatureUnit.Known()) || (!toTimeUnit.Known());
  if (error)
  {
    // LOG_ERROR("Invalid arguments.");
    // LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  bool lengthUnused = ((fromLengthUnit == KIM::LENGTH_UNIT::unused)
                       || (toLengthUnit == KIM::LENGTH_UNIT::unused));
  if ((lengthExponent != 0.0) && lengthUnused)
  {
    // LOG_ERROR("Unable to convert unit.");
    // LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  double const lengthConversion
      = (lengthUnused) ? 1
                       : (lengthConvertToSI.find(fromLengthUnit)->second
                          / lengthConvertToSI.find(toLengthUnit)->second);

  bool energyUnused = ((fromEnergyUnit == KIM::ENERGY_UNIT::unused)
                       || (toEnergyUnit == KIM::ENERGY_UNIT::unused));
  if ((energyExponent != 0.0) && energyUnused)
  {
    // LOG_ERROR("Unable to convert unit.");
    // LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  double const energyConversion
      = (energyUnused) ? 1
                       : (energyConvertToSI.find(fromEnergyUnit)->second
                          / energyConvertToSI.find(toEnergyUnit)->second);

  bool chargeUnused = ((fromChargeUnit == KIM::CHARGE_UNIT::unused)
                       || (toChargeUnit == KIM::CHARGE_UNIT::unused));
  if ((chargeExponent != 0.0) && chargeUnused)
  {
    // LOG_ERROR("Unable to convert unit.");
    // LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  double const chargeConversion
      = (chargeUnused) ? 1
                       : (chargeConvertToSI.find(fromChargeUnit)->second
                          / chargeConvertToSI.find(toChargeUnit)->second);

  bool temperatureUnused
      = ((fromTemperatureUnit == KIM::TEMPERATURE_UNIT::unused)
         || (toTemperatureUnit == KIM::TEMPERATURE_UNIT::unused));
  if ((temperatureExponent != 0.0) && temperatureUnused)
  {
    // LOG_ERROR("Unable to convert unit.");
    // LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  double const temperatureConversion
      = (temperatureUnused)
            ? 1
            : (temperatureConvertToSI.find(fromTemperatureUnit)->second
               / temperatureConvertToSI.find(toTemperatureUnit)->second);

  bool timeUnused = ((fromTimeUnit == KIM::TIME_UNIT::unused)
                     || (toTimeUnit == KIM::TIME_UNIT::unused));
  if ((timeExponent != 0.0) && timeUnused)
  {
    // LOG_ERROR("Unable to convert unit.");
    // LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  double const timeConversion
      = (timeUnused) ? 1
                     : (timeConvertToSI.find(fromTimeUnit)->second
                        / timeConvertToSI.find(toTimeUnit)->second);

  *conversionFactor = pow(lengthConversion, lengthExponent)
                      * pow(energyConversion, energyExponent)
                      * pow(chargeConversion, chargeExponent)
                      * pow(temperatureConversion, temperatureExponent)
                      * pow(timeConversion, timeExponent);

  // LOG_DEBUG("Exit 0=" + callString);
  return false;
}

void ModelImplementation::SetLogID(std::string const & logID)
{
#if DEBUG_VERBOSITY
  std::string const callString = "SetLogID('" + logID + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->SetID(logID);

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::PushLogVerbosity(LogVerbosity const logVerbosity)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "PushLogVerbosity(" + logVerbosity.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->PushVerbosity(logVerbosity);

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::PopLogVerbosity()
{
#if DEBUG_VERBOSITY
  std::string const callString = "PopLogVerbosity().";
#endif
  LOG_DEBUG("Enter  " + callString);

  log_->PopVerbosity();

  LOG_DEBUG("Exit   " + callString);
}

void ModelImplementation::LogEntry(LogVerbosity const logVerbosity,
                                   std::string const & message,
                                   int const lineNumber,
                                   std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelImplementation::LogEntry(LogVerbosity const logVerbosity,
                                   std::stringstream const & message,
                                   int const lineNumber,
                                   std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelImplementation::ToString() const
{
#if DEBUG_VERBOSITY
  std::string const callString = "ToString().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::stringstream ss;
  ss << std::setprecision(10) << std::scientific << std::left;
  ss << "====================================================================="
        "===========\n\n";

  ss << "Model object\n"
     << "------------\n\n";
  ss << "Model Name : " << modelName_ << "\n";
  if (itemType_ == SharedLibrary::PARAMETERIZED_MODEL)
  { ss << "Model Driver Name : " << modelDriverName_ << "\n"; }
  ss << "Log ID : " << log_->GetID() << "\n";
  ss << "\n";

  ss << "Model Supplied Routines :\n"
     << "\t" << std::setw(25) << "Routine Name" << std::setw(10) << "Language"
     << std::setw(10) << "Required" << std::setw(25)
     << "Pointer (1-set / 0-unset)"
     << "\n"
     << "\t" << std::setw(25) << "-------------------------" << std::setw(10)
     << "----------" << std::setw(10) << "----------" << std::setw(25)
     << "-------------------------"
     << "\n\n";
  int numberOfModelRoutineNames;
  MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames(&numberOfModelRoutineNames);
  for (int i = 0; i < numberOfModelRoutineNames; ++i)
  {
    ModelRoutineName modelRoutineName;
    MODEL_ROUTINE_NAME::GetModelRoutineName(i, &modelRoutineName);

    std::map<ModelRoutineName const,
             LanguageName,
             MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
        = routineLanguage_.find(modelRoutineName);
    std::map<ModelRoutineName const, int, MODEL_ROUTINE_NAME::Comparator>::
        const_iterator requiredResult
        = routineRequired_.find(modelRoutineName);
    std::map<ModelRoutineName const,
             Function *,
             MODEL_ROUTINE_NAME::Comparator>::const_iterator fptrResult
        = routineFunction_.find(modelRoutineName);

    ss << "\t" << std::setw(25) << modelRoutineName.ToString() << std::setw(10)
       << langResult->second.ToString() << std::setw(10)
       << requiredResult->second << std::setw(25) << SFUNC(fptrResult->second)
       << "\n";
  }
  ss << "\n";

  ss << "Numbering : " << modelNumbering_.ToString() << "\n\n";

  ss << "Units : \n"
        "\tLength Unit      : "
     << lengthUnit_.ToString()
     << "\n"
        "\tEnergy Unit      : "
     << energyUnit_.ToString()
     << "\n"
        "\tCharge Unit      : "
     << chargeUnit_.ToString()
     << "\n"
        "\tTemperature Unit : "
     << temperatureUnit_.ToString()
     << "\n"
        "\tTime Unit        : "
     << timeUnit_.ToString() << "\n\n";

  if (influenceDistance_ == NULL)
    ss << "Influence Distance : "
       << "NULL"
       << "\n\n";
  else
    ss << "Influence Distance : " << *influenceDistance_ << "\n\n";

  ss << "Number Of Neighbor Lists : " << numberOfNeighborLists_ << "\n";
  ss << "Neighbor List Cutoffs :\n";
  ss << "\t"
     << "index"
     << " : " << std::setw(20) << "cutoff distance" << std::setw(40)
     << "modelWillNotRequestNeighborsOfNoncontributingParticles"
     << "\n";
  ss << "\t"
     << "-----"
     << "---" << std::setw(20) << "--------------------" << std::setw(40)
     << "----------------------------------------"
     << "\n\n";
  for (int i = 0; i < numberOfNeighborLists_; ++i)
  {
    ss << "\t" << std::setw(5) << i << " : " << std::setw(20) << cutoffs_[i]
       << std::setw(40)
       << modelWillNotRequestNeighborsOfNoncontributingParticles_[i] << "\n";
  }
  ss << "\n\n";

  ss << "Supported Species :\n";
  int const specWidth = 10;
  ss << "\t" << std::setw(specWidth) << "Species" << std::setw(specWidth)
     << "Code"
     << "\n";
  ss << "\t" << std::setw(specWidth) << "----------" << std::setw(specWidth)
     << "----------"
     << "\n\n";
  for (std::map<SpeciesName const, int, SPECIES_NAME::Comparator>::
           const_iterator spec
       = supportedSpecies_.begin();
       spec != supportedSpecies_.end();
       ++spec)
  {
    ss << "\t" << std::setw(specWidth) << (spec->first).ToString()
       << std::setw(specWidth) << spec->second << "\n";
  }
  ss << "\n";


  int numberOfParameters = parameterPointer_.size();
  ss << "Number Of Parameters : " << numberOfParameters << "\n";
  ss << "\t" << std::setw(8) << "index" << std::setw(10) << "Data Type"
     << std::setw(10) << "Extent" << std::setw(15) << "Pointer"
     << "Name"
     << "\n";
  ss << "\t" << std::setw(8) << "--------" << std::setw(10) << "----------"
     << std::setw(10) << "----------" << std::setw(15) << "---------------"
     << "-------------------------"
     << "\n\n";
  for (int i = 0; i < numberOfParameters; ++i)
  {
    ss << "\t" << std::setw(8) << i << std::setw(10)
       << parameterDataType_[i].ToString() << std::setw(10)
       << parameterExtent_[i] << std::setw(15) << SPTR(parameterPointer_[i])
       << parameterName_[i] << "\n";
  }
  ss << "\n";

  ss << "Buffers\n";
  ss << "\t" << std::setw(15) << "Buffer" << std::setw(15) << "Pointer"
     << "\n";
  ss << "\t" << std::setw(15) << "---------------" << std::setw(15)
     << "---------------"
     << "\n\n";
  ss << "\t" << std::setw(15) << "Model" << std::setw(15) << SPTR(modelBuffer_)
     << "\n"
     << "\t" << std::setw(15) << "Simulator" << std::setw(15)
     << SPTR(simulatorBuffer_) << "\n\n";

  ss << "===================================================================="
        "============\n";

  string_ = ss.str();
  LOG_DEBUG("Exit   " + callString);
  return string_;
}


ModelImplementation::ModelImplementation(SharedLibrary * const sharedLibrary,
                                         Log * const log) :
    itemType_(SharedLibrary::STAND_ALONE_MODEL),
    modelName_(""),
    modelDriverName_(""),
    sharedLibrary_(sharedLibrary),
    numberOfParameterFiles_(0),
    log_(log),
    numberingHasBeenSet_(false),
    modelNumbering_(NUMBERING::zeroBased),
    simulatorNumbering_(NUMBERING::zeroBased),
    numberingOffset_(0),
    unitsHaveBeenSet_(false),
    lengthUnit_(LENGTH_UNIT::unused),
    energyUnit_(ENERGY_UNIT::unused),
    chargeUnit_(CHARGE_UNIT::unused),
    temperatureUnit_(TEMPERATURE_UNIT::unused),
    timeUnit_(TIME_UNIT::unused),
    influenceDistance_(NULL),
    numberOfNeighborLists_(0),
    cutoffs_(NULL),
    modelWillNotRequestNeighborsOfNoncontributingParticles_(NULL),
    modelBuffer_(NULL),
    simulatorBuffer_(NULL),
    string_("")
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "ModelImplementation(" + SPTR(sharedLibrary) + ", " + SPTR(log) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  // populate default values for
  // routineLanguage, routineRequired, routineFunction
  int numberOfModelRoutineNames;
  MODEL_ROUTINE_NAME::GetNumberOfModelRoutineNames(&numberOfModelRoutineNames);
  for (int i = 0; i < numberOfModelRoutineNames; ++i)
  {
    ModelRoutineName modelRoutineName;
    MODEL_ROUTINE_NAME::GetModelRoutineName(i, &modelRoutineName);

    routineLanguage_[modelRoutineName] = LANGUAGE_NAME::cpp;
    routineRequired_[modelRoutineName] = false;
    routineFunction_[modelRoutineName] = NULL;
  }

  LOG_DEBUG("Exit   " + callString);
}

ModelImplementation::~ModelImplementation()
{
#if DEBUG_VERBOSITY
  std::string const callString = "~ModelImplementation().";
#endif
  LOG_DEBUG("Enter  " + callString);

  delete sharedLibrary_;

  LOG_DEBUG("Destroying Log object and exit " + callString);
  Log::Destroy(&log_);
}

int ModelImplementation::ModelCreate(
    Numbering const numbering,
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit,
    std::string const & modelName)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "ModelCreate(" + numbering.ToString() + ", "
        + requestedLengthUnit.ToString() + ", " + requestedEnergyUnit.ToString()
        + ", " + requestedChargeUnit.ToString() + ", "
        + requestedTemperatureUnit.ToString() + ", "
        + requestedTimeUnit.ToString() + ", '" + modelName + "').";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error
      = (!numbering.Known()) || (!requestedLengthUnit.Known())
        || (!requestedEnergyUnit.Known()) || (!requestedChargeUnit.Known())
        || (!requestedTemperatureUnit.Known()) || (!requestedTimeUnit.Known());
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  modelName_ = modelName;

  error = SetSimulatorNumbering(numbering);
  if (error)
  {
    LOG_ERROR("Could not set simulator numbering.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  std::vector<std::string> sharedLibraryList;
  if (!findItem(OLD_KIM::KIM_MODELS_DIR, modelName, &sharedLibraryList, NULL))
  {
    LOG_ERROR("Could not find model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  error = sharedLibrary_->Open(sharedLibraryList[OLD_KIM::IE_FULLPATH]);
  if (error)
  {
    LOG_ERROR("Could not open model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  error = sharedLibrary_->GetType(&itemType_);
  switch (itemType_)
  {
    case SharedLibrary::STAND_ALONE_MODEL:
      LOG_DEBUG("Initializing a stand alone model.");
      error = InitializeStandAloneModel(requestedLengthUnit,
                                        requestedEnergyUnit,
                                        requestedChargeUnit,
                                        requestedTemperatureUnit,
                                        requestedTimeUnit);
      if (error)
      {
        LOG_ERROR("Initialization of Stand Alone model returned error.");
        LOG_DEBUG("Exit 1=" + callString);
        return true;
      }
      break;
    case SharedLibrary::PARAMETERIZED_MODEL:
      LOG_DEBUG("Initializing a parameterized model.");
      error = InitializeParameterizedModel(requestedLengthUnit,
                                           requestedEnergyUnit,
                                           requestedChargeUnit,
                                           requestedTemperatureUnit,
                                           requestedTimeUnit);
      if (error)
      {
        LOG_ERROR("Initialization of Parameterized Model returned error.");
        LOG_DEBUG("Exit 1=" + callString);
        return true;
      }
      break;
    case SharedLibrary::MODEL_DRIVER:
      LOG_ERROR("Creation of a model driver is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
    case SharedLibrary::SIMULATOR_MODEL:
      LOG_ERROR("Creation of a simulator model is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
    default:
      LOG_ERROR("Creation of an unknown model type is not allowed.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
      break;
  }

#if ERROR_VERBOSITY
  // Error checking
  //  no need to check parameters
  if (!numberingHasBeenSet_)
  {
    LOG_ERROR("Model supplied Create() routine did not set numbering.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (!unitsHaveBeenSet_)
  {
    LOG_ERROR("Model supplied Create() routine did not set units.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (influenceDistance_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "influenceDistance.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (numberOfNeighborLists_ < 1)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "valid numberOfNeighborLists, must be >= 1.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (cutoffs_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "cutoffs.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  double maxCutoff = 0.0;
  for (int i = 0; i < numberOfNeighborLists_; ++i)
  {
    if (maxCutoff < cutoffs_[i]) maxCutoff = cutoffs_[i];
  }
  if (maxCutoff > *influenceDistance_)
  {
    LOG_ERROR("Model max(cutoffs) > influenceDistance.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (modelWillNotRequestNeighborsOfNoncontributingParticles_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "modelWillNotRequestNeighborsOfNoncontributingParticles.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  {
    using namespace KIM::MODEL_ROUTINE_NAME;
    for (unsigned int i = 0; i < requiredByAPI_ModelRoutines.size(); ++i)
    {
      ModelRoutineName const modelRoutineName
          = requiredByAPI_ModelRoutines.at(i);

      std::map<ModelRoutineName const,
               Function *,
               MODEL_ROUTINE_NAME::Comparator>::const_iterator funcResult
          = routineFunction_.find(modelRoutineName);

      if (funcResult->second == NULL)
      {
        LOG_ERROR("Model supplied Create() routine did not set pointer for "
                  + modelRoutineName.ToString() + ".");
        LOG_DEBUG("Exit 1=" + callString);
        return true;
      }
    }
  }

  if (parameterPointer_.size() > 0)
  {
    if (routineFunction_[MODEL_ROUTINE_NAME::Refresh] == NULL)
    {  // Must be provided if parameter pointers set
      LOG_ERROR("Model supplied Create() routine did not set pointer for "
                + MODEL_ROUTINE_NAME::Refresh.ToString() + ".");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }
  else
  {
    if (routineFunction_[MODEL_ROUTINE_NAME::Refresh] != NULL)
    {  // Must not be provided if no parameters set
      LOG_ERROR("Model supplied Create() routine set pointer for "
                + MODEL_ROUTINE_NAME::Refresh.ToString()
                + " but did not "
                  "set parameter pointer(s).");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    if (routineFunction_[MODEL_ROUTINE_NAME::WriteParameterizedModel] != NULL)
    {  // Must not be provided if no parameters set
      LOG_ERROR("Model supplied Create() routine set pointer for "
                + MODEL_ROUTINE_NAME::WriteParameterizedModel.ToString()
                + " but did not "
                  "set parameter pointer(s).");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
  }

  if (supportedSpecies_.empty())
  {
    LOG_ERROR("Model supplied Create() routine did not set SpeciesCode.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  // set numberingOffset_
  if (simulatorNumbering_ == modelNumbering_)
    numberingOffset_ = 0;
  else if (simulatorNumbering_ == NUMBERING::zeroBased)
    numberingOffset_ = 1;
  else
    numberingOffset_ = -1;

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::ModelDestroy()
{
#if DEBUG_VERBOSITY
  std::string const callString = "ModelDestroy().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::Destroy);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::Destroy);

  ModelDestroyFunction * CppDestroy
      = reinterpret_cast<ModelDestroyFunction *>(funcResult->second);
  KIM_ModelDestroyFunction * CDestroy
      = reinterpret_cast<KIM_ModelDestroyFunction *>(funcResult->second);
  typedef void ModelDestroyF(KIM_ModelDestroy * const, int * const);
  ModelDestroyF * FDestroy
      = reinterpret_cast<ModelDestroyF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  { error = CppDestroy(reinterpret_cast<KIM::ModelDestroy *>(&M)); }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelDestroy cM;
    cM.p = &M;
    error = CDestroy(&cM);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelDestroy cM;
    cM.p = &M;
    KIM_ModelDestroy cM_Handle;
    cM_Handle.p = &cM;
    FDestroy(&cM_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Destroy() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ModelComputeArgumentsCreate(
    ComputeArguments * const computeArguments) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "ModelComputeArgumentsCreate().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::ComputeArgumentsCreate);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::ComputeArgumentsCreate);

  ModelComputeArgumentsCreateFunction * CppComputeArgumentsCreate
      = reinterpret_cast<ModelComputeArgumentsCreateFunction *>(
          funcResult->second);
  KIM_ModelComputeArgumentsCreateFunction * CComputeArgumentsCreate
      = reinterpret_cast<KIM_ModelComputeArgumentsCreateFunction *>(
          funcResult->second);
  typedef void ModelComputeArgumentsCreateF(
      KIM_ModelCompute const * const,
      KIM_ModelComputeArgumentsCreate * const,
      int * const);
  ModelComputeArgumentsCreateF * FComputeArgumentsCreate
      = reinterpret_cast<ModelComputeArgumentsCreateF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void const * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  {
    error = CppComputeArgumentsCreate(
        reinterpret_cast<KIM::ModelCompute const *>(&M),
        reinterpret_cast<KIM::ModelComputeArgumentsCreate *>(computeArguments));
  }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelComputeArgumentsCreate cMcac;
    cMcac.p = computeArguments;
    error = CComputeArgumentsCreate(&cM, &cMcac);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelCompute cM_Handle;
    cM_Handle.p = &cM;
    KIM_ModelComputeArgumentsCreate cMcac;
    cMcac.p = computeArguments;
    KIM_ModelComputeArgumentsCreate cMcac_Handle;
    cMcac_Handle.p = &cMcac;
    FComputeArgumentsCreate(&cM_Handle, &cMcac_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied ComputeArgumentsCreate() routine "
              "returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ModelComputeArgumentsDestroy(
    ComputeArguments * const computeArguments) const
{
#if DEBUG_VERBOSITY
  std::string const callString = "ModelComputeArgumentsDestroy().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::ComputeArgumentsDestroy);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::ComputeArgumentsDestroy);

  ModelComputeArgumentsDestroyFunction * CppComputeArgumentsDestroy
      = reinterpret_cast<ModelComputeArgumentsDestroyFunction *>(
          funcResult->second);
  KIM_ModelComputeArgumentsDestroyFunction * CComputeArgumentsDestroy
      = reinterpret_cast<KIM_ModelComputeArgumentsDestroyFunction *>(
          funcResult->second);
  typedef void ModelComputeArgumentsDestroyF(
      KIM_ModelCompute const * const,
      KIM_ModelComputeArgumentsDestroy * const,
      int * const);
  ModelComputeArgumentsDestroyF * FComputeArgumentsDestroy
      = reinterpret_cast<ModelComputeArgumentsDestroyF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void const * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  {
    error = CppComputeArgumentsDestroy(
        reinterpret_cast<KIM::ModelCompute const *>(&M),
        reinterpret_cast<KIM::ModelComputeArgumentsDestroy *>(
            computeArguments));
  }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelComputeArgumentsDestroy cMcad;
    cMcad.p = computeArguments;
    error = CComputeArgumentsDestroy(&cM, &cMcad);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelCompute cM_Handle;
    cM_Handle.p = &cM;
    KIM_ModelComputeArgumentsDestroy cMcad;
    cMcad.p = computeArguments;
    KIM_ModelComputeArgumentsDestroy cMcad_Handle;
    cMcad_Handle.p = &cMcad;
    FComputeArgumentsDestroy(&cM_Handle, &cMcad_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied ComputeArgumentsDestroy() routine "
              "returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ModelCompute(
    ComputeArguments const * const computeArguments) const
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "ModelCompute(" + SPTR(computeArguments) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::Compute);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::Compute);

  ModelComputeFunction * CppCompute
      = reinterpret_cast<ModelComputeFunction *>(funcResult->second);
  KIM_ModelComputeFunction * CCompute
      = reinterpret_cast<KIM_ModelComputeFunction *>(funcResult->second);
  typedef void ModelComputeF(KIM_ModelCompute * const,
                             KIM_ModelComputeArguments const * const,
                             int * const);
  ModelComputeF * FCompute
      = reinterpret_cast<ModelComputeF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void const * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  {
    error = CppCompute(
        reinterpret_cast<KIM::ModelCompute const *>(&M),
        reinterpret_cast<KIM::ModelComputeArguments const *>(computeArguments));
  }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelComputeArguments cMca;
    cMca.p = reinterpret_cast<void *>(
        const_cast<KIM::ComputeArguments *>(computeArguments));
    error = CCompute(&cM, &cMca);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelCompute cM_Handle;
    cM_Handle.p = &cM;
    KIM_ModelComputeArguments cMca;
    cMca.p = reinterpret_cast<void *>(
        const_cast<KIM::ComputeArguments *>(computeArguments));
    KIM_ModelComputeArguments cMca_Handle;
    cMca_Handle.p = &cMca;
    FCompute(&cM_Handle, &cMca_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Compute() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ModelExtension(void * const extensionStructure)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "ModelExtension(" + SPTR(extensionStructure) + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::Extension);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::Extension);

  ModelExtensionFunction * CppExtension
      = reinterpret_cast<ModelExtensionFunction *>(funcResult->second);
  KIM_ModelExtensionFunction * CExtension
      = reinterpret_cast<KIM_ModelExtensionFunction *>(funcResult->second);
  typedef void ModelExtensionF(
      KIM_ModelExtension * const, void * const, int * const);
  ModelExtensionF * FExtension
      = reinterpret_cast<ModelExtensionF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  {
    error = CppExtension(reinterpret_cast<KIM::ModelExtension *>(&M),
                         extensionStructure);
  }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelExtension cM;
    cM.p = &M;
    error = CExtension(&cM, extensionStructure);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelExtension cM;
    cM.p = &M;
    KIM_ModelExtension cM_Handle;
    cM_Handle.p = &cM;
    FExtension(&cM_Handle, extensionStructure, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Extension() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ModelRefresh()
{
#if DEBUG_VERBOSITY
  std::string const callString = "ModelRefresh().";
#endif
  LOG_DEBUG("Enter  " + callString);

  if (parameterPointer_.size() == 0)
  {
    LOG_ERROR("Model does not have any adjustable parameters. "
              "No Refresh() function available.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::Refresh);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::Refresh);

  ModelRefreshFunction * CppRefresh
      = reinterpret_cast<ModelRefreshFunction *>(funcResult->second);
  KIM_ModelRefreshFunction * CRefresh
      = reinterpret_cast<KIM_ModelRefreshFunction *>(funcResult->second);
  typedef void ModelRefreshF(KIM_ModelRefresh * const, int * const);
  ModelRefreshF * FRefresh
      = reinterpret_cast<ModelRefreshF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  { error = CppRefresh(reinterpret_cast<KIM::ModelRefresh *>(&M)); }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelRefresh cM;
    cM.p = &M;
    error = CRefresh(&cM);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelRefresh cM;
    cM.p = &M;
    KIM_ModelRefresh cM_Handle;
    cM_Handle.p = &cM;
    FRefresh(&cM_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Refresh() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::ModelWriteParameterizedModel() const
{
#if DEBUG_VERBOSITY
  std::string const callString = "ModelWriteParameterizedModel().";
#endif
  LOG_DEBUG("Enter  " + callString);

  std::map<ModelRoutineName const, Function *, MODEL_ROUTINE_NAME::Comparator>::
      const_iterator funcResult
      = routineFunction_.find(MODEL_ROUTINE_NAME::WriteParameterizedModel);
  std::map<ModelRoutineName const,
           LanguageName,
           MODEL_ROUTINE_NAME::Comparator>::const_iterator langResult
      = routineLanguage_.find(MODEL_ROUTINE_NAME::WriteParameterizedModel);

  ModelWriteParameterizedModelFunction * CppWriteParameterizedModel
      = reinterpret_cast<ModelWriteParameterizedModelFunction *>(
          funcResult->second);
  KIM_ModelWriteParameterizedModelFunction * CWriteParameterizedModel
      = reinterpret_cast<KIM_ModelWriteParameterizedModelFunction *>(
          funcResult->second);
  typedef void ModelWriteParameterizedModelF(
      KIM_ModelWriteParameterizedModel * const, int * const);
  ModelWriteParameterizedModelF * FWriteParameterizedModel
      = reinterpret_cast<ModelWriteParameterizedModelF *>(funcResult->second);

  int error;
  struct Mdl
  {
    void const * p;
  };
  Mdl M;
  M.p = this;
  if (langResult->second == LANGUAGE_NAME::cpp)
  {
    error = CppWriteParameterizedModel(
        reinterpret_cast<KIM::ModelWriteParameterizedModel const *>(&M));
  }
  else if (langResult->second == LANGUAGE_NAME::c)
  {
    KIM_ModelWriteParameterizedModel cM;
    cM.p = &M;
    error = CWriteParameterizedModel(&cM);
  }
  else if (langResult->second == LANGUAGE_NAME::fortran)
  {
    KIM_ModelWriteParameterizedModel cM;
    cM.p = &M;
    KIM_ModelWriteParameterizedModel cM_Handle;
    cM_Handle.p = &cM;
    FWriteParameterizedModel(&cM_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR(
        "Model supplied WriteParameterizedModel() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
    return false;
  }
}

int ModelImplementation::InitializeStandAloneModel(
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit)
{
#if DEBUG_VERBOSITY
  std::string const callString
      = "InitializeStandAloneModel(" + requestedLengthUnit.ToString() + ", "
        + requestedEnergyUnit.ToString() + ", " + requestedChargeUnit.ToString()
        + ", " + requestedTemperatureUnit.ToString() + ", "
        + requestedTimeUnit.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = (!requestedLengthUnit.Known()) || (!requestedEnergyUnit.Known())
              || (!requestedChargeUnit.Known())
              || (!requestedTemperatureUnit.Known())
              || (!requestedTimeUnit.Known());
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  LanguageName languageName;
  Function * functionPointer = NULL;
  error = sharedLibrary_->GetCreateFunctionPointer(&languageName,
                                                   &functionPointer);
  if (error)
  {
    LOG_ERROR("Could not get ModelCreateFunctionPointer.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  routineLanguage_[MODEL_ROUTINE_NAME::Create] = languageName;
  routineRequired_[MODEL_ROUTINE_NAME::Create] = true;
  routineFunction_[MODEL_ROUTINE_NAME::Create] = functionPointer;

  ModelCreateFunction * CppCreate
      = reinterpret_cast<ModelCreateFunction *>(functionPointer);
  KIM_ModelCreateFunction * CCreate
      = reinterpret_cast<KIM_ModelCreateFunction *>(functionPointer);
  typedef void ModelCreateF(KIM_ModelCreate * const modelCreate,
                            KIM_LengthUnit const requestedLengthUnit,
                            KIM_EnergyUnit const requestedEnergyUnit,
                            KIM_ChargeUnit const requestedChargeUnit,
                            KIM_TemperatureUnit const requestedTemperatureUnit,
                            KIM_TimeUnit const requestedTimeUnit,
                            int * const);
  ModelCreateF * FCreate = reinterpret_cast<ModelCreateF *>(functionPointer);

  struct Mdl
  {
    void * p;
  };
  Mdl M;
  M.p = this;
  KIM_LengthUnit requestedLengthUnitC = makeLengthUnitC(requestedLengthUnit);
  KIM_EnergyUnit requestedEnergyUnitC = makeEnergyUnitC(requestedEnergyUnit);
  KIM_ChargeUnit requestedChargeUnitC = makeChargeUnitC(requestedChargeUnit);
  KIM_TemperatureUnit requestedTemperatureUnitC
      = makeTemperatureUnitC(requestedTemperatureUnit);
  KIM_TimeUnit requestedTimeUnitC = makeTimeUnitC(requestedTimeUnit);
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppCreate(reinterpret_cast<KIM::ModelCreate *>(&M),
                      requestedLengthUnit,
                      requestedEnergyUnit,
                      requestedChargeUnit,
                      requestedTemperatureUnit,
                      requestedTimeUnit);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    KIM_ModelCreate cM;
    cM.p = &M;
    error = CCreate(&cM,
                    requestedLengthUnitC,
                    requestedEnergyUnitC,
                    requestedChargeUnitC,
                    requestedTemperatureUnitC,
                    requestedTimeUnitC);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    KIM_ModelCreate cM;
    cM.p = &M;
    KIM_ModelCreate cM_Handle;
    cM_Handle.p = &cM;
    FCreate(&cM_Handle,
            requestedLengthUnitC,
            requestedEnergyUnitC,
            requestedChargeUnitC,
            requestedTemperatureUnitC,
            requestedTimeUnitC,
            &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (error)
  {
    LOG_ERROR("Model supplied Create() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit 0=" + callString);
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
#if DEBUG_VERBOSITY
  std::string const callString
      = "InitializeParameterizedModel(" + requestedLengthUnit.ToString() + ", "
        + requestedEnergyUnit.ToString() + ", " + requestedChargeUnit.ToString()
        + ", " + requestedTemperatureUnit.ToString() + ", "
        + requestedTimeUnit.ToString() + ").";
#endif
  LOG_DEBUG("Enter  " + callString);

#if ERROR_VERBOSITY
  int error = (!requestedLengthUnit.Known()) || (!requestedEnergyUnit.Known())
              || (!requestedChargeUnit.Known())
              || (!requestedTemperatureUnit.Known())
              || (!requestedTimeUnit.Known());
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
#endif

  // get driver name
  error = sharedLibrary_->GetDriverName(&modelDriverName_);
  if (error)
  {
    LOG_ERROR("Could not get Model Driver name.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // write parameter files to scratch space
  error = WriteParameterFiles();
  if (error)
  {
    LOG_ERROR("Could not write parameter files to scratch space.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // close model and open driver
  error = sharedLibrary_->Close();
  if (error)
  {
    LOG_ERROR("Could not close model shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  std::vector<std::string> sharedLibraryList;
  if (!findItem(OLD_KIM::KIM_MODEL_DRIVERS_DIR,
                modelDriverName_,
                &sharedLibraryList,
                NULL))
  {
    LOG_ERROR("Could not find model driver shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  error = sharedLibrary_->Open(sharedLibraryList[OLD_KIM::IE_FULLPATH]);
  if (error)
  {
    LOG_ERROR("Could not open model driver shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // check that it is a driver
  SharedLibrary::ITEM_TYPE itemType;
  error = sharedLibrary_->GetType(&itemType);
  if ((error) || (itemType != SharedLibrary::MODEL_DRIVER))
  {
    LOG_ERROR("Invalid model driver shared library.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  LanguageName languageName;
  Function * functionPointer = NULL;
  error = sharedLibrary_->GetCreateFunctionPointer(&languageName,
                                                   &functionPointer);
  if (error)
  {
    LOG_ERROR("Could not get ModelCreateFunctionPointer.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  routineLanguage_[MODEL_ROUTINE_NAME::Create] = languageName;
  routineRequired_[MODEL_ROUTINE_NAME::Create] = true;
  routineFunction_[MODEL_ROUTINE_NAME::Create] = functionPointer;

  ModelDriverCreateFunction * CppCreate
      = reinterpret_cast<ModelDriverCreateFunction *>(functionPointer);
  KIM_ModelDriverCreateFunction * CCreate
      = reinterpret_cast<KIM_ModelDriverCreateFunction *>(functionPointer);
  typedef void ModelDriverCreateF(
      KIM_ModelDriverCreate * const modelDriverCreate,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit,
      int * const);
  ModelDriverCreateF * FCreate
      = reinterpret_cast<ModelDriverCreateF *>(functionPointer);


  struct Mdl
  {
    void * p;
  };
  Mdl M;
  M.p = this;
  KIM_LengthUnit requestedLengthUnitC = makeLengthUnitC(requestedLengthUnit);
  KIM_EnergyUnit requestedEnergyUnitC = makeEnergyUnitC(requestedEnergyUnit);
  KIM_ChargeUnit requestedChargeUnitC = makeChargeUnitC(requestedChargeUnit);
  KIM_TemperatureUnit requestedTemperatureUnitC
      = makeTemperatureUnitC(requestedTemperatureUnit);
  KIM_TimeUnit requestedTimeUnitC = makeTimeUnitC(requestedTimeUnit);
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppCreate(reinterpret_cast<KIM::ModelDriverCreate *>(&M),
                      requestedLengthUnit,
                      requestedEnergyUnit,
                      requestedChargeUnit,
                      requestedTemperatureUnit,
                      requestedTimeUnit);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    KIM_ModelDriverCreate cM;
    cM.p = &M;
    error = CCreate(&cM,
                    requestedLengthUnitC,
                    requestedEnergyUnitC,
                    requestedChargeUnitC,
                    requestedTemperatureUnitC,
                    requestedTimeUnitC);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    KIM_ModelDriverCreate cM;
    cM.p = &M;
    KIM_ModelDriverCreate cM_Handle;
    cM_Handle.p = &cM;
    FCreate(&cM_Handle,
            requestedLengthUnitC,
            requestedEnergyUnitC,
            requestedChargeUnitC,
            requestedTemperatureUnitC,
            requestedTimeUnitC,
            &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }
  if (error)
  {
    LOG_ERROR("Model Driver supplied Create() routine returned error.");
    LOG_DEBUG("Exit 1=" + callString);
    return true;
  }

  // remove parameter files
  for (int i = 0; i < numberOfParameterFiles_; ++i)
  { remove(parameterFileNames_[i].c_str()); }
  // clear out parameter file stuff
  numberOfParameterFiles_ = -1;
  parameterFileNames_.clear();

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::WriteParameterFiles()
{
#if DEBUG_VERBOSITY
  std::string const callString = "WriteParameterFiles().";
#endif
  LOG_DEBUG("Enter  " + callString);

  sharedLibrary_->GetNumberOfParameterFiles(&numberOfParameterFiles_);
  std::vector<unsigned char const *> parameterFileStrings;
  std::vector<unsigned int> parameterFileStringLengths;
  for (int i = 0; i < numberOfParameterFiles_; ++i)
  {
    unsigned char const * strPtr;
    unsigned int length;
    int error = sharedLibrary_->GetParameterFile(i, NULL, &length, &strPtr);
    if (error)
    {
      LOG_ERROR("Could not get parameter file data.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    parameterFileStrings.push_back(strPtr);
    parameterFileStringLengths.push_back(length);
  }

  static char const fileNameString[] = "kim-model-parameter-file-XXXXXXXXXXXX";
  for (int i = 0; i < numberOfParameterFiles_; ++i)
  {
    std::stringstream templateString;
    templateString << P_tmpdir << "/" << fileNameString;
    char * cstr = strdup(templateString.str().c_str());
    int fileid = mkstemp(cstr);
    if (fileid == -1)
    {
      free(cstr);
      LOG_ERROR("Could not create a secure temporary file.");
      LOG_DEBUG("Exit 1=" + callString);
      return true;
    }
    parameterFileNames_.push_back(cstr);
    free(cstr);

    FILE * fl = fdopen(fileid, "w");
    fwrite(parameterFileStrings[i],
           sizeof(unsigned char),
           parameterFileStringLengths[i],
           fl);
    fclose(fl);  // also closed the fileid
  }

  LOG_DEBUG("Exit 0=" + callString);
  return false;
}

int ModelImplementation::IsCIdentifier(std::string const & id) const
{
  std::string const numbers("0123456789");
  std::string const cIdentifierChars("_"
                                     "abcdefghijklmnopqrstuvwxyz"
                                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                     + numbers);

  if (id.length() == 0) return false;
  if (std::string::npos != id.find_first_not_of(cIdentifierChars)) return false;
  if (0 == id.find_first_of(numbers)) return false;

  return true;
}
}  // namespace KIM
