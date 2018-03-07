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

#include <sstream>
#include <iomanip>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_H_
extern "C"
{
#include "KIM_UnitSystem.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_CREATE_H_
extern "C"
{
#include "KIM_ModelCreate.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_DRIVER_CREATE_H_
extern "C"
{
#include "KIM_ModelDriverCreate.h"
}  // extern "C"
#endif

#ifndef KIM_MODEL_REFRESH_H_
extern "C"
{
#include "KIM_ModelRefresh.h"
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
extern std::vector<ArgumentName> const requiredByAPI_Arguments;
}  // namespace ARGUMENT_NAME

namespace CALLBACK_NAME
{
extern std::vector<CallbackName> const requiredByAPI_Callbacks;
}  // namespace CALLBACK_NAME
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

// log helpers
#define SNUM( x ) static_cast<std::ostringstream &>(    \
    std::ostringstream() << std::dec << x).str()
#define SPTR( x ) static_cast<std::ostringstream &>(                    \
    std::ostringstream() << static_cast<void const * const>(x) ).str()
#define SFUNC( x ) static_cast<std::ostringstream &>(           \
    std::ostringstream() << static_cast<func *>(x)).str()


#include "KIM_ModelImplementationLogMacros.hpp"
namespace KIM
{
// Forward declarations
class ModelCreate;
class ModelDriverCreate;
class ModelRefresh;
class ModelCompute;
class ModelDestroy;

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
  if (error)
  {
    return true;
  }

  ModelImplementation * pModelImplementation;
  pModelImplementation = new ModelImplementation(new ModelLibrary(pLog), pLog);
  pModelImplementation->LogEntry(
      LOG_VERBOSITY::debug,
      "Created Log and ModelImplementation objects after enter Create().",
      __LINE__, __FILE__);

  error = pModelImplementation->ModelCreate(
      numbering, requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
      requestedTemperatureUnit, requestedTimeUnit, modelName);
  if (error)
  {
    pModelImplementation->LogEntry(
        LOG_VERBOSITY::error,
        "ModelCreate routine returned with an error.",
        __LINE__, __FILE__);
    delete pModelImplementation;  // also deletes pLog
    return true;
  }

  LengthUnit finalLengthUnit;
  EnergyUnit finalEnergyUnit;
  ChargeUnit finalChargeUnit;
  TemperatureUnit finalTemperatureUnit;
  TimeUnit finalTimeUnit;
  pModelImplementation->GetUnits(&finalLengthUnit, &finalEnergyUnit,
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
    pModelImplementation->LogEntry(LOG_VERBOSITY::debug,
                                   "Accepted requested units.",
                                   __LINE__, __FILE__);
    *requestedUnitsAccepted = true;
  }
  else
  {
    pModelImplementation->LogEntry(LOG_VERBOSITY::debug,
                                   "Rejected requested units.",
                                   __LINE__, __FILE__);
    *requestedUnitsAccepted = false;
  }

  *modelImplementation = pModelImplementation;
  pModelImplementation->LogEntry(LOG_VERBOSITY::debug,
                                 "Exit Create().",
                                 __LINE__, __FILE__);
  return false;
}


void ModelImplementation::Destroy(
    ModelImplementation ** const modelImplementation)
{
  (*modelImplementation)->LogEntry(LOG_VERBOSITY::debug,
                                "Enter Destroy().",
                                __LINE__, __FILE__);

  int error = (*modelImplementation)->ModelDestroy();
  if (error)
  {
    (*modelImplementation)->LogEntry(
        KIM::LOG_VERBOSITY::error,
        "ModelDestroy routine returned with an error.",
        __LINE__, __FILE__);
  }

  (*modelImplementation)->LogEntry(
      LOG_VERBOSITY::debug,
      "Destroying ModelImplementation object and exit Destroy().",
      __LINE__, __FILE__);
  delete *modelImplementation;
  *modelImplementation = 0;
}

void ModelImplementation::SetInfluenceDistancePointer(
    double const * const influenceDistance)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetInfluenceDistancePointer("
      + SPTR(influenceDistance) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if (influenceDistance == NULL)
    LOG_ERROR("Null pointer provided for InfluenceDistancePotiner.");
#endif

  influenceDistance_ = influenceDistance;

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::GetInfluenceDistance(
    double * const influenceDistance) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetInfluenceDistance("
      + SPTR(influenceDistance) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  *influenceDistance = *influenceDistance_;

  LOG_DEBUG("Exit " + callString);
}


void ModelImplementation::SetNeighborListCutoffsPointer(
    int const numberOfCutoffs, double const * const cutoffs)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetNeighborListCutoffsPointer("
      + SNUM(numberOfCutoffs) + ", " + SPTR(cutoffs) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if (numberOfCutoffs < 1)
    LOG_ERROR("Number of neighbor list cutoffs, " + SNUM(numberOfCutoffs)
              + ", must be >= 1.");
  if (cutoffs == NULL)
    LOG_ERROR("Null pointer provided for cutoffs.");
#endif

  numberOfCutoffs_ = numberOfCutoffs;
  cutoffs_ = cutoffs;

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::GetNeighborListCutoffsPointer(
    int * const numberOfCutoffs, double const ** const cutoffs) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetNeighborListCutoffsPointer("
      + SPTR(numberOfCutoffs) + ", " + SPTR(cutoffs) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  if (numberOfCutoffs != NULL)
    *numberOfCutoffs = numberOfCutoffs_;
  if (cutoffs != NULL)
    *cutoffs = cutoffs_;

  LOG_DEBUG("Exit " + callString);
}

int ModelImplementation::SetRefreshPointer(LanguageName const languageName,
                                           func * const fptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetRefreshPointer("
      + languageName.String() + ", " + SFUNC(fptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(languageName);
  if (error)
  {
    LOG_ERROR("Invalid arguemnts.");
    return true;
  }
#endif

  refreshLanguage_ = languageName;
  refreshFunction_ = fptr;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetDestroyPointer(LanguageName const languageName,
                                           func * const fptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetDestroyPointer("
      + languageName.String() + ", " + SFUNC(fptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(languageName);
  if (error)
  {
    LOG_ERROR("Invalid arguemnts.");
    return true;
  }
#endif

  destroyLanguage_ = languageName;
  destroyFunction_ = fptr;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetComputePointer(LanguageName const languageName,
                                           func * const fptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetComputePointer("
      + languageName.String() + ", " + SFUNC(fptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(languageName);
  if (error)
  {
    LOG_ERROR("Invalid arguemnts.");
    return true;
  }
#endif

  computeLanguage_ = languageName;
  computeFunction_ = fptr;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetSpeciesCode(SpeciesName const speciesName,
                                        int const code)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetSpeciesCode("
      + speciesName.String() + ", " + SNUM(code) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(speciesName);
  if (error)
  {
    LOG_ERROR("Invalid arguemnts.");
    return true;
  }
#endif

  supportedSpecies_[speciesName] = code;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetSpeciesSupportAndCode(
    KIM::SpeciesName const speciesName,
    int * const speciesIsSupported,
    int * const code) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetSpeciesSupportAndCode("
      + speciesName.String() + ", " + SPTR(speciesIsSupported)
      + ", " + SPTR(code) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(speciesName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    return true;
  }
#endif

  std::map<SpeciesName const, int, CALLBACK_NAME::Comparator>::const_iterator
      result = supportedSpecies_.find(speciesName);
  if (result == supportedSpecies_.end())
  {
    LOG_DEBUG("Species is not supported.");
    *speciesIsSupported = false;
  }
  else
  {
    LOG_DEBUG("Species is supported.");
    *speciesIsSupported = true;
    if (code != NULL)
      *code = result->second;
  }

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetArgumentSupportStatus(
    ArgumentName const argumentName, SupportStatus const supportStatus)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetArgumentSupportStatus("
      + argumentName.String() + ", " + supportStatus.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName) || Validate(supportStatus);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((argumentSupportStatus_[argumentName]
       == SUPPORT_STATUS::requiredByAPI)
      && (supportStatus != SUPPORT_STATUS::requiredByAPI))
  {
    LOG_ERROR("Argument '" + argumentName.String()
              + "' SupportStatus is 'requiredByAPI' and cannot be changed.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  argumentSupportStatus_[argumentName] = supportStatus;

  // initialize pointer if not already done
  if (supportStatus != SUPPORT_STATUS::notSupported)
  {
    std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
        const_iterator result = argumentPointer_.find(argumentName);

    if (result == argumentPointer_.end())
    {
      LOG_DEBUG("Initialize argument pointer.");
      argumentPointer_[argumentName] = NULL;
    }
  }

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetArgumentSupportStatus(
    ArgumentName const argumentName, SupportStatus * const supportStatus)
    const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetArgumentSupportStatus("
      + argumentName.String() + ", " + SPTR(supportStatus) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentSupportStatus_.find(argumentName);
  *supportStatus = result->second;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetCallbackSupportStatus(
    CallbackName const callbackName, SupportStatus const supportStatus)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetCallbackSupportStatus("
      + callbackName.String() + ", " + supportStatus.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(callbackName) || Validate(supportStatus);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((callbackSupportStatus_[callbackName]
       == SUPPORT_STATUS::requiredByAPI)
      && (supportStatus != SUPPORT_STATUS::requiredByAPI))
  {
    LOG_ERROR("Callback '" + callbackName.String()
              + "' SupportStatus is 'requiredByAPI' and cannot "
              "be changed.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  callbackSupportStatus_[callbackName] = supportStatus;

  // initialize pointer if not already done
  if (supportStatus != SUPPORT_STATUS::notSupported)
  {
    std::map<CallbackName const, func *, CALLBACK_NAME::Comparator>::
        const_iterator result = callbackFunctionPointer_.find(callbackName);

    if (result == callbackFunctionPointer_.end())
    {
      LOG_DEBUG("Initialize callback pointer.");
      callbackLanguage_[callbackName] = LANGUAGE_NAME::cpp;  // place holder
      callbackFunctionPointer_[callbackName] = NULL;
      callbackDataObjectPointer_[callbackName] = NULL;
    }
  }

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetCallbackSupportStatus(
    CallbackName const callbackName, SupportStatus * const supportStatus) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetCallbackSupportStatus("
      + callbackName.String() + ", " + SPTR(supportStatus) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(callbackName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<CallbackName const, SupportStatus, CALLBACK_NAME::Comparator>::
      const_iterator result = callbackSupportStatus_.find(callbackName);
  *supportStatus = result->second;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetModelNumbering(Numbering const numbering)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetModelNumbering("
      + numbering.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(numbering);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  modelNumbering_ = numbering;
  numberingHasBeenSet_ = true;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetSimulatorNumbering(Numbering const numbering)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetSimulatorNumbering("
      + numbering.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(numbering);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  simulatorNumbering_ = numbering;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetUnits(LengthUnit const lengthUnit,
                                  EnergyUnit const energyUnit,
                                  ChargeUnit const chargeUnit,
                                  TemperatureUnit const temperatureUnit,
                                  TimeUnit const timeUnit)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetUnits("
      + lengthUnit.String() + ", "
      + energyUnit.String() + ", "
      + chargeUnit.String() + ", "
      + temperatureUnit.String() + ", "
      + timeUnit.String()
      + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error =
      Validate(lengthUnit) ||
      Validate(energyUnit) ||
      Validate(chargeUnit) ||
      Validate(temperatureUnit) ||
      Validate(timeUnit);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (lengthUnit == LENGTH_UNIT::unused)
  {
    LOG_ERROR("Models cannot specify 'unused' for LengthUnit");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (energyUnit == ENERGY_UNIT::unused)
  {
    LOG_ERROR("Models cannot specify 'unused' for EnergyUnit");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  lengthUnit_ = lengthUnit;
  energyUnit_ = energyUnit;
  chargeUnit_ = chargeUnit;
  temperatureUnit_ = temperatureUnit;
  timeUnit_ = timeUnit;

  unitsHaveBeenSet_ = true;

  LOG_DEBUG("Exit " + callString);
  return false;
}

void ModelImplementation::GetUnits(LengthUnit * const lengthUnit,
                                   EnergyUnit * const energyUnit,
                                   ChargeUnit * const chargeUnit,
                                   TemperatureUnit * const temperatureUnit,
                                   TimeUnit * const timeUnit) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetUnits("
      + SPTR(lengthUnit) + ", "
      + SPTR(energyUnit) + ", "
      + SPTR(chargeUnit) + ", "
      + SPTR(temperatureUnit) + ", "
      + SPTR(timeUnit) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  if (lengthUnit != NULL)
    *lengthUnit = lengthUnit_;
  if (energyUnit != NULL)
    *energyUnit = energyUnit_;
  if (chargeUnit != NULL)
    *chargeUnit = chargeUnit_;
  if (temperatureUnit != NULL)
    *temperatureUnit = temperatureUnit_;
  if (timeUnit != NULL)
    *timeUnit = timeUnit_;

  LOG_DEBUG("Exit " + callString);
}

int ModelImplementation::GetNumberOfParameterFiles(
    int * const numberOfParameterFiles) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetNumberOfParameterFiles("
      + SPTR(numberOfParameterFiles) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if (modelType_ != ModelLibrary::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("Only parameterized models have parameter files.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  *numberOfParameterFiles = numberOfParameterFiles_;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetParameterFileName(
    int const index, std::string const ** const parameterFileName) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetParameterFileName("
      + SNUM(index) + ", " + SPTR(parameterFileName) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if (modelType_ != ModelLibrary::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("Only parameterized models have parameter files.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((index < 0) || (index >= numberOfParameterFiles_))
  {
    LOG_ERROR("Invalid parameter file index, " + SNUM(index) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  *parameterFileName = &(parameterFileNames_[index]);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetParameterPointer(int const extent, int * const ptr,
                                             std::string const & description)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetParameterPointer("
      + SNUM(extent) + ", " + SPTR(ptr) + ", '" + description + "').";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if (extent == 0)
  {
    LOG_ERROR("Extent, " + SNUM(extent) + ", must be > 0.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  if (ptr == NULL)
  {
    LOG_ERROR("Null pointer provided for parameter.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Integer);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetParameterPointer(int const extent,
                                             double * const ptr,
                                             std::string const & description)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetParameterPointer("
      + SNUM(extent) + ", " + SPTR(ptr) + ", '" + description + "').";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if (extent == 0)
  {
    LOG_ERROR("Extent, " + SNUM(extent) + ", must be > 0.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  if (ptr == NULL)
  {
    LOG_ERROR("Null pointer provided for parameter.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Double);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  LOG_DEBUG("Exit " + callString);
  return false;
}

void ModelImplementation::GetNumberOfParameters(int * const numberOfParameters)
    const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetNumberOfParameters("
      + SPTR(numberOfParameters) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  *numberOfParameters = parameterPointer_.size();

  LOG_DEBUG("Exit " + callString);
}

int ModelImplementation::GetParameterDataTypeExtentAndDescription(
    int const parameterIndex, DataType * const dataType, int * const extent,
    std::string const ** const description) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString
      = "GetParameterDataTypeExtentAndDescription("
      + SNUM(parameterIndex) + ", " + SPTR(dataType) + ", "
      + SPTR(extent) + ", " + SPTR(description) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if ((parameterIndex < 0) ||
      (static_cast<unsigned int>(parameterIndex)
       >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  if (dataType != NULL)
    *dataType = parameterDataType_[parameterIndex];
  if (extent != NULL)
    *extent = parameterExtent_[parameterIndex];
  if (description != NULL)
    *description = &(parameterDescription_[parameterIndex]);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      int * const parameterValue) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetParameter("
      + SNUM(parameterIndex) + ", " + SNUM(arrayIndex) + ", "
      + SPTR(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if ((parameterIndex < 0) ||
      (static_cast<unsigned int>(parameterIndex)
       >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  *parameterValue = reinterpret_cast<int const *>
      (parameterPointer_[parameterIndex])[arrayIndex];

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      double * const parameterValue) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetParameter("
      + SNUM(parameterIndex) + ", " + SNUM(arrayIndex) + ", "
      + SPTR(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if ((parameterIndex < 0) ||
      (static_cast<unsigned int>(parameterIndex)
       >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  *parameterValue = reinterpret_cast<double const *>
      (parameterPointer_[parameterIndex])[arrayIndex];

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      int const parameterValue)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetParameter("
      + SNUM(parameterIndex) + ", " + SNUM(arrayIndex) + ", "
      + SNUM(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if ((parameterIndex < 0) ||
      (static_cast<unsigned int>(parameterIndex)
       >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  reinterpret_cast<int *>(parameterPointer_[parameterIndex])[arrayIndex]
      = parameterValue;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetParameter(int const parameterIndex,
                                      int const arrayIndex,
                                      double const parameterValue)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetParameter("
      + SNUM(parameterIndex) + ", " + SNUM(arrayIndex) + ", "
      + SNUM(parameterValue) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if ((parameterIndex < 0) ||
      (static_cast<unsigned int>(parameterIndex)
       >= parameterPointer_.size()))
  {
    LOG_ERROR("Invalid parameter index, " + SNUM(parameterIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if ((arrayIndex < 0) || (arrayIndex >= parameterExtent_[parameterIndex]))
  {
    LOG_ERROR("Invalid parameter arrayIndex, " + SNUM(arrayIndex) + ".");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  reinterpret_cast<double *>(parameterPointer_[parameterIndex])[arrayIndex]
      = parameterValue;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetArgumentPointer(ArgumentName const argumentName,
                                            int const * const ptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetArgumentPointer("
      + argumentName.String() + ", " + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentSupportStatus_.find(argumentName);
  if (result->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value cannot be set for Argument '"
              + argumentName.String() + "' which is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  argumentPointer_[argumentName]
      = reinterpret_cast<void *>(const_cast<int *>(ptr));

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetArgumentPointer(ArgumentName const argumentName,
                                            double const * const ptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetArgumentPointer("
      + argumentName.String() + ", " + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentSupportStatus_.find(argumentName);
  if (result->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value cannot be set for Arguments '"
              + argumentName.String() + "' which is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  argumentPointer_[argumentName]
      = reinterpret_cast<void *>(const_cast<double *>(ptr));

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            int const ** const ptr) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetArgumentPointer("
      + argumentName.String() + ", " + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator statusResult = argumentSupportStatus_.find(argumentName);
  if (statusResult->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value does not exist for Argument '"
              + (statusResult->first).String()
              + "' which is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentPointer_.find(argumentName);
  *ptr = reinterpret_cast<int const *>(result->second);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            int ** const ptr) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetArgumentPointer("
      + argumentName.String() + ", " + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator statusResult = argumentSupportStatus_.find(argumentName);
  if (statusResult->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value does not exist for Argument '"
              + (statusResult->first).String()
              + "' which is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentPointer_.find(argumentName);

  *ptr = reinterpret_cast<int *>(result->second);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            double const ** const ptr) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetArgumentPointer("
      + argumentName.String() + ", " + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator statusResult = argumentSupportStatus_.find(argumentName);
  if (statusResult->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value does not exist for Argument '"
              + (statusResult->first).String()
              + "' which is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentPointer_.find(argumentName);

  *ptr = reinterpret_cast<double const *>(result->second);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            double ** const ptr) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetArgumentPointer("
      + argumentName.String() + ", " + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(argumentName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
      const_iterator statusResult = argumentSupportStatus_.find(argumentName);
  if (statusResult->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value does not exist for Argument '"
              + (statusResult->first).String()
              + "' which is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
      const_iterator result = argumentPointer_.find(argumentName);

  *ptr = reinterpret_cast<double *>(result->second);

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::SetCallbackPointer(CallbackName const callbackName,
                                            LanguageName const languageName,
                                            func * const fptr,
                                            void const * const dataObject)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetCallbackPointer("
      + callbackName.String() + ", " + languageName.String()
      + ", " + SFUNC(fptr) + ", " + SPTR(dataObject) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error =
      Validate(callbackName) ||
      Validate(languageName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  std::map<CallbackName const, SupportStatus, CALLBACK_NAME::Comparator>::
      const_iterator result = callbackSupportStatus_.find(callbackName);

  if (result->second == SUPPORT_STATUS::notSupported)
  {
    LOG_ERROR("Pointer value cannot be set for Callback '"
              + callbackName.String() + "' that is 'notSupported'.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  callbackLanguage_[callbackName] = languageName;
  callbackFunctionPointer_[callbackName] = fptr;
  callbackDataObjectPointer_[callbackName] = dataObject;

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::IsCallbackPresent(
    CallbackName const callbackName, int * const present) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "IsCallbackPresent("
      + callbackName.String() + ", " + SPTR(present) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error = Validate(callbackName);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return false;
  }
#endif

  std::map<CallbackName const, func *, CALLBACK_NAME::Comparator>::
      const_iterator result = callbackFunctionPointer_.find(callbackName);

  if (result->second == NULL)
  {
    *present = false;
  }
  else
  {
    *present = true;
  }

  LOG_DEBUG("Exit " + callString);
  return false;
}

struct KIM_ModelCompute
{
  void * p;
};

int ModelImplementation::Compute() const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "Compute().";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  // Check that all required arguments are present
  for (std::map<ArgumentName const, SupportStatus,
           ARGUMENT_NAME::Comparator>::const_iterator
           itr = argumentSupportStatus_.begin();
       itr != argumentSupportStatus_.end(); ++itr)
  {
    if ((itr->second == SUPPORT_STATUS::requiredByAPI) ||
        (itr->second == SUPPORT_STATUS::required))
    {
      if (argumentPointer_.find(itr->first)->second == NULL)
      {
        LOG_ERROR("Required Argument '" + (itr->first).String()
                  + "' is not present.");
        LOG_DEBUG("Exit " + callString);
        return true;
      }
    }
  }

  // Check that all required callbacks are present
  for (std::map<CallbackName const, SupportStatus,
           CALLBACK_NAME::Comparator>::const_iterator
           itr = callbackSupportStatus_.begin();
       itr != callbackSupportStatus_.end(); ++itr)
  {
    if ((itr->second == SUPPORT_STATUS::requiredByAPI) ||
        (itr->second == SUPPORT_STATUS::required))
    {
      if (callbackFunctionPointer_.find(itr->first)->second == NULL)
      {
        LOG_ERROR("Required Callback '" + (itr->first).String()
                  + "' is not present.");
        LOG_DEBUG("Exit " + callString);
        return true;
      }
    }
  }
#endif

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
  if (computeLanguage_ == LANGUAGE_NAME::cpp)
  {
    error = CppCompute(reinterpret_cast<KIM::ModelCompute *>(&M));
  }
  else if (computeLanguage_ == LANGUAGE_NAME::c)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    error = CCompute(&cM);
  }
  else if (computeLanguage_ == LANGUAGE_NAME::fortran)
  {
    KIM_ModelCompute cM;
    cM.p = &M;
    KIM_ModelCompute cM_Handle;
    cM_Handle.p = &cM;
    FCompute(&cM_Handle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Compute() routine returned error.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit " + callString);
    return false;
  }
}

struct KIM_ModelRefresh
{
  void * p;
};

int ModelImplementation::ClearInfluenceDistanceAndCutoffsThenRefreshModel()
{
#if LOG_DEBUG_SOURCE
  std::string const callString =
      "ClearInfluenceDistanceAndCutoffsThenRefreshModel().";
#endif
  LOG_DEBUG("Enter " + callString);

  influenceDistance_ = 0;
  numberOfCutoffs_ = 0;
  cutoffs_ = 0;

  typedef int ModelRefreshCpp(KIM::ModelRefresh * const);
  ModelRefreshCpp * CppRefresh
      = reinterpret_cast<ModelRefreshCpp *>(refreshFunction_);
  typedef int ModelRefreshC(KIM_ModelRefresh * const);
  ModelRefreshC * CRefresh
      = reinterpret_cast<ModelRefreshC *>(refreshFunction_);
  typedef void ModelRefreshF(KIM_ModelRefresh * const, int * const);
  ModelRefreshF * FRefresh
      = reinterpret_cast<ModelRefreshF *>(refreshFunction_);

  int error;
  struct Mdl {void * p;};
  Mdl M;
  M.p = this;
  if (refreshLanguage_ == LANGUAGE_NAME::cpp)
  {
    error = CppRefresh(
        reinterpret_cast<KIM::ModelRefresh *>(&M));
  }
  else if (refreshLanguage_ == LANGUAGE_NAME::c)
  {
    KIM_ModelRefresh cM;
    cM.p = &M;
    error = CRefresh(&cM);
  }
  else if (refreshLanguage_ == LANGUAGE_NAME::fortran)
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
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Refresh() routine returned error.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  else
  {
#if LOG_ERROR_SOURCE
    // error checking
    if (influenceDistance_ == NULL)
    {
      LOG_ERROR("Model supplied Refresh() routine did not set "
                "InfluenceDistancePointer");
      LOG_DEBUG("Exit " + callString);
      return true;
    }
    if (cutoffs_ == NULL)
    {
      LOG_ERROR("Model supplied Refresh() routine did not "
                "set CutoffsPointer");
      LOG_DEBUG("Exit " + callString);
      return true;
    }
#endif
    LOG_DEBUG("Exit " + callString);
    return false;
  }
}

int ModelImplementation::GetNeighborList(int const neighborListIndex,
                                         int const particleNumber,
                                         int * const numberOfNeighbors,
                                         int const ** const neighborsOfParticle)
    const
{
  // No debug logging for callbacks: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "GetNeighborList("
  //       + SNUM(neighborListIndex) + ", " + SNUM(particleNumber) + ", "
  //       + SPTR(numberOfNeighbors) + ", " + SPTR(neighborsOfParticle) + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  if ((neighborListIndex < 0) || (neighborListIndex >= numberOfCutoffs_))
  {
    LOG_ERROR("Invalid neighborListIndex, " + SNUM(neighborListIndex)
              + ".");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }

  int zeroBasedParticleNumber = particleNumber +
      ((NUMBERING::zeroBased == modelNumbering_) ? 0 : -1);
  std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
      const_iterator pointerResult
      = argumentPointer_.find(ARGUMENT_NAME::numberOfParticles);
  int const * numberOfParticles
      = reinterpret_cast<int const *>(pointerResult->second);
  if ((zeroBasedParticleNumber < 0) ||
      (zeroBasedParticleNumber >= *(numberOfParticles)))
  {
    LOG_ERROR("Invalid particleNumber, " + SNUM(zeroBasedParticleNumber)
              + ".");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  std::map<CallbackName const, LanguageName, CALLBACK_NAME::Comparator>::
      const_iterator languageResult
      = callbackLanguage_.find(CALLBACK_NAME::GetNeighborList);

  LanguageName const languageName = languageResult->second;
  void const * dataObject
      = (callbackDataObjectPointer_.find(CALLBACK_NAME::GetNeighborList))
      ->second;

  func * functionPointer
      = (callbackFunctionPointer_.find(CALLBACK_NAME::GetNeighborList))->second;
  typedef int GetNeighborListCpp(void const * const dataObject,
                                 int const neighborListIndex,
                                 int const particleNumber,
                                 int * const numberOfNeighbors,
                                 int const ** const neighborsOfParticle);
  GetNeighborListCpp * CppGetNeighborList
      = reinterpret_cast<GetNeighborListCpp *>(functionPointer);
  typedef int GetNeighborListC(void const * const dataObject,
                               int const neighborListIndex,
                               int const particleNumber,
                               int * const numberOfNeighbors,
                               int const ** const neighborsOfParticle);
  GetNeighborListC * CGetNeighborList
      = reinterpret_cast<GetNeighborListC *>(functionPointer);
  typedef void GetNeighborListF(void const * const dataObject,
                                int const neighborListIndex,
                                int const particleNumber,
                                int * const numberOfNeighbors,
                                int const ** const neighborsOfParticle,
                                int * const ierr);
  GetNeighborListF * FGetNeighborList
      = reinterpret_cast<GetNeighborListF *>(functionPointer);


  int simulatorParticleNumber = particleNumber +
      ((simulatorNumbering_ == modelNumbering_) ? 0 : -numberingOffset_);
  int const * simulatorNeighborsOfParticle;
  int error;
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppGetNeighborList(dataObject, neighborListIndex,
                               simulatorParticleNumber, numberOfNeighbors,
                               &simulatorNeighborsOfParticle);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    error = CGetNeighborList(dataObject, neighborListIndex,
                             simulatorParticleNumber,
                             numberOfNeighbors, &simulatorNeighborsOfParticle);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    FGetNeighborList(dataObject, neighborListIndex+1, simulatorParticleNumber,
                     numberOfNeighbors, &simulatorNeighborsOfParticle, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Simulator supplied GetNeighborList() routine returned error.");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }

  // account for numbering differences if needed
  if (simulatorNumbering_ != modelNumbering_)
  {
    // LOG_DEBUG("Numbering conversion is required.");

    std::vector<int> & list = getNeighborListStorage_[neighborListIndex];
    list.resize(*numberOfNeighbors);
    for (int i=0; i<*numberOfNeighbors; ++i)
      list[i] = simulatorNeighborsOfParticle[i] + numberingOffset_;

    *neighborsOfParticle = list.data();
  }
  else
  {
    // LOG_DEBUG("Numbering conversion is not required.");

    *neighborsOfParticle = simulatorNeighborsOfParticle;
  }

  // LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::ProcessDEDrTerm(double const de, double const r,
                                         double const * const dx,
                                         int const i, int const j) const
{
  // No debug logging for callbacks: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "ProcessDEDrTerm("
  //       + SNUM(de) + ", " + SNUM(r) + ", " + SPTR(dx) + ", "
  //       + SNUM(i) + ", " + SNUM(j) + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  std::map<CallbackName const, LanguageName, CALLBACK_NAME::Comparator>::
      const_iterator languageResult
      = callbackLanguage_.find(CALLBACK_NAME::ProcessDEDrTerm);

  LanguageName languageName = languageResult->second;
  void const * dataObject
      = (callbackDataObjectPointer_.find(CALLBACK_NAME::ProcessDEDrTerm))
      ->second;

  func * functionPointer
      = (callbackFunctionPointer_.find(CALLBACK_NAME::ProcessDEDrTerm))->second;
  typedef int ProcessDEDrTermCpp(void const * const dataObject, double const de,
                                 double const r, double const * const dx,
                                 int const i, int const j);
  ProcessDEDrTermCpp * CppProcess_dEdr
      = reinterpret_cast<ProcessDEDrTermCpp *>(functionPointer);
  typedef int ProcessDEDrTermC(void const * const dataObject, double const de,
                               double const r, double const * const dx,
                               int const i, int const j);
  ProcessDEDrTermC * CProcess_dEdr
      = reinterpret_cast<ProcessDEDrTermC *>(functionPointer);
  typedef void ProcessDEDrTermF(void const * const dataObject, double const de,
                                double const r, double const * const dx,
                                int const i, int const j, int * const ierr);
  ProcessDEDrTermF * FProcess_dEdr
      = reinterpret_cast<ProcessDEDrTermF *>(functionPointer);

  int offset
      = ((simulatorNumbering_ == modelNumbering_) ? 0 : -numberingOffset_);
  int simulatorI = i + offset;
  int simulatorJ = j + offset;

  int error;
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppProcess_dEdr(dataObject, de, r, dx, simulatorI, simulatorJ);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    error = CProcess_dEdr(dataObject, de, r, dx, simulatorI, simulatorJ);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    FProcess_dEdr(dataObject, de, r, dx, simulatorI, simulatorJ, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Simulator supplied ProcessDEDrTerm() routine returned error.");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }
  else
  {
    // LOG_DEBUG("Exit " + callString);
    return false;
  }
}

int ModelImplementation::ProcessD2EDr2Term(double const de,
                                           double const * const r,
                                           double const * const dx,
                                           int const * const i,
                                           int const * const j) const
{
  // No debug logging for callbacks: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "ProcessD2EDr2Term("
  //       + SNUM(de) + ", " + SPTR(r) + ", " + SPTR(dx) + ", "
  //       + SPTR(i) + ", " + SPTR(j) + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);
  std::map<CallbackName const, LanguageName, CALLBACK_NAME::Comparator>::
      const_iterator languageResult
      = callbackLanguage_.find(CALLBACK_NAME::ProcessD2EDr2Term);

  LanguageName languageName = languageResult->second;
  void const * dataObject = (callbackDataObjectPointer_
                             .find(CALLBACK_NAME::ProcessD2EDr2Term))->second;

  func * functionPointer
      = (callbackFunctionPointer_.find(CALLBACK_NAME::ProcessD2EDr2Term))
      ->second;
  typedef int ProcessD2EDr2TermCpp(void const * const dataObject,
                                   double const de, double const * const r,
                                   double const * const dx,
                                   int const * const i, int const * const j);
  ProcessD2EDr2TermCpp * CppProcess_d2Edr2
      = reinterpret_cast<ProcessD2EDr2TermCpp *>(functionPointer);
  typedef int ProcessD2EDr2TermC(void const * const dataObject, double const de,
                                 double const * const r,
                                 double const * const dx,
                                 int const * const i, int const * const j);
  ProcessD2EDr2TermC * CProcess_d2Edr2
      = reinterpret_cast<ProcessD2EDr2TermC *>(functionPointer);
  typedef void ProcessD2EDr2TermF(void const * const dataObject,
                                  double const de, double const * const r,
                                  double const * const dx,
                                  int const * const i, int const * const j,
                                  int * const ierr);
  ProcessD2EDr2TermF * FProcess_d2Edr2
      = reinterpret_cast<ProcessD2EDr2TermF *>(functionPointer);

  int offset
      = ((simulatorNumbering_ == modelNumbering_) ? 0 : -numberingOffset_);
  int simulatorI[2];
  simulatorI[0] = i[0] + offset;
  simulatorI[1] = i[1] + offset;

  int simulatorJ[2];
  simulatorJ[0] = j[0] + offset;
  simulatorJ[1] = j[1] + offset;

  int error;
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppProcess_d2Edr2(dataObject, de, r, dx, simulatorI, simulatorJ);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    error = CProcess_d2Edr2(dataObject, de, r, dx, simulatorI, simulatorJ);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    FProcess_d2Edr2(dataObject, de, r, dx, simulatorI, simulatorJ, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Simulator supplied ProcessD2EDr2Term() routine returned error.");
    // LOG_DEBUG("Exit " + callString);
    return true;
  }
  else
  {
    // LOG_DEBUG("Exit " + callString);
    return false;
  }
}

void ModelImplementation::SetModelBufferPointer(void * const ptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetModelBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  modelBuffer_ = ptr;

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::GetModelBufferPointer(void ** const ptr) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetModelBufferPointer(" + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  *ptr = modelBuffer_;

  LOG_DEBUG("Exit " + callString);
}


void ModelImplementation::SetSimulatorBufferPointer(void * const ptr)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetSimulatorBufferPointer("
      + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  simulatorBuffer_ = ptr;

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::GetSimulatorBufferPointer(void ** const ptr) const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "GetSimulatorBufferPointer("
      + SPTR(ptr) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  *ptr = simulatorBuffer_;

  LOG_DEBUG("Exit " + callString);
}

namespace
{
typedef std::map<LengthUnit const, double, LENGTH_UNIT::Comparator>
LengthMap;

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

typedef std::map<EnergyUnit const, double, ENERGY_UNIT::Comparator>
EnergyMap;

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

typedef std::map<ChargeUnit const, double, CHARGE_UNIT::Comparator>
ChargeMap;

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

typedef std::map<TimeUnit const, double, TIME_UNIT::Comparator>
TimeMap;

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

int ModelImplementation::ConvertUnit(
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
#if LOG_DEBUG_SOURCE
  std::string const callString = "ConvertUnit("
      + fromLengthUnit.String() + ", "
      + fromEnergyUnit.String() + ", "
      + fromChargeUnit.String() + ", "
      + fromTemperatureUnit.String() + ", "
      + fromTimeUnit.String() + ", "
      + toLengthUnit.String() + ", "
      + toEnergyUnit.String() + ", "
      + toChargeUnit.String() + ", "
      + toTemperatureUnit.String() + ", "
      + toTimeUnit.String() + ", "
      + SNUM(lengthExponent) + ", "
      + SNUM(energyExponent) + ", "
      + SNUM(chargeExponent) + ", "
      + SNUM(temperatureExponent) + ", "
      + SNUM(timeExponent) + ", "
      + SPTR(conversionFactor) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  static LengthMap const lengthConvertToSI = GetLengthMap();
  static EnergyMap const energyConvertToSI = GetEnergyMap();
  static ChargeMap const chargeConvertToSI = GetChargeMap();
  static TemperatureMap const temperatureConvertToSI = GetTemperatureMap();
  static TimeMap const timeConvertToSI = GetTimeMap();

#if LOG_ERROR_SOURCE
  int error =
      Validate(fromLengthUnit) ||
      Validate(fromEnergyUnit) ||
      Validate(fromChargeUnit) ||
      Validate(fromTemperatureUnit) ||
      Validate(fromTimeUnit) ||
      Validate(toLengthUnit) ||
      Validate(toEnergyUnit) ||
      Validate(toChargeUnit) ||
      Validate(toTemperatureUnit) ||
      Validate(toTimeUnit);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  double const lengthConversion
      = lengthConvertToSI.find(toLengthUnit)->second /
      lengthConvertToSI.find(fromLengthUnit)->second;
  double const energyConversion
      = energyConvertToSI.find(toEnergyUnit)->second /
      energyConvertToSI.find(fromEnergyUnit)->second;
  double const chargeConversion
      = chargeConvertToSI.find(toChargeUnit)->second /
      chargeConvertToSI.find(fromChargeUnit)->second;
  double const temperatureConversion
      = temperatureConvertToSI.find(toTemperatureUnit)->second /
      temperatureConvertToSI.find(fromTemperatureUnit)->second;
  double const timeConversion
      = timeConvertToSI.find(toTimeUnit)->second /
      timeConvertToSI.find(fromTimeUnit)->second;

  *conversionFactor
      = pow(lengthConversion, lengthExponent)
      * pow(energyConversion, energyExponent)
      * pow(chargeConversion, chargeExponent)
      * pow(temperatureConversion, temperatureExponent)
      * pow(timeConversion, timeExponent);

  LOG_DEBUG("Exit " + callString);
  return false;
}

void ModelImplementation::SetLogID(std::string const & logID)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "SetLogID('" + logID + "').";
#endif
  LOG_DEBUG("Enter " + callString);

  log_->SetID(logID);

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::PushLogVerbosity(LogVerbosity const logVerbosity)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "PushLogVerbosity("
      + logVerbosity.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  log_->PushVerbosity(logVerbosity);

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::PopLogVerbosity()
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "PopLogVerbosity().";
#endif
  LOG_DEBUG("Enter " + callString);

  log_->PopVerbosity();

  LOG_DEBUG("Exit " + callString);
}

void ModelImplementation::LogEntry(LogVerbosity const logVerbosity,
                                   std::string const & message,
                                   int const lineNumber,
                                   std::string const & fileName) const
{
  // No debug logs to avoid infinite loop
  log_->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelImplementation::String() const
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "String().";
#endif
  LOG_DEBUG("Enter " + callString);

  std::stringstream ss;
  ss << std::setprecision(10) << std::scientific << std::left;
  ss <<
      "====================================================================="
      "===========\n\n";

  ss << "Model Name : " << modelName_ << "\n";
  if (modelType_ == ModelLibrary::PARAMETERIZED_MODEL)
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
     << std::setw(25) << "Refresh"
     << std::setw(10) << refreshLanguage_.String()
     << std::setw(15) << refreshFunction_
     << "\n"
     << "\t"
     << std::setw(25) << "Destroy"
     << std::setw(10) << destroyLanguage_.String()
     << std::setw(15) << destroyFunction_
     << "\n"
     << "\t"
     << std::setw(25) << "Compute"
     << std::setw(10) << computeLanguage_.String()
     << std::setw(15) << computeFunction_
     << "\n\n";

  ss << "Numbering : " << modelNumbering_.String() << "\n\n";

  ss << "Units : \n"
      "\tLength Unit      : " << lengthUnit_.String() << "\n"
      "\tEnergy Unit      : " << energyUnit_.String() << "\n"
      "\tCharge Unit      : " << chargeUnit_.String() << "\n"
      "\tTemperature Unit : " << temperatureUnit_.String() << "\n"
      "\tTime Unit        : " << timeUnit_.String() << "\n\n";

  ss << "Influence Distance : " << *influenceDistance_ << "\n\n";

  ss << "Number Of Neighbor List Cutoffs : " << numberOfCutoffs_ << "\n";
  ss << "Neighbor List Cutoffs :\n";
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
  for (std::map<SpeciesName const, int, SPECIES_NAME::Comparator>::
           const_iterator spec = supportedSpecies_.begin();
       spec != supportedSpecies_.end();
       ++spec)
  {
    ss << "\t" << std::setw(specWidth) << (spec->first).String()
       << std::setw(specWidth) << spec->second << "\n";
  }
  ss << "\n";

  ss << "Compute Arguments :\n";
  int const argW = 25;
  ss << "\t" << std::setw(argW) << "Argument Name"
     << std::setw(argW) << "SupportStatus"
     << std::setw(argW) << "Pointer"
     << "\n";
  ss << "\t" << std::setw(argW) << "-------------------------"
     << std::setw(argW) << "-------------------------"
     << std::setw(argW) << "-------------------------"
     << "\n\n";
  for (std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>::
           const_iterator argName = argumentSupportStatus_.begin();
       argName != argumentSupportStatus_.end();
       ++argName)
  {
    ss << "\t" << std::setw(argW) << (argName->first).String()
       << std::setw(argW) << (argName->second).String();

    if ((argName->second) != SUPPORT_STATUS::notSupported)
    {
      std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>::
          const_iterator ptr = argumentPointer_.find(argName->first);
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
     << std::setw(cbW) << "SupportStatus"
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
  for (std::map<CallbackName const, SupportStatus, CALLBACK_NAME::Comparator>::
           const_iterator cbName = callbackSupportStatus_.begin();
       cbName != callbackSupportStatus_.end();
       ++cbName)
  {
    ss << "\t" << std::setw(cbW) << (cbName->first).String()
       << std::setw(cbW) << (cbName->second).String();

    if ((cbName->second) != SUPPORT_STATUS::notSupported)
    {
      std::map<CallbackName const, LanguageName, CALLBACK_NAME::Comparator>::
          const_iterator ptr = callbackLanguage_.find(cbName->first);
      if (ptr != callbackLanguage_.end())
      {
        ss << std::setw(cbW) << (ptr->second).String();
        std::map<CallbackName const, func *, CALLBACK_NAME::Comparator>::
            const_iterator ptr2 = callbackFunctionPointer_.find(cbName->first);
        ss << std::setw(cbW) << ptr2->second;
        std::map<CallbackName const, void const *, CALLBACK_NAME::Comparator>::
            const_iterator ptr3
            = callbackDataObjectPointer_.find(cbName->first);
        ss << std::setw(cbW) << ptr3->second;
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
       << std::setw(10) << parameterDataType_[i].String()
       << std::setw(10) << parameterExtent_[i]
       << std::setw(15) << (void *) parameterPointer_[i]
       << parameterDescription_[i]
       << "\n";
  }
  ss << "\n";


  ss <<
      "====================================================================="
      "===========\n";

  string_ = ss.str();
  LOG_DEBUG("Exit " + callString);
  return string_;
}


ModelImplementation::ModelImplementation(ModelLibrary * const modelLibrary,
                                         Log * const log) :
    modelLibrary_(modelLibrary),
    log_(log),
    numberingHasBeenSet_(false),
    unitsHaveBeenSet_(false),
    influenceDistance_(0),
    numberOfCutoffs_(0),
    cutoffs_(0),
    refreshFunction_(0),
    destroyFunction_(0),
    computeFunction_(0),
    modelBuffer_(0),
    simulatorBuffer_(0)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "ModelImplementation("
      + SPTR(modelLibrary) + ", " + SPTR(log) + ").";
#endif
  LOG_DEBUG("Enter " + callString);

  // populate Arguments
  int numberOfArguments;
  ARGUMENT_NAME::GetNumberOfArguments(&numberOfArguments);
  for (int i=0; i<numberOfArguments; ++i)
  {
    ArgumentName argumentName;
    ARGUMENT_NAME::GetArgumentName(i, &argumentName);
    argumentSupportStatus_[argumentName] = SUPPORT_STATUS::notSupported;
  }
  // populate requiredByAPI Arguments
  for (std::vector<ArgumentName>::const_iterator requiredByAPI_Argument
           = ARGUMENT_NAME::requiredByAPI_Arguments.begin();
       requiredByAPI_Argument != ARGUMENT_NAME::requiredByAPI_Arguments.end();
       ++requiredByAPI_Argument)
  {
    argumentSupportStatus_[*requiredByAPI_Argument]
        = SUPPORT_STATUS::requiredByAPI;
    argumentPointer_[*requiredByAPI_Argument] = NULL;
  }

  // populate Callbacks
  int numberOfCallbacks;
  CALLBACK_NAME::GetNumberOfCallbacks(&numberOfCallbacks);
  for (int i=0; i<numberOfCallbacks; ++i)
  {
    CallbackName callbackName;
    CALLBACK_NAME::GetCallbackName(i, &callbackName);
    callbackSupportStatus_[callbackName] = SUPPORT_STATUS::notSupported;
  }
  // populate Callbacks
  for (std::vector<CallbackName>::const_iterator requiredByAPI_Callback
           = CALLBACK_NAME::requiredByAPI_Callbacks.begin();
       requiredByAPI_Callback != CALLBACK_NAME::requiredByAPI_Callbacks.end();
       ++requiredByAPI_Callback)
  {
    callbackSupportStatus_[*requiredByAPI_Callback]
        = SUPPORT_STATUS::requiredByAPI;
    callbackLanguage_[*requiredByAPI_Callback]
        = LANGUAGE_NAME::cpp;  // place holder
    callbackFunctionPointer_[*requiredByAPI_Callback] = NULL;
    callbackDataObjectPointer_[*requiredByAPI_Callback] = NULL;
  }

  LOG_DEBUG("Exit " + callString);
}

ModelImplementation::~ModelImplementation()
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "~ModelImplementation().";
#endif
  LOG_DEBUG("Enter " + callString);

  delete modelLibrary_;

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
#if LOG_DEBUG_SOURCE
  std::string const callString = "ModelCreate("
      + numbering.String() + ", "
      + requestedLengthUnit.String() + ", "
      + requestedEnergyUnit.String() + ", "
      + requestedChargeUnit.String() + ", "
      + requestedTemperatureUnit.String() + ", "
      + requestedTimeUnit.String() + ", '"
      + modelName + "').";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error =
      Validate(numbering) ||
      Validate(requestedLengthUnit) ||
      Validate(requestedEnergyUnit) ||
      Validate(requestedChargeUnit) ||
      Validate(requestedTemperatureUnit) ||
      Validate(requestedTimeUnit);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  modelName_ = modelName;

  error = SetSimulatorNumbering(numbering);
  if (error)
  {
    LOG_ERROR("Could not set simulator numbering.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  error = modelLibrary_->Open(true, modelName);
  if (error)
  {
    LOG_ERROR("Could not open model shared library.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  error = modelLibrary_->GetModelType(&modelType_);
  switch (modelType_)
  {
    case ModelLibrary::STAND_ALONE_MODEL:
      LOG_DEBUG("Initializing a stand alone model.");
      error = InitializeStandAloneModel(requestedLengthUnit,
                                        requestedEnergyUnit,
                                        requestedChargeUnit,
                                        requestedTemperatureUnit,
                                        requestedTimeUnit);
      if (error)
      {
        LOG_ERROR("Initialization of Stand Alone model returned error.");
        LOG_DEBUG("Exit " + callString);
        return true;
      }
      break;
    case ModelLibrary::PARAMETERIZED_MODEL:
      LOG_DEBUG("Initializing a parameterized model.");
      error = InitializeParameterizedModel(requestedLengthUnit,
                                           requestedEnergyUnit,
                                           requestedChargeUnit,
                                           requestedTemperatureUnit,
                                           requestedTimeUnit);
      if (error)
      {
        LOG_ERROR("Initialization of Parameterized Model returned error.");
        LOG_DEBUG("Exit " + callString);
        return true;
      }
      break;
    case ModelLibrary::MODEL_DRIVER:
      LOG_ERROR("Creation of a model driver is not allowed.");
      LOG_DEBUG("Exit " + callString);
      return true;
      break;
    case ModelLibrary::SIMULATOR_MODEL:
      LOG_ERROR("Creation of a simulator model is not allowed.");
      LOG_DEBUG("Exit " + callString);
      return true;
      break;
    default:
      LOG_ERROR("Creation of an unknown model type is not allowed.");
      LOG_DEBUG("Exit " + callString);
      return true;
      break;
  }

#if LOG_ERROR_SOURCE
  // Error checking
  //  no need to check arguments, callbacks, or parameters
  if (!numberingHasBeenSet_)
  {
    LOG_ERROR("Model supplied Create() routine did not set numbering.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (!unitsHaveBeenSet_)
  {
    LOG_ERROR("Model supplied Create() routine did not set units.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (influenceDistance_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "InfluenceDistancePointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (cutoffs_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "CutoffsPointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (refreshFunction_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "RefreshPointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (destroyFunction_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "DestroyPointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (computeFunction_ == NULL)
  {
    LOG_ERROR("Model supplied Create() routine did not set "
              "ComputePointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (supportedSpecies_.empty())
  {
    LOG_ERROR("Model supplied Create() routine did not set SpeciesCode.");
    LOG_DEBUG("Exit " + callString);
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

  // resize getNeighborListStorage_
  if (simulatorNumbering_ != modelNumbering_)
    getNeighborListStorage_.resize(numberOfCutoffs_);

  LOG_DEBUG("Exit " + callString);
  return false;
}

struct KIM_ModelDestroy
{
  void * p;
};

int ModelImplementation::ModelDestroy()
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "ModelDestroy().";
#endif
  LOG_DEBUG("Enter " + callString);

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
  if (destroyLanguage_ == LANGUAGE_NAME::cpp)
  {
    error = CppDestroy(reinterpret_cast<KIM::ModelDestroy *>(&M));
  }
  else if (destroyLanguage_ == LANGUAGE_NAME::c)
  {
    KIM_ModelDestroy cM;
    cM.p = &M;
    error = CDestroy(&cM);
  }
  else if (destroyLanguage_ == LANGUAGE_NAME::fortran)
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
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  if (error)
  {
    LOG_ERROR("Model supplied Destroy() routine returned error.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit " + callString);
    return false;
  }
}

struct KIM_ModelCreate
{
  void * p;
};

int ModelImplementation::InitializeStandAloneModel(
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "InitializeStandAloneModel("
      + requestedLengthUnit.String() + ", "
      + requestedEnergyUnit.String() + ", "
      + requestedChargeUnit.String() + ", "
      + requestedTemperatureUnit.String() + ", "
      + requestedTimeUnit.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error =
      Validate(requestedLengthUnit) ||
      Validate(requestedEnergyUnit) ||
      Validate(requestedChargeUnit) ||
      Validate(requestedTemperatureUnit) ||
      Validate(requestedTimeUnit);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  LanguageName languageName;
  func * functionPointer = 0;
  error = modelLibrary_->GetModelCreateFunctionPointer(
      &languageName, &functionPointer);
  if (error)
  {
    LOG_ERROR("Could not get ModelCreateFunctionPointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  typedef int ModelCreateCpp(
      KIM::ModelCreate * const modelCreate,
      LengthUnit const requestedLengthUnit,
      EnergyUnit const requestedEnergyUnit,
      ChargeUnit const requestedChargeUnit,
      TemperatureUnit const requestedTemperatureUnit,
      TimeUnit const requestedTimeUnit);
  ModelCreateCpp * CppCreate
      = reinterpret_cast<ModelCreateCpp *>(functionPointer);
  typedef int ModelCreateC(
      KIM_ModelCreate * const modelCreate,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit);
  ModelCreateC * CCreate
      = reinterpret_cast<ModelCreateC *>(functionPointer);
  typedef void ModelCreateF(
      KIM_ModelCreate * const modelCreate,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit,
      int * const);
  ModelCreateF * FCreate
      = reinterpret_cast<ModelCreateF *>(functionPointer);

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
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppCreate(
        reinterpret_cast<KIM::ModelCreate *>(&M),
        requestedLengthUnit, requestedEnergyUnit,
        requestedChargeUnit, requestedTemperatureUnit,
        requestedTimeUnit);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    KIM_ModelCreate cM;
    cM.p = &M;
    error = CCreate(&cM, requestedLengthUnitC, requestedEnergyUnitC,
                    requestedChargeUnitC, requestedTemperatureUnitC,
                    requestedTimeUnitC);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    KIM_ModelCreate cM;
    cM.p = &M;
    KIM_ModelCreate cM_Handle;
    cM_Handle.p = &cM;
    FCreate(&cM_Handle, requestedLengthUnitC, requestedEnergyUnitC,
            requestedChargeUnitC, requestedTemperatureUnitC,
            requestedTimeUnitC, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  if (error)
  {
    LOG_ERROR("Model supplied Create() routine returned error.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  else
  {
    LOG_DEBUG("Exit " + callString);
    return false;
  }
}

struct KIM_ModelDriverCreate
{
  void * p;
};

int ModelImplementation::InitializeParameterizedModel(
    LengthUnit const requestedLengthUnit,
    EnergyUnit const requestedEnergyUnit,
    ChargeUnit const requestedChargeUnit,
    TemperatureUnit const requestedTemperatureUnit,
    TimeUnit const requestedTimeUnit)
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "InitializeParameterizedModel("
      + requestedLengthUnit.String() + ", "
      + requestedEnergyUnit.String() + ", "
      + requestedChargeUnit.String() + ", "
      + requestedTemperatureUnit.String() + ", "
      + requestedTimeUnit.String() + ").";
#endif
  LOG_DEBUG("Enter " + callString);

#if LOG_ERROR_SOURCE
  int error =
      Validate(requestedLengthUnit) ||
      Validate(requestedEnergyUnit) ||
      Validate(requestedChargeUnit) ||
      Validate(requestedTemperatureUnit) ||
      Validate(requestedTimeUnit);
  if (error)
  {
    LOG_ERROR("Invalid arguments.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
#endif

  // get driver name
  error = modelLibrary_->GetModelDriverName(&modelDriverName_);
  if (error)
  {
    LOG_ERROR("Could not get Model Driver name.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  // write parameter files to scratch space
  error = WriteParameterFiles();
  if (error)
  {
    LOG_ERROR("Could not write parameter files to scratch space.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  // close model and open driver
  error = modelLibrary_->Close();
  if (error)
  {
    LOG_ERROR("Could not close model shared library.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  error = modelLibrary_->Open(false, modelDriverName_);
  if (error)
  {
    LOG_ERROR("Could not open model driver shared library.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  // check that it is a driver
  ModelLibrary::ITEM_TYPE itemType;
  error = modelLibrary_->GetModelType(&itemType);
  if ((error) || (itemType != ModelLibrary::MODEL_DRIVER))
  {
    LOG_ERROR("Invalid model driver shared library.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  LanguageName languageName;
  func * functionPointer = 0;
  error = modelLibrary_->GetModelCreateFunctionPointer(
      &languageName, &functionPointer);
  if (error)
  {
    LOG_ERROR("Could not get ModelCreateFunctionPointer.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }

  typedef int ModelDriverCreateCpp(
      KIM::ModelDriverCreate * const modelDriverCreate,
      LengthUnit const requestedLengthUnit,
      EnergyUnit const requestedEnergyUnit,
      ChargeUnit const requestedChargeUnit,
      TemperatureUnit const requestedTemperatureUnit,
      TimeUnit const requestedTimeUnit);
  ModelDriverCreateCpp * CppCreate
      = reinterpret_cast<ModelDriverCreateCpp *>(functionPointer);
  typedef int ModelDriverCreateC(
      KIM_ModelDriverCreate * const modelDriverCreate,
      KIM_LengthUnit const requestedLengthUnit,
      KIM_EnergyUnit const requestedEnergyUnit,
      KIM_ChargeUnit const requestedChargeUnit,
      KIM_TemperatureUnit const requestedTemperatureUnit,
      KIM_TimeUnit const requestedTimeUnit);
  ModelDriverCreateC * CCreate
      = reinterpret_cast<ModelDriverCreateC *>(functionPointer);
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
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppCreate(
        reinterpret_cast<KIM::ModelDriverCreate *>(&M),
        requestedLengthUnit, requestedEnergyUnit,
        requestedChargeUnit, requestedTemperatureUnit,
        requestedTimeUnit);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    KIM_ModelDriverCreate cM;
    cM.p = &M;
    error = CCreate(&cM, requestedLengthUnitC, requestedEnergyUnitC,
                    requestedChargeUnitC, requestedTemperatureUnitC,
                    requestedTimeUnitC);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    KIM_ModelDriverCreate cM;
    cM.p = &M;
    KIM_ModelDriverCreate cM_Handle;
    cM_Handle.p = &cM;
    FCreate(&cM_Handle, requestedLengthUnitC, requestedEnergyUnitC,
            requestedChargeUnitC, requestedTemperatureUnitC,
            requestedTimeUnitC, &error);
  }
  else
  {
    LOG_ERROR("Unknown LanguageName.  SHOULD NEVER GET HERE.");
    LOG_DEBUG("Exit " + callString);
    return true;
  }
  if (error)
  {
    LOG_ERROR("Model Driver supplied Create() routine returned error.");
    LOG_DEBUG("Exit " + callString);
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

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::WriteParameterFiles()
{
#if LOG_DEBUG_SOURCE
  std::string const callString = "WriteParameterFiles().";
#endif
  LOG_DEBUG("Enter " + callString);

  modelLibrary_->GetNumberOfParameterFiles(&numberOfParameterFiles_);
  std::vector<unsigned char const *> parameterFileStrings;
  std::vector<unsigned int> parameterFileStringLengths;
  for (int i=0; i<numberOfParameterFiles_; ++i)
  {
    unsigned char const * strPtr;
    unsigned int length;
    int error = modelLibrary_->GetParameterFileString(i, &length , &strPtr);
    if (error)
    {
      LOG_ERROR("Could not get parameter file data.");
      LOG_DEBUG("Exit " + callString);
      return true;
    }
    parameterFileStrings.push_back(strPtr);
    parameterFileStringLengths.push_back(length);
  }

  static char const fileNameString[] = "kim-model-parameter-file-XXXXXXXXXXXX";
  for (int i=0; i<numberOfParameterFiles_; ++i)
  {
    std::stringstream templateString;
    templateString << P_tmpdir << "/" << fileNameString;
    char * cstr = strdup(templateString.str().c_str());
    int fileid = mkstemp(cstr);
    if (fileid == -1)
    {
      free(cstr);
      LOG_ERROR("Could not create a secure temporary file.");
      LOG_DEBUG("Exit " + callString);
      return true;
    }
    parameterFileNames_.push_back(cstr);
    free(cstr);

    FILE* fl = fdopen(fileid,"w");
    fwrite(parameterFileStrings[i], sizeof(unsigned char),
           parameterFileStringLengths[i], fl);
    fclose(fl);  // also closed the fileid
  }

  LOG_DEBUG("Exit " + callString);
  return false;
}

int ModelImplementation::Validate(ArgumentName const argumentName) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + argumentName.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfArguments;
  ARGUMENT_NAME::GetNumberOfArguments(&numberOfArguments);

  for (int i = 0; i < numberOfArguments; ++i)
  {
    ArgumentName argName;
    ARGUMENT_NAME::GetArgumentName(i, &argName);

    if (argumentName == argName)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid ArgumentName encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(CallbackName const callbackName) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + callbackName.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfCallbacks;
  CALLBACK_NAME::GetNumberOfCallbacks(&numberOfCallbacks);

  for (int i = 0; i < numberOfCallbacks; ++i)
  {
    CallbackName cbName;
    CALLBACK_NAME::GetCallbackName(i, &cbName);

    if (callbackName == cbName)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid CallbackName encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(ChargeUnit const chargeUnit) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + chargeUnit.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfChargeUnits;
  CHARGE_UNIT::GetNumberOfChargeUnits(&numberOfChargeUnits);

  for (int i = 0; i < numberOfChargeUnits; ++i)
  {
    ChargeUnit cgUnit;
    CHARGE_UNIT::GetChargeUnit(i, &cgUnit);

    if (chargeUnit == cgUnit)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid ChargeUnit encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(DataType const dataType) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + dataType.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfDataTypes;
  DATA_TYPE::GetNumberOfDataTypes(&numberOfDataTypes);

  for (int i = 0; i < numberOfDataTypes; ++i)
  {
    DataType dType;
    DATA_TYPE::GetDataType(i, &dType);

    if (dataType == dType)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid DataType encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(EnergyUnit const energyUnit) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + energyUnit.String() + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfEnergyUnits;
  ENERGY_UNIT::GetNumberOfEnergyUnits(&numberOfEnergyUnits);

  for (int i = 0; i < numberOfEnergyUnits; ++i)
  {
    EnergyUnit eUnit;
    ENERGY_UNIT::GetEnergyUnit(i, &eUnit);

    if (energyUnit == eUnit)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid EnergyUnit encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(LanguageName const languageName) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + languageName.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfLanguageNames;
  LANGUAGE_NAME::GetNumberOfLanguageNames(&numberOfLanguageNames);

  for (int i = 0; i < numberOfLanguageNames; ++i)
  {
    LanguageName langName;
    LANGUAGE_NAME::GetLanguageName(i, &langName);

    if (languageName == langName)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid LanguageName encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(LengthUnit const lengthUnit) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + lengthUnit.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfLengthUnits;
  LENGTH_UNIT::GetNumberOfLengthUnits(&numberOfLengthUnits);

  for (int i = 0; i < numberOfLengthUnits; ++i)
  {
    LengthUnit lenUnit;
    LENGTH_UNIT::GetLengthUnit(i, &lenUnit);

    if (lengthUnit == lenUnit)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid LengthUnit encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(Numbering const numbering) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + numbering.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfNumberings;
  NUMBERING::GetNumberOfNumberings(&numberOfNumberings);

  for (int i = 0; i < numberOfNumberings; ++i)
  {
    Numbering num;
    NUMBERING::GetNumbering(i, &num);

    if (numbering == num)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid Numbering encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(SpeciesName const speciesName) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + speciesName.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfSpeciesNames;
  SPECIES_NAME::GetNumberOfSpeciesNames(&numberOfSpeciesNames);

  for (int i = 0; i < numberOfSpeciesNames; ++i)
  {
    SpeciesName specName;
    SPECIES_NAME::GetSpeciesName(i, &specName);

    if (speciesName == specName)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid SpeciesName encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(SupportStatus const supportStatus) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + supportStatus.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfSupportStatuses;
  SUPPORT_STATUS::GetNumberOfSupportStatuses(&numberOfSupportStatuses);

  for (int i = 0; i < numberOfSupportStatuses; ++i)
  {
    SupportStatus supStatus;
    SUPPORT_STATUS::GetSupportStatus(i, &supStatus);

    if (supportStatus == supStatus)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid SupportStatus encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(TemperatureUnit const temperatureUnit) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + temperatureUnit.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfTemperatureUnits;
  TEMPERATURE_UNIT::GetNumberOfTemperatureUnits(&numberOfTemperatureUnits);

  for (int i = 0; i < numberOfTemperatureUnits; ++i)
  {
    TemperatureUnit tempUnit;
    TEMPERATURE_UNIT::GetTemperatureUnit(i, &tempUnit);

    if (temperatureUnit == tempUnit)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid TemperatureUnit encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}

int ModelImplementation::Validate(TimeUnit const timeUnit) const
{
  // No debug logging for Validate: too expensive
  //
  // #if LOG_DEBUG_SOURCE
  //   std::string const callString = "Validate(" + timeUnit.String()
  //       + ").";
  // #endif
  //   LOG_DEBUG("Enter " + callString);

  int numberOfTimeUnits;
  TIME_UNIT::GetNumberOfTimeUnits(&numberOfTimeUnits);

  for (int i = 0; i < numberOfTimeUnits; ++i)
  {
    TimeUnit tmUnit;
    TIME_UNIT::GetTimeUnit(i, &tmUnit);

    if (timeUnit == tmUnit)
    {
      // LOG_DEBUG("Exit " + callString);
      return false;
    }
  }

  LOG_ERROR("Invalid TimeUnit encountered.");
  // LOG_DEBUG("Exit " + callString);
  return true;
}
}  // namespace KIM
