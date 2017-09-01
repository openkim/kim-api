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
  ModelImplementation * pModelImplementation;
  pModelImplementation = new ModelImplementation(new ModelLibrary());

  int error = pModelImplementation->ModelCreate(
      numbering, requestedLengthUnit, requestedEnergyUnit, requestedChargeUnit,
      requestedTemperatureUnit, requestedTimeUnit, modelName);
  if (error)
  {
    delete pModelImplementation;
    // LOG_ERROR("");  @@@@ need access to a log object
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
    *requestedUnitsAccepted = true;
  }
  else
  {
    *requestedUnitsAccepted = false;
  }

  *modelImplementation = pModelImplementation;

  return false;
}


void ModelImplementation::Destroy(
    ModelImplementation ** const modelImplementation)
{

  int error = (*modelImplementation)->ModelDestroy();
  if (error)
  {
    // LOG_ERROR(""); @@@ need access to a log object
  }

  delete *modelImplementation;
  *modelImplementation = 0;
}

void ModelImplementation::SetInfluenceDistancePointer(
    double const * const influenceDistance)
{
  influenceDistance_ = influenceDistance;
}
void ModelImplementation::GetInfluenceDistance(
    double * const influenceDistance) const
{
  *influenceDistance = *influenceDistance_;
}


void ModelImplementation::SetCutoffsPointer(
    int const numberOfCutoffs, double const * const cutoffs)
{
  numberOfCutoffs_ = numberOfCutoffs;
  cutoffs_ = cutoffs;
}

// allows NULL as value of cutoffs (to get just numberOfCutoffs)
void ModelImplementation::GetCutoffsPointer(int * const numberOfCutoffs,
                                            double const ** const cutoffs) const
{
  *numberOfCutoffs = numberOfCutoffs_;
  *cutoffs = cutoffs_;
}


int ModelImplementation::SetRefreshPointer(LanguageName const languageName,
                                           func * const fptr)
{
  refreshLanguage_ = languageName;
  refreshFunction_ = fptr;

  return false;
}

int ModelImplementation::SetDestroyPointer(LanguageName const languageName,
                                           func * const fptr)
{
  destroyLanguage_ = languageName;
  destroyFunction_ = fptr;

  return false;
}

int ModelImplementation::SetComputePointer(LanguageName const languageName,
                                           func * const fptr)
{
  computeLanguage_ = languageName;
  computeFunction_ = fptr;

  return false;
}


int ModelImplementation::SetSpeciesCode(SpeciesName const speciesName,
                                        int const code)
{
  supportedSpecies_[speciesName] = code;

  return false;
}

int ModelImplementation::GetSpeciesSupportAndCode(
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


int ModelImplementation::SetArgumentSupportStatus(
    ArgumentName const argumentName, SupportStatus const supportStatus)
{
  argumentSupportStatus_[argumentName] = supportStatus;

  // @@@ do lots of error checking

  if (supportStatus != SUPPORT_STATUS::notSupported)
  {
    auto result = argumentPointer_.find(argumentName);

    if (result == argumentPointer_.end())
    {
      argumentPointer_[argumentName] = 0;
    }
  }

  return false;
}

int ModelImplementation::GetArgumentSupportStatus(
    ArgumentName const argumentName, SupportStatus * const supportStatus)
    const
{
  auto result = argumentSupportStatus_.find(argumentName);

  if (result == argumentSupportStatus_.end())
  {
    LOG_ERROR("");
    return true;
  }
  else
  {
    *supportStatus = result->second;
    return false;
  }
}


int ModelImplementation::SetCallbackSupportStatus(
    CallbackName const callbackName, SupportStatus const supportStatus)
{
  callbackSupportStatus_[callbackName] = supportStatus;

  return false;
}

int ModelImplementation::GetCallbackSupportStatus(
    CallbackName const callbackName, SupportStatus * const supportStatus) const
{
  auto result = callbackSupportStatus_.find(callbackName);

  if (result == callbackSupportStatus_.end())
  {
    LOG_ERROR("");
    return true;
  }
  else
  {
    *supportStatus = result->second;
    return false;
  }
}


int ModelImplementation::SetModelNumbering(Numbering const numbering)
{
  modelNumbering_ = numbering;

  return false;
}

int ModelImplementation::SetSimulatorNumbering(Numbering const numbering)
{
  simulatorNumbering_ = numbering;
  return false;
}


int ModelImplementation::SetUnits(LengthUnit const lengthUnit,
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

void ModelImplementation::GetUnits(LengthUnit * const lengthUnit,
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


int ModelImplementation::GetNumberOfParameterFiles(
    int * const numberOfParameterFiles) const
{
  if (modelType_ != ModelLibrary::ITEM_TYPE::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("");
    return true;
  }

  *numberOfParameterFiles = numberOfParameterFiles_;
  return false;
}

int ModelImplementation::GetParameterFileName(
    int const index, std::string * const parameterFileName) const
{
  if (modelType_ != ModelLibrary::ITEM_TYPE::PARAMETERIZED_MODEL)
  {
    LOG_ERROR("");
    return true;
  }

  if ((index < 0) || (index >= numberOfParameterFiles_))
  {
    LOG_ERROR("");
    return true;
  }

  *parameterFileName = parameterFileNames_[index];
  return false;
}

int ModelImplementation::SetParameterPointer(int const extent, int * const ptr,
                                             std::string const & description)
{
  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Integer);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  return false;
}

int ModelImplementation::SetParameterPointer(int const extent,
                                             double * const ptr,
                                             std::string const & description)
{
  parameterDescription_.push_back(description);
  parameterDataType_.push_back(DATA_TYPE::Double);
  parameterExtent_.push_back(extent);
  parameterPointer_.push_back(ptr);

  return false;
}

void ModelImplementation::GetNumberOfParameters(int * const numberOfParameters)
    const
{
  *numberOfParameters = parameterPointer_.size();
}

int ModelImplementation::GetParameterDataTypeAndDescription(
    int const index, DataType * const dataType,
    std::string * const description) const
{
  *dataType = parameterDataType_[index];
  *description = parameterDescription_[index];

  return false;
}

int ModelImplementation::GetParameterExtentAndPointer(
    int const index, int * extent, int ** const ptr)
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<int *>(parameterPointer_[index]);

  return false;
}

int ModelImplementation::GetParameterExtentAndPointer(
    int const index, int * extent, int const ** const ptr) const
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<int const *>(parameterPointer_[index]);

  return false;
}

int ModelImplementation::GetParameterExtentAndPointer(
    int const index, int * extent, double ** const ptr)
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<double *>(parameterPointer_[index]);

  return false;
}

int ModelImplementation::GetParameterExtentAndPointer(
    int const index, int * extent, double const ** const ptr) const
{
  *extent = parameterExtent_[index];
  *ptr = reinterpret_cast<double const *>(parameterPointer_[index]);

  return false;
}


int ModelImplementation::SetArgumentPointer(ArgumentName const argumentName,
                                            int const * const ptr)
{
  //@@@ check for existence
  argumentPointer_[argumentName]
      = reinterpret_cast<void *>(const_cast<int *>(ptr));

  return false;
}

int ModelImplementation::SetArgumentPointer(ArgumentName const argumentName,
                                            double const * const ptr)
{
  //@@@ check for existence
  argumentPointer_[argumentName]
      = reinterpret_cast<void *>(const_cast<double *>(ptr));

  return false;
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            int const ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    LOG_ERROR(argumentName.String());
    return true;
  }
  else
  {
    *ptr = reinterpret_cast<int const *>(result->second);
    return false;
  }
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            int ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    LOG_ERROR("");
    return true;
  }
  else
  {
    *ptr = reinterpret_cast<int *>(result->second);
    return false;
  }
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            double const ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    LOG_ERROR(argumentName.String());
    return true;
  }
  else
  {
    *ptr = reinterpret_cast<double const *>(result->second);
    return false;
  }
}

int ModelImplementation::GetArgumentPointer(ArgumentName const argumentName,
                                            double ** const ptr) const
{
  auto result = argumentPointer_.find(argumentName);

  if (result == argumentPointer_.end())
  {
    *ptr = 0;
    LOG_ERROR(argumentName.String());
    return true;
  }
  else
  {
    *ptr = reinterpret_cast<double *>(result->second);
    return false;
  }
}



int ModelImplementation::SetCallbackPointer(CallbackName const callbackName,
                                            LanguageName const languageName,
                                            func * const fptr,
                                            void const * const dataObject)
{
  auto result = callbackSupportStatus_.find(callbackName);

  if ((result == callbackSupportStatus_.end())
      ||
      (result->second == SUPPORT_STATUS::notSupported))
  {
    LOG_ERROR("");
    return true;
  }
  else
  {
    callbackLanguage_[callbackName] = languageName;
    callbackFunctionPointer_[callbackName] = fptr;
    callbackDataObjectPointer_[callbackName] = dataObject;
    return false;
  }
}

int ModelImplementation::IsCallbackPresent(
    CallbackName const callbackName, int * const present) const
{
  auto result = callbackFunctionPointer_.find(callbackName);

  if ((result == callbackFunctionPointer_.end())
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


int ModelImplementation::Compute() const
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
    FCompute(&cM, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }

  if (error)
  {
    LOG_ERROR("");
    return true;
  }
  else
    return false;
}

int ModelImplementation::ClearInfluenceDistanceAndCutoffsThenRefreshModel()
{
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
    FRefresh(&cM, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }

  if (error)
  {
    LOG_ERROR("");
    return true;
  }
  else
    return false;
}

int ModelImplementation::GetNeighborList(int const neighborListIndex,
                                         int const particleNumber,
                                         int * const numberOfNeighbors,
                                         int const ** const neighborsOfParticle)
    const
{
  auto languageResult = callbackLanguage_.find(CALLBACK_NAME::GetNeighborList);
  if (languageResult == callbackLanguage_.end())
  {
    LOG_ERROR("");
    return true;
  }
  LanguageName languageName = languageResult->second;
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
    LOG_ERROR("");
    return true;
  }

  if (error)
  {
    LOG_ERROR("");
    return true;
  }

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

int ModelImplementation::ProcessDEDrTerm(double const de, double const r,
                                         double const * const dx,
                                         int const i, int const j) const
{
  auto languageResult = callbackLanguage_.find(CALLBACK_NAME::ProcessDEDrTerm);
  if (languageResult == callbackLanguage_.end())
  {
    LOG_ERROR("");
    return true;
  }
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

  int error;
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppProcess_dEdr(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    error = CProcess_dEdr(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    FProcess_dEdr(dataObject, de, r, dx, i, j, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }

  if (error)
  {
    LOG_ERROR("");
    return true;
  }
  else
    return false;
}

int ModelImplementation::ProcessD2EDr2Term(double const de,
                                           double const * const r,
                                           double const * const dx,
                                           int const * const i,
                                           int const * const j) const
{
  auto languageResult = callbackLanguage_
      .find(CALLBACK_NAME::ProcessD2EDr2Term);
  if (languageResult == callbackLanguage_.end())
  {
    LOG_ERROR("");
    return true;
  }
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

  int error;
  if (languageName == LANGUAGE_NAME::cpp)
  {
    error = CppProcess_d2Edr2(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::c)
  {
    error = CProcess_d2Edr2(dataObject, de, r, dx, i, j);
  }
  else if (languageName == LANGUAGE_NAME::fortran)
  {
    FProcess_d2Edr2(dataObject, de, r, dx, i, j, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }

  if (error)
  {
    LOG_ERROR("");
    return true;
  }
  else
    return false;
}

void ModelImplementation::SetModelBufferPointer(void * const ptr)
{
  modelBuffer_ = ptr;
}

void ModelImplementation::GetModelBufferPointer(void ** const ptr) const
{
  *ptr = modelBuffer_;
}


void ModelImplementation::SetSimulatorBufferPointer(void * const ptr)
{
  simulatorBuffer_ = ptr;
}

void ModelImplementation::GetSimulatorBufferPointer(void ** const ptr) const
{
  *ptr = simulatorBuffer_;
}


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

std::string ModelImplementation::String() const
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
     << std::setw(25) << "Refresh"
     << std::setw(10) << refreshLanguage_.String()
     << std::setw(15) << (void *) refreshFunction_
     << "\n"
     << "\t"
     << std::setw(25) << "Destroy"
     << std::setw(10) << destroyLanguage_.String()
     << std::setw(15) << (void *) destroyFunction_
     << "\n"
     << "\t"
     << std::setw(25) << "Compute"
     << std::setw(10) << computeLanguage_.String()
     << std::setw(15) << (void *) computeFunction_
     << "\n\n";

  ss << "Numbering : " << modelNumbering_.String() << "\n\n";

  ss << "Units : \n"
      "\tLength Unit      : " << lengthUnit_.String() << "\n"
      "\tEnergy Unit      : " << energyUnit_.String() << "\n"
      "\tCharge Unit      : " << chargeUnit_.String() << "\n"
      "\tTemperature Unit : " << temperatureUnit_.String() << "\n"
      "\tTime Unit        : " << timeUnit_.String() << "\n\n";

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
  for (auto argName = argumentSupportStatus_.begin();
       argName != argumentSupportStatus_.end();
       ++argName)
  {
    ss << "\t" << std::setw(argW) << (argName->first).String()
       << std::setw(argW) << (argName->second).String();

    if ((argName->second) != SUPPORT_STATUS::notSupported)
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
  for (auto cbName = callbackSupportStatus_.begin();
       cbName != callbackSupportStatus_.end();
       ++cbName)
  {
    ss << "\t" << std::setw(cbW) << (cbName->first).String()
       << std::setw(cbW) << (cbName->second).String();

    if ((cbName->second) != SUPPORT_STATUS::notSupported)
    {
      auto ptr = callbackLanguage_.find(cbName->first);
      if (ptr != callbackLanguage_.end())
      {
        ss << std::setw(cbW) << (ptr->second).String();
        auto ptr2 = callbackFunctionPointer_.find(cbName->first);
        ss << std::setw(cbW) << (void *) ptr2->second;
        auto ptr3 = callbackDataObjectPointer_.find(cbName->first);
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
  return ss.str();
}


ModelImplementation::ModelImplementation(ModelLibrary * const modelLibrary) :
    modelLibrary_(modelLibrary),
    influenceDistance_(0),
    numberOfCutoffs_(0),
    cutoffs_(0),
    refreshFunction_(0),
    destroyFunction_(0),
    computeFunction_(0),
    modelBuffer_(0),
    simulatorBuffer_(0)
{
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
  for (auto requiredByAPI_Argument
           = ARGUMENT_NAME::requiredByAPI_Arguments.begin();
       requiredByAPI_Argument != ARGUMENT_NAME::requiredByAPI_Arguments.end();
       ++requiredByAPI_Argument)
  {
    argumentSupportStatus_[*requiredByAPI_Argument]
        = SUPPORT_STATUS::requiredByAPI;
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
  for (auto requiredByAPI_Callback
           = CALLBACK_NAME::requiredByAPI_Callbacks.begin();
       requiredByAPI_Callback != CALLBACK_NAME::requiredByAPI_Callbacks.end();
       ++requiredByAPI_Callback)
  {
    callbackSupportStatus_[*requiredByAPI_Callback]
        = SUPPORT_STATUS::requiredByAPI;
  }
}

ModelImplementation::~ModelImplementation()
{
  delete modelLibrary_;
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
  modelName_ = modelName;

  int error = SetSimulatorNumbering(numbering);
  if (error)
  {
    LOG_ERROR("");
    return true;
  }

  error = modelLibrary_->open(true, modelName);
  if (error)
  {
    LOG_ERROR("");
    return true;
  }

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
        LOG_ERROR("");
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
        LOG_ERROR("");
        return true;
      }
      break;
    case ModelLibrary::ITEM_TYPE::MODEL_DRIVER:
      LOG_ERROR("");
      return true;
      break;
    case ModelLibrary::ITEM_TYPE::SIMULATOR_MODEL:
      LOG_ERROR("");
      return true;
      break;
    default:
      LOG_ERROR("");
      return true;
      break;
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
    FDestroy(&cM, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }

  if (error)
  {
    LOG_ERROR("");
    return true;
  }
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
  int error = modelLibrary_->getModelCreateFunctionPointer(
      &languageName, &functionPointer);
  if (error)
  {
    LOG_ERROR("");
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
    FCreate(&cM, requestedLengthUnitC, requestedEnergyUnitC,
            requestedChargeUnitC, requestedTemperatureUnitC,
            requestedTimeUnitC, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }
  if (error)
  {
    LOG_ERROR("");
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
    LOG_ERROR("");
    return true;
  }

  // write parameter files to scratch space
  error = WriteParameterFiles();
  if (error)
  {
    LOG_ERROR("");
    return true;
  }

  // close model and open driver
  error = modelLibrary_->close();
  if (error)
  {
    LOG_ERROR("");
    return true;
  }
  error = modelLibrary_->open(false, modelDriverName_);
  if (error)
  {
    LOG_ERROR("");
    return true;
  }
  // check that it is a driver
  ModelLibrary::ITEM_TYPE itemType;
  error = modelLibrary_->getModelType(&itemType);
  if ((error) || (itemType != ModelLibrary::ITEM_TYPE::MODEL_DRIVER))
  {
    LOG_ERROR("");
    return true;
  }

  LanguageName languageName;
  func * functionPointer = 0;
  error = modelLibrary_->getModelCreateFunctionPointer(
      &languageName, &functionPointer);
  if (error)
  {
    LOG_ERROR("");
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
    FCreate(&cM, requestedLengthUnitC, requestedEnergyUnitC,
            requestedChargeUnitC, requestedTemperatureUnitC,
            requestedTimeUnitC, &error);
  }
  else
  {
    LOG_ERROR("");
    return true;
  }
  if (error)
  {
    LOG_ERROR("");
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
    if (error)
    {
      LOG_ERROR("");
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
      LOG_ERROR("");
      return true;
    }
    parameterFileNames_.push_back(cstr);
    free(cstr);

    FILE* fl = fdopen(fileid,"w");
    fwrite(parameterFileStrings[i], sizeof(unsigned char),
           parameterFileStringLengths[i], fl);
    fclose(fl);  // also closed the fileid
  }

  return false;
}

}  // namespace KIM
