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


#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#define KIM_MODEL_IMPLEMENTATION_HPP_

#include <string>
#include <map>
#include <vector>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif

#ifndef KIM_SPECIESNAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_SUPPORT_STATUS_HPP_
#include "KIM_SupportStatus.hpp"
#endif

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif

#ifndef KIM_CALLBACK_NAME_HPP_
#include "KIM_CallbackName.hpp"
#endif

#ifndef KIM_MODEL_LIBRARY_HPP_
#include "KIM_ModelLibrary.hpp"
#endif


namespace KIM
{
// Forward declaration
class Log;

class ModelImplementation
{
 public:
  static int Create(Numbering const numbering,
                    LengthUnit const requestedLengthUnit,
                    EnergyUnit const requestedEnergyUnit,
                    ChargeUnit const requestedChargeUnit,
                    TemperatureUnit const requestedTemperatureUnit,
                    TimeUnit const requestedTimeUnit,
                    std::string const & modelName,
                    int * const requestedUnitsAccepted,
                    ModelImplementation ** const modelImplementation);
  static void Destroy(ModelImplementation ** const modelImplementation);

  void SetInfluenceDistancePointer(double const * const influenceDistance);
  void GetInfluenceDistance(double * const influenceDistance) const;


  void SetNeighborListCutoffsPointer(int const numberOfCutoffs,
                                     double const * const cutoffs);
  void GetNeighborListCutoffsPointer(int * const numberOfCutoffs,
                                     double const ** const cutoffs) const;


  int SetRefreshPointer(LanguageName const languageName, func * const fptr);
  int SetDestroyPointer(LanguageName const languageName, func * const fptr);
  int SetComputePointer(LanguageName const languageName, func * const fptr);


  int SetSpeciesCode(SpeciesName const speciesName, int const code);
  int GetSpeciesSupportAndCode(KIM::SpeciesName const speciesName,
                               int * const speciesIsSupported,
                               int * const code) const;


  int SetArgumentSupportStatus(ArgumentName const argumentName,
                               SupportStatus const supportStatus);
  int GetArgumentSupportStatus(ArgumentName const argumentName,
                               SupportStatus * const supportStatus) const;


  int SetCallbackSupportStatus(CallbackName const callbackName,
                               SupportStatus const supportStatus);
  int GetCallbackSupportStatus(CallbackName const callbackName,
                               SupportStatus * const supportStatus) const;

  int SetModelNumbering(Numbering const numbering);
 private:
  int SetSimulatorNumbering(Numbering const numbering);
 public:

  int SetUnits(LengthUnit const lengthUnit,
               EnergyUnit const energyUnit,
               ChargeUnit const chargeUnit,
               TemperatureUnit const temperatureUnit,
               TimeUnit const timeUnit);
  void GetUnits(LengthUnit * const lengthUnit,
                EnergyUnit * const energyUnit,
                ChargeUnit * const chargeUnit,
                TemperatureUnit * const temperatureUnit,
                TimeUnit * const timeUnit) const;


  int GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;
  int GetParameterFileName(int const index,
                           std::string * const parameterFileName) const;

  int SetParameterPointer(int const extent, int * const ptr,
                          std::string const & description);
  int SetParameterPointer(int const extent, double * const ptr,
                          std::string const & description);
  void GetNumberOfParameters(int * const numberOfParameters) const;
  int GetParameterDataTypeExtentAndDescription(
      int const parameterIndex, DataType * const dataType, int * const extent,
      std::string * const description) const;
  int GetParameter(int const parameterIndex, int const arrayIndex,
                   int * const parameterValue) const;
  int GetParameter(int const parameterIndex, int const arrayIndex,
                   double * const parameterValue) const;
  int SetParameter(int const parameterIndex, int const arrayIndex,
                   int const parameterValue);
  int SetParameter(int const parameterIndex, int const arrayIndex,
                   double const parameterValue);


  int SetArgumentPointer(ArgumentName const argumentName,
                         int const * const ptr);
  int SetArgumentPointer(ArgumentName const argumentName,
                         double const * const ptr);
  int GetArgumentPointer(ArgumentName const argumentName,
                         int const ** const ptr) const;
  int GetArgumentPointer(ArgumentName const argumentName,
                         int ** const ptr) const;
  int GetArgumentPointer(ArgumentName const argumentName,
                         double const ** const ptr) const;
  int GetArgumentPointer(ArgumentName const argumentName,
                         double ** const ptr) const;


  int SetCallbackPointer(CallbackName const callbackName,
                         LanguageName const languageName,
                         func * const fptr,
                         void const * const dataObject);
  int IsCallbackPresent(CallbackName const callbackName, int * const present)
      const;


  int Compute() const;
  int ClearInfluenceDistanceAndCutoffsThenRefreshModel();


  int GetNeighborList(int const neighborListIndex, int const particleNumber,
                      int * const numberOfNeighbors,
                      int const ** const neighborsOfParticle) const;

  int ProcessDEDrTerm(double const de, double const r, double const * const dx,
                      int const i, int const j) const;

  int ProcessD2EDr2Term(double const de, double const * const r,
                        double const * const dx, int const * const i,
                        int const * const j) const;




  void SetModelBufferPointer(void * const ptr);
  void GetModelBufferPointer(void ** const ptr) const;


  void SetSimulatorBufferPointer(void * const ptr);
  void GetSimulatorBufferPointer(void ** const ptr) const;

  int ConvertUnit(
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
      double * const conversionFactor) const;


  //@@@ better name for this?
  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();
  void LogEntry(LogVerbosity const logVerbosity, std::string const & message,
                int const lineNumber, std::string const & fileName) const;
  std::string String() const;

 private:
  // do not allow copy constructor or operator=
  ModelImplementation(ModelImplementation const &);
  void operator=(ModelImplementation const &);

  ModelImplementation(ModelLibrary * const modelLibrary, Log * const log);
  ~ModelImplementation();

  int ModelCreate(Numbering const numbering,
                  LengthUnit const requestedLengthUnit,
                  EnergyUnit const requestedEnergyUnit,
                  ChargeUnit const requestedChargeUnit,
                  TemperatureUnit const requestedTemperatureUnit,
                  TimeUnit const requestedTimeUnit,
                  std::string const & modelName);
  int ModelDestroy();


  ModelLibrary::ITEM_TYPE modelType_;
  std::string modelName_;
  std::string modelDriverName_;

  ModelLibrary * modelLibrary_;
  int numberOfParameterFiles_;
  std::vector<std::string> parameterFileNames_;

  Log * log_;

  int InitializeStandAloneModel(
      LengthUnit const requestedLengthUnit,
      EnergyUnit const requestedEnergyUnit,
      ChargeUnit const requestedChargeUnit,
      TemperatureUnit const requestedTemperatureUnit,
      TimeUnit const requestedTimeUnit);

  int InitializeParameterizedModel(
      LengthUnit const requestedLengthUnit,
      EnergyUnit const requestedEnergyUnit,
      ChargeUnit const requestedChargeUnit,
      TemperatureUnit const requestedTemperatureUnit,
      TimeUnit const requestedTimeUnit);

  int WriteParameterFiles();

  Numbering modelNumbering_;
  Numbering simulatorNumbering_;
  int numberingOffset_;


  LengthUnit lengthUnit_;
  EnergyUnit energyUnit_;
  ChargeUnit chargeUnit_;
  TemperatureUnit temperatureUnit_;
  TimeUnit timeUnit_;


  double const * influenceDistance_;


  int numberOfCutoffs_;
  double const * cutoffs_;


  LanguageName refreshLanguage_;
  func * refreshFunction_;
  LanguageName destroyLanguage_;
  func * destroyFunction_;
  LanguageName computeLanguage_;
  func * computeFunction_;


  std::map<SpeciesName const, int, SPECIES_NAME::Comparator> supportedSpecies_;


  std::map<ArgumentName const, SupportStatus, ARGUMENT_NAME::Comparator>
  argumentSupportStatus_;
  std::map<ArgumentName const, void *, ARGUMENT_NAME::Comparator>
  argumentPointer_;


  std::map<CallbackName const, SupportStatus, CALLBACK_NAME::Comparator>
  callbackSupportStatus_;
  std::map<CallbackName const, LanguageName, CALLBACK_NAME::Comparator>
  callbackLanguage_;
  std::map<CallbackName const, func *, CALLBACK_NAME::Comparator>
  callbackFunctionPointer_;
  std::map<CallbackName const, void const *, CALLBACK_NAME::Comparator>
  callbackDataObjectPointer_;

  mutable std::vector<std::vector<int> > getNeighborListStorage_;


  std::vector<std::string> parameterDescription_;
  std::vector<DataType> parameterDataType_;
  std::vector<int> parameterExtent_;
  std::vector<void *> parameterPointer_;

  void * modelBuffer_;
  void * simulatorBuffer_;

};  // class ModelImplementation
}  // namespace KIM
#endif  // KIM_MODEL_IMPLEMENTATION_HPP_
