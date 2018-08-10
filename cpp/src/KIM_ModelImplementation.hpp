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
#include <sstream>
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

#ifndef KIM_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ComputeArguments.hpp"
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

  int ComputeArgumentsCreate(ComputeArguments ** const computeArguments) const;
  int ComputeArgumentsDestroy(ComputeArguments ** const computeArguments) const;

  void SetInfluenceDistancePointer(double const * const influenceDistance);
  void GetInfluenceDistance(double * const influenceDistance) const;


  void SetNeighborListPointers(
      int const numberOfNeighborLists,
      double const * const cutoffs,
      int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);
  void GetNeighborListPointers(
      int * const numberOfNeighborLists,
      double const ** const cutoffs,
      int const ** const
      modelWillNotRequestNeighborsOfNoncontributingParticles) const;


  int SetRefreshPointer(LanguageName const languageName, func * const fptr);
  int SetDestroyPointer(LanguageName const languageName, func * const fptr);
  int SetComputeArgumentsCreatePointer(LanguageName const languageName,
                                       func * const fptr);
  int SetComputeArgumentsDestroyPointer(LanguageName const languageName,
                                        func * const fptr);
  int SetComputePointer(LanguageName const languageName, func * const fptr);


  int SetSpeciesCode(SpeciesName const speciesName, int const code);
  int GetSpeciesSupportAndCode(KIM::SpeciesName const speciesName,
                               int * const speciesIsSupported,
                               int * const code) const;


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
                           std::string const ** const parameterFileName) const;

  int SetParameterPointer(int const extent, int * const ptr,
                          std::string const & description);
  int SetParameterPointer(int const extent, double * const ptr,
                          std::string const & description);
  void GetNumberOfParameters(int * const numberOfParameters) const;
  int GetParameterDataTypeExtentAndDescription(
      int const parameterIndex, DataType * const dataType, int * const extent,
      std::string const ** const description) const;
  int GetParameter(int const parameterIndex, int const arrayIndex,
                   int * const parameterValue) const;
  int GetParameter(int const parameterIndex, int const arrayIndex,
                   double * const parameterValue) const;
  int SetParameter(int const parameterIndex, int const arrayIndex,
                   int const parameterValue);
  int SetParameter(int const parameterIndex, int const arrayIndex,
                   double const parameterValue);


  int Compute(ComputeArguments const * const computeArguments) const;
  int ClearThenRefresh();


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


  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();
  void LogEntry(LogVerbosity const logVerbosity, std::string const & message,
                int const lineNumber, std::string const & fileName) const;
  void LogEntry(LogVerbosity const logVerbosity,
                std::stringstream const & message,
                int const lineNumber, std::string const & fileName) const;
  std::string const & String() const;

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

  int ModelComputeArgumentsCreate(ComputeArguments * const computeArguments)
      const;
  int ModelComputeArgumentsDestroy(ComputeArguments * const computeArguments)
      const;


  int Validate(ChargeUnit const chargeUnit) const;
  int Validate(DataType const dataType) const;
  int Validate(EnergyUnit const energyUnit) const;
  int Validate(LanguageName const languageName) const;
  int Validate(LengthUnit const lengthUnit) const;
  int Validate(Numbering const numbering) const;
  int Validate(SpeciesName const speciesName) const;
  int Validate(SupportStatus const supportStatus) const;
  int Validate(TemperatureUnit const temperatureUnit) const;
  int Validate(TimeUnit const timeUnit) const;

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

  bool numberingHasBeenSet_;
  Numbering modelNumbering_;
  Numbering simulatorNumbering_;
  int numberingOffset_;

  bool unitsHaveBeenSet_;
  LengthUnit lengthUnit_;
  EnergyUnit energyUnit_;
  ChargeUnit chargeUnit_;
  TemperatureUnit temperatureUnit_;
  TimeUnit timeUnit_;


  double const * influenceDistance_;


  int numberOfNeighborLists_;
  double const * cutoffs_;
  int const * modelWillNotRequestNeighborsOfNoncontributingParticles_;


  LanguageName refreshLanguage_;
  func * refreshFunction_;
  LanguageName destroyLanguage_;
  func * destroyFunction_;
  LanguageName computeArgumentsCreateLanguage_;
  func * computeArgumentsCreateFunction_;
  LanguageName computeArgumentsDestroyLanguage_;
  func * computeArgumentsDestroyFunction_;
  LanguageName computeLanguage_;
  func * computeFunction_;


  std::map<SpeciesName const, int, SPECIES_NAME::Comparator> supportedSpecies_;


  std::vector<std::string> parameterDescription_;
  std::vector<DataType> parameterDataType_;
  std::vector<int> parameterExtent_;
  std::vector<void *> parameterPointer_;

  void * modelBuffer_;
  void * simulatorBuffer_;

  mutable std::string string_;

};  // class ModelImplementation
}  // namespace KIM
#endif  // KIM_MODEL_IMPLEMENTATION_HPP_
