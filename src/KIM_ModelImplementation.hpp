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


#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#define KIM_MODEL_IMPLEMENTATION_HPP_

#include <string>
#include <unordered_map>
#include <vector>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
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

#ifndef KIM_ATTRIBUTE_HPP_
#include "KIM_Attribute.hpp"
#endif

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif

#ifndef KIM_MODEL_LIBRARY_HPP_
#include "KIM_ModelLibrary.hpp"
#endif


namespace KIM
{
// Forward declaration


class ModelImplementation
{
 public:
  static int create(Numbering const numbering,
                    LengthUnit const requestedLengthUnit,
                    EnergyUnit const requestedEnergyUnit,
                    ChargeUnit const requestedChargeUnit,
                    TemperatureUnit const requestedTemperatureUnit,
                    TimeUnit const requestedTimeUnit,
                    std::string const & modelName,
                    int * const requestedUnitsAccepted,
                    ModelImplementation ** const modelImplementation);
  static void destroy(ModelImplementation ** const modelImplementation);

  void set_influence_distance(double const * const influenceDistance);
  void get_influence_distance(double * const influenceDistance) const;


  void set_cutoffs(int const numberOfCutoffs, double const * const cutoffs);
  // allows NULL as value of cutoffs (to get just numberOfCutoffs)
  void get_cutoffs(int * const numberOfCutoffs, double const ** const cutoffs)
      const;


  int set_reinit(LanguageName const languageName, func * const fptr);
  int set_destroy(LanguageName const languageName, func * const fptr);
  int set_compute_func(LanguageName const languageName, func * const fptr);


  int set_species_code(SpeciesName const speciesName, int const code);
  int get_species_support_and_code(KIM::SpeciesName const speciesName,
                                   int * const speciesIsSupported,
                                   int * const code) const;


  int set_argument_attribute(ArgumentName const argumentName,
                             Attribute const attribute);
  int get_argument_attribute(ArgumentName const argumentName,
                             Attribute * const attribute) const;


  int set_call_back_attribute(CallBackName const callBackName,
                              Attribute const attribute);
  int get_call_back_attribute(CallBackName const callBackName,
                              Attribute * const attribute) const;

  int set_model_numbering(Numbering const numbering);
  int set_simulator_numbering(Numbering const numbering);

  int set_units(LengthUnit const lengthUnit,
                EnergyUnit const energyUnit,
                ChargeUnit const chargeUnit,
                TemperatureUnit const temperatureUnit,
                TimeUnit const timeUnit);
  void get_units(LengthUnit * const lengthUnit,
                 EnergyUnit * const energyUnit,
                 ChargeUnit * const chargeUnit,
                 TemperatureUnit * const temperatureUnit,
                 TimeUnit * const timeUnit) const;


  int get_number_of_parameter_files(int * const numberOfParameterFiles) const;
  int get_parameter_file_name(int const index,
                              std::string * const parameterFileName) const;

  int set_parameter(int const extent, int * const ptr,
                    std::string const & description);
  int set_parameter(int const extent, double * const ptr,
                    std::string const & description);
  void get_num_params(int * const numberOfParameters) const;
  int get_parameter_data_type_and_description(
      int const index, DataType * const dataType,
      std::string * const description) const;
  int get_parameter_extent_and_pointer(int const index, int * extent,
                                       int ** const ptr);
  int get_parameter_extent_and_pointer(int const index, int * extent,
                                       int const ** const ptr) const;
  int get_parameter_extent_and_pointer(int const index, int * extent,
                                       double ** const ptr);
  int get_parameter_extent_and_pointer(int const index, int * extent,
                                       double const ** const ptr) const;


  int set_data(ArgumentName const argumentName, int const * const ptr);
  int set_data(ArgumentName const argumentName, double const * const ptr);
  int get_data(ArgumentName const argumentName, int const ** const ptr) const;
  int get_data(ArgumentName const argumentName, int ** const ptr) const;
  int get_data(ArgumentName const argumentName, double const ** const ptr)
      const;
  int get_data(ArgumentName const argumentName, double ** const ptr) const;


  int set_call_back(CallBackName const callBackName,
                    LanguageName const languageName,
                    func * const fptr,
                    void const * const dataObject);
  int is_call_back_present(CallBackName const callBackName, int * const present)
      const;


  int compute() const;
  int ClearInfluenceDistanceAndCutoffsThenReinitializeModel();


  int get_neigh(int const neighborListIndex, int const particleNumber,
                int * const numberOfNeighbors,
                int const ** const neighborsOfParticle) const;

  int process_dEdr(double const de, double const r, double const * const dx,
                   int const i, int const j) const;

  int process_d2Edr2(double const de, double const * const r,
                     double const * const dx, int const * const i,
                     int const * const j) const;




  void set_model_buffer(void * const ptr);
  void get_model_buffer(void ** const ptr) const;


  void set_sim_buffer(void * const ptr);
  void get_sim_buffer(void ** const ptr) const;

  int convert_unit(
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
  void Log(LogVerbosity const logVerbosity, std::string const & message,
           int const lineNumber, std::string const & fileName) const;
  std::string string() const;

 private:
  // do not allow copy constructor or operator=
  ModelImplementation(ModelImplementation const &);
  void operator=(ModelImplementation const &);

  ModelImplementation(ModelLibrary * const modelLibrary);
  ~ModelImplementation();

  int ModelInitialization(Numbering const numbering,
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


  LanguageName reinitializationLanguage_;
  func * reinitializationFunction_;
  LanguageName destroyLanguage_;
  func * destroyFunction_;
  LanguageName computeLanguage_;
  func * computeFunction_;


  std::unordered_map<SpeciesName const, int> supportedSpecies_;


  std::unordered_map<ArgumentName const, Attribute> argumentAttribute_;
  std::unordered_map<ArgumentName const, void *> argumentPointer_;


  std::unordered_map<CallBackName const, Attribute> callBackAttribute_;
  std::unordered_map<CallBackName const, LanguageName> callBackLanguage_;
  std::unordered_map<CallBackName const, func *> callBackFunctionPointer_;
  std::unordered_map<CallBackName const, void const *>
  callBackDataObjectPointer_;

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
