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


#ifndef KIM_MODEL_HPP_
#define KIM_MODEL_HPP_

#include <string>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

namespace KIM
{
// Forward declarations
class LogVerbosity;
class DataType;
class LanguageName;
class SpeciesName;
class Numbering;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ArgumentName;
class CallBackName;
class Attribute;
class ModelImplementation;


class Model
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
                    Model ** const model);
  static void destroy(Model ** const model);

  void get_influence_distance(double * const influenceDistance) const;

  // allows NULL as value of cutoffs (to get just numberOfCutoffs)
  void get_cutoffs(int * const numberOfCutoffs, double const ** const cutoffs)
      const;

  int get_argument_attribute(ArgumentName const argumentName,
                             Attribute * const attribute) const;
  int get_call_back_attribute(CallBackName const callBackName,
                              Attribute * const attribute) const;

  void get_units(LengthUnit * const lengthUnit,
                 EnergyUnit * const energyUnit,
                 ChargeUnit * const chargeUnit,
                 TemperatureUnit * const temperatureUnit,
                 TimeUnit * const timeUnit) const;


  // data functions
  int set_data(ArgumentName const argumentName, int const * const ptr);
  int set_data(ArgumentName const argumentName, double const * const ptr);

  int set_call_back(CallBackName const callBackName,
                    LanguageName const languageName,
                    func * const fptr,
                    void const * const dataObject);

  int compute() const;
  int ClearInfluenceDistanceAndCutoffsThenReinitializeModel();

  int get_species_support_and_code(KIM::SpeciesName const speciesName,
                                   int * const speciesIsSupported,
                                   int * const code) const;

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

  void set_sim_buffer(void * const ptr);
  void get_sim_buffer(void ** const ptr) const;

  std::string string() const;

  void SetLogID(std::string const & logID);
  void PushLogVerbosity(LogVerbosity const logVerbosity);
  void PopLogVerbosity();

 private:
  // do not allow copy constructor or operator=
  Model(Model const &);
  void operator=(Model const &);

  Model();
  ~Model();

  ModelImplementation * pimpl;
};  // class Model
}  // namespace KIM
#endif  // KIM_MODEL_HPP_
