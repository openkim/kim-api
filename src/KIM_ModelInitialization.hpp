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


#ifndef KIM_MODEL_INITIALIZATION_HPP_
#define KIM_MODEL_INITIALIZATION_HPP_


#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif


namespace KIM
{
// Forward declarations
class LogVerbosity;
class LanguageName;
class Numbering;
class SpeciesName;
class Attribute;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
class ArgumentName;
class CallBackName;


class ModelInitialization
{
 public:
  int set_model_numbering(Numbering const numbering);

  // stores pointer value
  void set_influence_distance(double const * const influenceDistance);

  // stores pointer value
  void set_cutoffs(int const numberOfCutoffs, double const * const cutoffs);

  // method functions
  int set_reinit(LanguageName const languageName, func * const fptr);
  int set_destroy(LanguageName const languageName, func * const fptr);
  int set_compute_func(LanguageName const languageName, func * const fptr);

  int set_species_code(SpeciesName const speciesName, int const code);

  int set_argument_attribute(ArgumentName const argumentName,
                             Attribute const attribute);

  int set_call_back_attribute(CallBackName const callBackName,
                              Attribute const attribute);

  int set_parameter(int const extent, int * const ptr,
                    std::string const & description);
  int set_parameter(int const extent, double * const ptr,
                    std::string const & description);

  void set_model_buffer(void * const ptr);

  int set_units(LengthUnit const lengthUnit,
                EnergyUnit const energyUnit,
                ChargeUnit const chargeUnit,
                TemperatureUnit const temperatureUnit,
                TimeUnit const timeUnit);

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

  void Log(LogVerbosity const logVerbosity, std::string const & message,
           int const lineNumber, std::string const & fileName) const;
  std::string string() const;

 private:
  // do not allow copy constructor or operator=
  ModelInitialization(ModelInitialization const &);
  void operator=(ModelInitialization const &);

  ModelInitialization();
  ~ModelInitialization();

  class ModelInitializationImplementation;
  ModelInitializationImplementation * pimpl;
};  // class ModelInitialization
}  // namespace KIM
#endif  // KIM_MODEL_INITIALIZATION_HPP_
