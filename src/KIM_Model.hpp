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

namespace OLD_KIM
{
class KIM_API_model;
}

// A macro to disallow the copy constructor and operator= functions.
// This should be used in the private: declarations for a class
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)

namespace KIM
{
typedef void (func)();

// Forward declarations
class SpeciesName;
class ParameterDataType;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
namespace COMPUTE
{
class ArgumentName;
class LanguageName;
}


class Model{
 public:
  static int create(std::string const & simulatorString,
                    std::string const & modelName,
                    Model ** const model);
  static void destroy(Model ** const model);

  // @@@ below move to KIM::COMPUTE::ArgumentList class
  // data functions
  int get_data(COMPUTE::ArgumentName const argumentName, void ** const ptr)
      const;
  int set_data(COMPUTE::ArgumentName const argumentName, int const extent,
               void const * const ptr);

  // method functions
  int get_method(COMPUTE::ArgumentName const argumentName,
                 COMPUTE::LanguageName * const languageName,
                 func ** const fptr) const;
  int set_method(COMPUTE::ArgumentName const argumentName, int const extent,
                 COMPUTE::LanguageName const languageName,
                 func const * const fptr);

  // compute functions
  int get_compute(COMPUTE::ArgumentName const argumentName, int * const flag)
      const;
  int set_compute(COMPUTE::ArgumentName const argumentName, int flag);

  int get_size(COMPUTE::ArgumentName const argumentName, int * const size)
      const;
  // @@@ above move to KIM::COMPUTE::ArgumentList class

  // @@@ add ostream argument to print() (?)
  void print() const;

  int compute() const;  // @@@ add KIM::COMPUTE::ArgumentList argument
  int get_neigh(int particleNumber, int * const numberOfNeighbors,
                int const ** const neighborsOfParticle) const;
  int init();  // @@@ merge into create
  int reinit();  // @@@ rename to recreate (or other name)
  int destroy_model();  // @@@ merge into destroy

  void get_num_model_species(int * const numberOfSpecies) const;
  int get_model_species(int const index, KIM::SpeciesName * const speciesName)
      const;
  // @@@ these will go away
  void get_num_sim_species(int * const numberOfSpecies) const;
  int get_sim_species(int const index, KIM::SpeciesName * const speciesName)
      const;
  static int get_model_kim_string(std::string const & modelName,
                                  std::string * const kimString);

  int get_species_code(KIM::SpeciesName const speciesName, int * const code)
      const;
  // @@@ do we keep this mechanism, make it mandatory, or remove?
  int set_species_code(KIM::SpeciesName const speciesName, int const code);

  void get_num_params(int * const numberOfParameters) const;
  int get_parameter_data_type(int const index,
                              ParameterDataType * const dataType) const;
  int get_parameter(int const index, int * const extent, void ** const ptr);
  int set_parameter(int const index, int const extent, void * const ptr);
  int get_parameter_description(int const index,
                                std::string * const description) const;
  int set_parameter_description(int const index,
                                std::string const & description);

  void set_model_buffer(void const * const ptr);
  void get_model_buffer(void ** const ptr) const;
  void set_sim_buffer(void const * const ptr);
  void get_sim_buffer(void ** const ptr) const;

  int process_dEdr(double const de, double const r, double const * const dx,
                   int const i, int const j) const;

  int process_d2Edr2(double const de, double const * const r,
                     double const * const dx, int const * const i,
                     int const * const j) const;

  int get_unit_handling(int * const flag) const;
  // @@@ to be moved to a UnitSystem class
  void get_unit_length(LengthUnit * const length) const;
  void get_unit_energy(EnergyUnit * const energy) const;
  void get_unit_charge(ChargeUnit * const charge) const;
  void get_unit_temperature(TemperatureUnit * const temperature) const;
  void get_unit_time(TimeUnit * const time) const;
  int convert_to_act_unit(
      LengthUnit const length,
      EnergyUnit const energy,
      ChargeUnit const charge,
      TemperatureUnit const temperature,
      TimeUnit const time,
      double const length_exponent,
      double const energy_exponent,
      double const charge_exponent,
      double const temperature_exponent,
      double const time_exponent,
      double * const factor) const;

  // @@@ to be removed
  friend class OLD_KIM::KIM_API_model;

 private:
  // do not allow copy constructor or operator=
  DISALLOW_COPY_AND_ASSIGN(Model);

  Model();
  ~Model();

  class ModelImplementation;
  ModelImplementation * pimpl;
};  // class Model
}  // namespace KIM
#endif  // KIM_MODEL_HPP_
