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


#ifndef KIM_SIMULATOR_HPP_
#define KIM_SIMULATOR_HPP_

#include <string>

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

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
}


class Simulator{
 public:
  // stores pointer value
  void set_influence_distance(double * const influenceDistance);

  // stores pointer value
  void set_cutoffs(int const numberOfCutoffs, double const * const cutoffs);

  // method functions
  void set_reinit(LanguageName const languageName, func * const fptr);
  void set_destroy(LanguageName const languageName, func * const fptr);
  void set_compute_func(LanguageName const languageName,
                        func * const fptr);

  // @@@ below move to KIM::COMPUTE::ArgumentList class
  // data functions
  int get_data(COMPUTE::ArgumentName const argumentName, void ** const ptr)
      const;

  // compute functions
  int get_compute(COMPUTE::ArgumentName const argumentName, int * const flag)
      const;

  // who uses this?
  int get_size(COMPUTE::ArgumentName const argumentName, int * const size)
      const;
  // @@@ above move to KIM::COMPUTE::ArgumentList class

  // @@@ add ostream argument to print() (?)
  void print() const;

  void get_neighObject(void ** const ptr) const;
  int get_neigh(int const neighborListIndex, int const particleNumber,
                int * const numberOfNeighbors,
                int const ** const neighborsOfParticle) const;

  // @@@ how would the Model use this inforamtion? can these be removed?
  void get_num_model_species(int * const numberOfSpecies) const;
  int get_model_species(int const index, KIM::SpeciesName * const speciesName)
      const;

  // @@@ these will go away
  void get_num_sim_species(int * const numberOfSpecies) const;
  int get_sim_species(int const index, KIM::SpeciesName * const speciesName)
      const;

  int get_species_code(KIM::SpeciesName const speciesName, int * const code)
      const;
  // @@@ do we keep this mechanism, make it mandatory, or remove?
  int set_species_code(KIM::SpeciesName const speciesName, int const code);

  int set_parameter(int const index, int const extent, void * const ptr);
  int set_parameter_description(int const index,
                                std::string const & description);

  void set_model_buffer(void const * const ptr);
  void get_model_buffer(void ** const ptr) const;

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
  DISALLOW_COPY_AND_ASSIGN(Simulator);

  Simulator();
  ~Simulator();

  class SimulatorImplementation;
  SimulatorImplementation * pimpl;
};  // class Simulator
}  // namespace KIM
#endif  // KIM_SIMULATOR_HPP_
