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
// Forward declarations
class DataType;
class SpeciesName;
class LengthUnit;
class EnergyUnit;
class ChargeUnit;
class TemperatureUnit;
class TimeUnit;
namespace COMPUTE
{
class ArgumentName;
class ModelComputeArguments;
}


class Model
{
 public:
  static int create(std::string const & simulatorString,
                    std::string const & modelName,
                    Model ** const model);
  static void destroy(Model ** const model);

  int create_compute_arguments(
      COMPUTE::ModelComputeArguments ** const arguments) const;
  void destroy_compute_arguments(
      COMPUTE::ModelComputeArguments ** const arguments) const;

  void get_influence_distance(double * const influenceDistance) const;

  // allows NULL as value of cutoffs (to get just numberOfCutoffs)
  void get_cutoffs(int * const numberOfCutoffs, double const ** const cutoffs)
      const;

  // @@@ add ostream argument to print() (?)
  void print() const;

  int compute(COMPUTE::ModelComputeArguments const * const arguments) const;
  int reinit();  // @@@ rename to recreate (or other name)

  // @@@ how should this work?  maybe a "IsSpeciesSupported"?
  // @@@ OR is this needed, since we have get_species_code?
  void get_num_model_species(int * const numberOfSpecies) const;
  int get_model_species(int const index, KIM::SpeciesName * const speciesName)
      const;

  // @@@ this will go away
  static int get_model_kim_string(std::string const & modelName,
                                  std::string * const kimString);

  int get_species_code(KIM::SpeciesName const speciesName, int * const code)
      const;

  void get_num_params(int * const numberOfParameters) const;
  int get_parameter_data_type(int const index, DataType * const dataType) const;
  int get_parameter(int const index, int * const extent, void ** const ptr);
  int get_parameter_description(int const index,
                                std::string * const description) const;

  void set_sim_buffer(void const * const ptr);
  void get_sim_buffer(void ** const ptr) const;

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
