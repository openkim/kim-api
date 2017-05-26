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


#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif

#ifndef KIM_ATTRIBUTE_HPP_
#include "KIM_Attribute.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

namespace KIM
{
int Model::create(Numbering const numbering,
                  LengthUnit const requestedLengthUnit,
                  EnergyUnit const requestedEnergyUnit,
                  ChargeUnit const requestedChargeUnit,
                  TemperatureUnit const requestedTemperatureUnit,
                  TimeUnit const requestedTimeUnit,
                  std::string const & modelName,
                  int * const requestedUnitsAccepted,
                  Model ** const model)
{
  *model = new Model();

  return ModelImplementation::create(numbering,
                                     requestedLengthUnit,
                                     requestedEnergyUnit,
                                     requestedChargeUnit,
                                     requestedTemperatureUnit,
                                     requestedTimeUnit,
                                     modelName,
                                     requestedUnitsAccepted,
                                     &((*model)->pimpl));
}

void Model::destroy(Model ** const model)
{
  ModelImplementation::destroy(&((*model)->pimpl));
  delete *model;
  *model = 0;
}

void Model::get_influence_distance(double * const influenceDistance) const
{
  pimpl->get_influence_distance(influenceDistance);
}

void Model::get_cutoffs(int * const numberOfCutoffs, double const ** cutoffs)
    const
{
  pimpl->get_cutoffs(numberOfCutoffs, cutoffs);
}

int Model::get_argument_attribute(ArgumentName const argumentName,
                                  Attribute * const attribute) const
{
  return pimpl->get_argument_attribute(argumentName, attribute);
}

int Model::get_call_back_attribute(CallBackName const callBackName,
                                   Attribute * const attribute) const
{
  return pimpl->get_call_back_attribute(callBackName, attribute);
}

void Model::get_units(LengthUnit * const lengthUnit,
                      EnergyUnit * const energyUnit,
                      ChargeUnit * const chargeUnit,
                      TemperatureUnit * const temperatureUnit,
                      TimeUnit * const timeUnit) const
{
  pimpl->get_units(lengthUnit, energyUnit, chargeUnit,temperatureUnit,
                   timeUnit);
}


int Model::set_data(ArgumentName const argumentName, int const * const ptr)
{
  return pimpl->set_data(argumentName, ptr);
}

int Model::set_data(ArgumentName const argumentName, double const * const ptr)
{
  return pimpl->set_data(argumentName, ptr);
}

int Model::set_call_back(CallBackName const callBackName,
                         LanguageName const languageName,
                         func * const fptr,
                         void const * const dataObject)
{
  return pimpl->set_call_back(callBackName, languageName, fptr, dataObject);
}

int Model::compute() const
{
  return pimpl->compute();
}

int Model::ClearInfluenceDistanceAndCutoffsThenReinitializeModel()
{
  return pimpl->ClearInfluenceDistanceAndCutoffsThenReinitializeModel();
}

int Model::get_species_support_and_code(SpeciesName const speciesName,
                                        int * const speciesIsSupported,
                                        int * const code) const
{
  return pimpl->get_species_support_and_code(speciesName, speciesIsSupported,
                                             code);
}


void Model::get_num_params(int * const numberOfParameters) const
{
  pimpl->get_num_params(numberOfParameters);
}

int Model::get_parameter_data_type_and_description(
    int const index, DataType * const dataType,
    std::string * const description) const
{
  return pimpl->get_parameter_data_type_and_description(index, dataType,
                                                        description);
}

int Model::get_parameter_extent_and_pointer(
    int const index, int * const extent, int const ** const ptr) const
{
  return pimpl->get_parameter_extent_and_pointer(index, extent, ptr);
}

int Model::get_parameter_extent_and_pointer(
    int const index, int * const extent, int ** const ptr)
{
  return pimpl->get_parameter_extent_and_pointer(index, extent, ptr);
}

int Model::get_parameter_extent_and_pointer(
    int const index, int * const extent, double const ** const ptr) const
{
  return pimpl->get_parameter_extent_and_pointer(index, extent, ptr);
}

int Model::get_parameter_extent_and_pointer(
    int const index, int * const extent, double ** const ptr)
{
  return pimpl->get_parameter_extent_and_pointer(index, extent, ptr);
}

void Model::set_sim_buffer(void * const ptr)
{
  pimpl->set_sim_buffer(ptr);
}

void Model::get_sim_buffer(void ** const ptr) const
{
  pimpl->get_sim_buffer(ptr);
}

std::string Model::string() const
{
  return pimpl->string();
}

void Model::SetLogID(std::string const & logID)
{
  pimpl->SetLogID(logID);
}

void Model::PushLogVerbosity(LogVerbosity const logVerbosity)
{
  pimpl->PushLogVerbosity(logVerbosity);
}

void Model::PopLogVerbosity()
{
  pimpl->PopLogVerbosity();
}

Model::Model() : pimpl(0)
{
}

Model::~Model()
{
}

}  // namespace KIM
