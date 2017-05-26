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


#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
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

#ifndef KIM_MODEL_DRIVER_INITIALIZATION_HPP_
#include "KIM_ModelDriverInitialization.hpp"
#endif

#ifndef KIM_MODEL_IMPLEMENTATION_HPP_
#include "KIM_ModelImplementation.hpp"
#endif

#define CONVERT_POINTER ModelImplementation *pImpl      \
  = reinterpret_cast<ModelImplementation *>(pimpl)


namespace KIM
{
void ModelDriverInitialization::get_number_of_parameter_files(
    int * const numberOfParameterFiles) const
{
  CONVERT_POINTER;

  pImpl->get_number_of_parameter_files(numberOfParameterFiles);
}

int ModelDriverInitialization::get_parameter_file_name(
    int const index, std::string * const parameterFileName) const
{
  CONVERT_POINTER;

  return pImpl->get_parameter_file_name(index, parameterFileName);
}


int ModelDriverInitialization::set_model_numbering(Numbering const numbering)
{
  CONVERT_POINTER;

  return pImpl->set_model_numbering(numbering);
}

void ModelDriverInitialization::set_influence_distance(
    double const * const influenceDistance)
{
  CONVERT_POINTER;

  pImpl->set_influence_distance(influenceDistance);
}

void ModelDriverInitialization::set_cutoffs(int const numberOfCutoffs,
                                            double const * const cutoffs)
{
  CONVERT_POINTER;

  pImpl->set_cutoffs(numberOfCutoffs, cutoffs);
}

int ModelDriverInitialization::set_reinit(LanguageName const languageName,
                                          func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->set_reinit(languageName, fptr);
}

int ModelDriverInitialization::set_destroy(LanguageName const languageName,
                                           func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->set_destroy(languageName, fptr);
}

int ModelDriverInitialization::set_compute_func(LanguageName const languageName,
                                                func * const fptr)
{
  CONVERT_POINTER;

  return pImpl->set_compute_func(languageName, fptr);
}

int ModelDriverInitialization::set_species_code(SpeciesName const speciesName,
                                                int const code)
{
  CONVERT_POINTER;

  return pImpl->set_species_code(speciesName, code);
}

int ModelDriverInitialization::set_argument_attribute(
    ArgumentName const argumentName, Attribute const attribute)
{
  CONVERT_POINTER;

  return pImpl->set_argument_attribute(argumentName, attribute);
}

int ModelDriverInitialization::set_call_back_attribute(
    CallBackName const callBackName, Attribute const attribute)
{
  CONVERT_POINTER;

  return pImpl->set_call_back_attribute(callBackName, attribute);
}

int ModelDriverInitialization::set_parameter(int const extent, int * const ptr,
                                             std::string const & description)
{
  CONVERT_POINTER;

  return pImpl->set_parameter(extent, ptr, description);
}

int ModelDriverInitialization::set_parameter(int const extent,
                                             double * const ptr,
                                             std::string const & description)
{
  CONVERT_POINTER;

  return pImpl->set_parameter(extent, ptr, description);
}

void ModelDriverInitialization::set_model_buffer(void * const ptr)
{
  CONVERT_POINTER;

  pImpl->set_model_buffer(ptr);
}


int ModelDriverInitialization::set_units(LengthUnit const lengthUnit,
                                         EnergyUnit const energyUnit,
                                         ChargeUnit const chargeUnit,
                                         TemperatureUnit const temperatureUnit,
                                         TimeUnit const timeUnit)
{
  CONVERT_POINTER;

  return pImpl->set_units(lengthUnit, energyUnit, chargeUnit,
                          temperatureUnit, timeUnit);
}


int ModelDriverInitialization::convert_unit(
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
  CONVERT_POINTER;

  return pImpl->convert_unit(
      fromLengthUnit,
      fromEnergyUnit,
      fromChargeUnit,
      fromTemperatureUnit,
      fromTimeUnit,
      toLengthUnit,
      toEnergyUnit,
      toChargeUnit,
      toTemperatureUnit,
      toTimeUnit,
      lengthExponent,
      energyExponent,
      chargeExponent,
      temperatureExponent,
      timeExponent,
      conversionFactor);
}

void ModelDriverInitialization::Log(LogVerbosity const logVerbosity,
                                    std::string const & message,
                                    int const lineNumber,
                                    std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->Log(logVerbosity, message, lineNumber, fileName);
}

std::string ModelDriverInitialization::string() const
{
  CONVERT_POINTER;

  return pImpl->string();
}

ModelDriverInitialization::ModelDriverInitialization() : pimpl(0)
{
}

ModelDriverInitialization::~ModelDriverInitialization()
{
}

}  // namespace KIM
