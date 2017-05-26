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

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_HPP_
#include "KIM_COMPUTE_ModelComputeArguments.hpp"
#endif

#include "old_KIM_API.h"
#include "old_KIM_API_status.h"

namespace KIM
{
namespace COMPUTE
{

namespace
{
char const * const argumentString(COMPUTE::ArgumentName const argumentName)
{
  if (argumentName == COMPUTE::ARGUMENT_NAME::numberOfParticles)
    return "numberOfParticles";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::numberOfSpecies)
    return "numberOfSpecies";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::particleSpecies)
    return "particleSpecies";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::particleContributing)
    return "particleContributing";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::coordinates)
    return "coordinates";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::energy)
    return "energy";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::forces)
    return "forces";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::particleEnergy)
    return "particleEnergy";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::virial)
    return "virial";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::particleVirial)
    return "particleVirial";
  else if (argumentName == COMPUTE::ARGUMENT_NAME::hessian)
    return "hessian";
  else
    return "None";
}
}  // namespace

void ModelComputeArguments::set_get_neigh(LanguageName const languageName,
                          func * const fptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;
  int langN;
  if (languageName == KIM::LANGUAGE_NAME::Cpp)
    langN = 1;
  else if (languageName == KIM::LANGUAGE_NAME::C)
    langN = 2;
  else  // Fortran
    langN = 3;

  pKIM->set_method("get_neigh", (intptr_t) 1, langN, (func *) fptr);
  //return (err < KIM_STATUS_OK);
}

void ModelComputeArguments::set_neighObject(void const * const ptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  pKIM->set_data("neighObject", (intptr_t) 1, (void *) ptr);
  //return (err < KIM_STATUS_OK);
}


void ModelComputeArguments::set_process_dEdr(LanguageName const languageName,
                                             func * const fptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;
  int langN;
  if (languageName == KIM::LANGUAGE_NAME::Cpp)
    langN = 1;
  else if (languageName == KIM::LANGUAGE_NAME::C)
    langN = 2;
  else  // Fortran
    langN = 3;

  pKIM->set_method("process_dEdr", (intptr_t) 1, langN, (func *) fptr);
  //return (err < KIM_STATUS_OK);
}

void ModelComputeArguments::set_process_d2Edr2(LanguageName const languageName,
                                               func * const fptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;
  int langN;
  if (languageName == KIM::LANGUAGE_NAME::Cpp)
    langN = 1;
  else if (languageName == KIM::LANGUAGE_NAME::C)
    langN = 2;
  else  // Fortran
    langN = 3;

  pKIM->set_method("process_d2Edr2", (intptr_t) 1, langN, (func *) fptr);
  //return (err < KIM_STATUS_OK);
}

int ModelComputeArguments::set_data(COMPUTE::ArgumentName const argumentName,
                    int const extent, void const * const ptr)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err = pKIM->set_data(argumentString(argumentName), (intptr_t) extent,
                           (void *) ptr);
  return (err < KIM_STATUS_OK);
}

int ModelComputeArguments::set_compute(COMPUTE::ArgumentName const argumentName,
                                       int flag)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  pKIM->set_compute(argumentString(argumentName), flag, &err);
  return (err < KIM_STATUS_OK);
}

void ModelComputeArguments::set_process_dEdr_compute(int flag)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  pKIM->set_compute("process_dEdr", flag, &err);
}

void ModelComputeArguments::set_process_d2Edr2_compute(int flag)
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  pKIM->set_compute("process_d2Edr2", flag, &err);
}

int ModelComputeArguments::get_size(COMPUTE::ArgumentName const argumentName,
                                    int * const size) const
{
  OLD_KIM::KIM_API_model * pKIM = (OLD_KIM::KIM_API_model *) pimpl;

  int err;
  *size = pKIM->get_size(argumentString(argumentName), &err);
  return (err < KIM_STATUS_OK);
}

ModelComputeArguments::ModelComputeArguments() : pimpl(0)
{
}

ModelComputeArguments::~ModelComputeArguments()
{
}

}  // namespace COMPUTE
}  // namespace KIM
