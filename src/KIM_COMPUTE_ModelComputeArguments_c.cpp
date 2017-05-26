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

#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_HPP_
#include "KIM_COMPUTE_ModelComputeArguments.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

extern "C"
{
#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_H_
#include "KIM_COMPUTE_ModelComputeArguments.h"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_COMPUTE_ArgumentName.h"
#endif
}  // extern "C"

namespace
{
KIM::COMPUTE::ArgumentName
makeArgumentNameCpp(KIM_COMPUTE_ArgumentName const argumentName)
{
  return KIM::COMPUTE::ArgumentName(argumentName.argumentID);
}

KIM::LanguageName
makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageID);
}
}  // namespace


extern "C"
{
void KIM_COMPUTE_ModelComputeArguments_set_get_neigh(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_LanguageName const languageName,
    func * const fptr)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pArguments->set_get_neigh(langN, fptr);
}

void KIM_COMPUTE_ModelComputeArguments_set_neighObject(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    void const * const ptr)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  return pArguments->set_neighObject(ptr);
}

void KIM_COMPUTE_ModelComputeArguments_set_process_dEdr(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_LanguageName const languageName,
    func * const fptr)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pArguments->set_process_dEdr(langN, fptr);
}

void KIM_COMPUTE_ModelComputeArguments_set_process_d2Edr2(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_LanguageName const languageName,
    func * const fptr)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pArguments->set_process_d2Edr2(langN, fptr);
}


// *data functions
int KIM_COMPUTE_ModelComputeArguments_set_data(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int const extent, void const * const ptr)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pArguments->set_data(argN, extent, ptr);
}

int KIM_COMPUTE_ModelComputeArguments_set_compute(
    KIM_COMPUTE_ModelComputeArguments * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName, int flag)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pArguments->set_compute(argN, flag);
}

void KIM_COMPUTE_ModelComputeArguments_set_process_dEdr_compute(
    KIM_COMPUTE_ModelComputeArguments * const arguments, int flag)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  pArguments->set_process_dEdr_compute(flag);
}

void KIM_COMPUTE_ModelComputeArguments_set_process_d2Edr2_compute(
    KIM_COMPUTE_ModelComputeArguments * const arguments, int flag)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  pArguments->set_process_d2Edr2_compute(flag);
}

int KIM_COMPUTE_ModelComputeArguments_get_size(
    KIM_COMPUTE_ModelComputeArguments const * const arguments,
    KIM_COMPUTE_ArgumentName const argumentName,
    int * const size)
{
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  KIM::COMPUTE::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pArguments->get_size(argN, size);
}
}  // extern "C"
