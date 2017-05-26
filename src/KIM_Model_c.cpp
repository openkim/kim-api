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

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif

#ifndef KIM_PARAMETER_HPP_
#include "KIM_Parameter.hpp"
#endif

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_COMPUTE_ArgumentName.hpp"
#endif

extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif

#ifndef KIM_PARAMETER_H_
#include "KIM_Parameter.h"
#endif

#ifndef KIM_MODEL_H_
#include "KIM_Model.h"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_H_
#include "KIM_COMPUTE_ArgumentName.h"
#endif

#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_H_
#include "KIM_COMPUTE_ModelComputeArguments.h"
#endif
}  // extern "C"

namespace
{
KIM::COMPUTE::ArgumentName
makeArgumentNameCpp(KIM_COMPUTE_ArgumentName const argumentName)
{
  return KIM::COMPUTE::ArgumentName(argumentName.argumentID);
}

KIM_ParameterDataType
makeParameterDataTypeC(KIM::ParameterDataType const dataType)
{
  KIM_ParameterDataType typ;
  KIM_ParameterDataType * pTyp = (KIM_ParameterDataType *) &dataType;
  typ.dataTypeID = pTyp->dataTypeID;
  return typ;
}

KIM::LanguageName
makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageID);
}

KIM::SpeciesName makeSpecNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesID);
}

KIM_SpeciesName makeSpecNameC(KIM::SpeciesName const speciesName)
{
  KIM_SpeciesName specN;
  KIM_SpeciesName *pSN = (KIM_SpeciesName*) & speciesName;
  specN.speciesID = pSN->speciesID;
  return specN;
}

}  // namespace


extern "C"
{
int KIM_Model_create(char const * const simulatorString,
                     char const * const modelName,
                     KIM_Model ** const model)
{
  std::string sim(simulatorString);
  std::string mod(modelName);
  KIM::Model * pmodel;
  int err = KIM::Model::create(sim, mod, &pmodel);
  (*model) = new KIM_Model;
  (*model)->p = (void *) pmodel;
  return err;
}

void KIM_Model_destroy(KIM_Model ** const model)
{
  KIM::Model * pmodel = (KIM::Model *) (*model)->p;
  KIM::Model::destroy(&pmodel);
  delete (*model);
  *model = 0;
}

int KIM_Model_create_compute_arguments(
    KIM_Model const * const model,
    KIM_COMPUTE_ModelComputeArguments ** const arguments)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ModelComputeArguments * pArguments;
  int err = pmodel->create_compute_arguments(&pArguments);
  (*arguments) = new KIM_COMPUTE_ModelComputeArguments;
  (*arguments)->p = (void *) pArguments;
  return err;
}

void KIM_Model_destroy_compute_arguments(
    KIM_Model const * const model,
    KIM_COMPUTE_ModelComputeArguments ** const arguments)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ModelComputeArguments *
      pArguments = (KIM::COMPUTE::ModelComputeArguments *) (*arguments)->p;
  pmodel->destroy_compute_arguments(& pArguments);
  delete(*arguments);
  *arguments = 0;
}

void KIM_Model_get_influence_distance(KIM_Model const * const model,
                                      double * const influenceDistance)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_influence_distance(influenceDistance);
}

void KIM_Model_get_cutoffs(KIM_Model const * const model,
                           int * const numberOfCutoffs,
                           double const ** const cutoffs)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_cutoffs(numberOfCutoffs, cutoffs);
}

void KIM_Model_print(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->print();
}

int KIM_Model_compute(KIM_Model const * const model,
                      KIM_COMPUTE_ModelComputeArguments const * const arguments)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::COMPUTE::ModelComputeArguments *
      pMCA = (KIM::COMPUTE::ModelComputeArguments *) arguments->p;
  return pmodel->compute(pMCA);
}

int KIM_Model_reinit(KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->reinit();
}

void KIM_Model_get_num_model_species(KIM_Model const * const model,
                                     int * const numberOfSpecies)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_num_model_species(numberOfSpecies);
}

int KIM_Model_get_model_species(KIM_Model const * const model,
                                int const index,
                                KIM_SpeciesName * const speciesName)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::SpeciesName specN;
  return pmodel->get_model_species(index, &specN);
  (*speciesName) = makeSpecNameC(specN);
}

int KIM_Model_get_species_code(KIM_Model const * const model,
                               KIM_SpeciesName const speciesName,
                               int * const code)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_species_code(makeSpecNameCpp(speciesName), code);
}

void KIM_Model_get_num_params(KIM_Model const * const model,
                              int * const numberOfParameters)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_num_params(numberOfParameters);
}

int KIM_Model_get_parameter_data_type(KIM_Model const * const model,
                                      int const index,
                                      KIM_ParameterDataType * const dataType)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::ParameterDataType typ;
  int err = pmodel->get_parameter_data_type(index, &typ);
  if (err) return err;

  *dataType = makeParameterDataTypeC(typ);
  return err;
}

int KIM_Model_get_parameter(KIM_Model * const model, int const index,
                            int * const extent, void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_parameter(index, extent, ptr);
}

int KIM_Model_get_parameter_description(KIM_Model const * const model,
                                        int const index,
                                        char const ** const description)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  static std::string str;
  int err = pmodel->get_parameter_description(index, &str);
  if (err) return err;
  *description = str.c_str();
  return err;
}

void KIM_Model_set_sim_buffer(KIM_Model * const model, void const * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->set_sim_buffer(ptr);
}

void KIM_Model_get_sim_buffer(KIM_Model const * const model, void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_sim_buffer(ptr);
}

int KIM_Model_get_model_kim_string(char const * const modelName,
                                   char const ** const kimString)
{
  static std::string kimSTR;
  int err = KIM::Model::get_model_kim_string(modelName, &kimSTR);
  *kimString = kimSTR.c_str();
  return err;
}

int KIM_Model_get_model_kim_string_length(char const * const modelName,
                                          int * const kimStringLength)
{
  std::string kimSTR;
  int err = KIM::Model::get_model_kim_string(modelName, &kimSTR);
  if (err) return err;

  *kimStringLength = kimSTR.length();
  return err;
}

}  // extern "C"
