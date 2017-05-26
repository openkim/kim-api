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
extern "C"
{
#ifndef KIM_LOG_VERBOSITY_H_
#include "KIM_LogVerbosity.h"
#endif
}  // extern "C"

#ifndef KIM_SPECIES_NAME_HPP_
#include "KIM_SpeciesName.hpp"
#endif
extern "C"
{
#ifndef KIM_SPECIES_NAME_H_
#include "KIM_SpeciesName.h"
#endif
}  // extern "C"

#ifndef KIM_LANGUAGE_NAME_HPP_
#include "KIM_LanguageName.hpp"
#endif
extern "C"
{
#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif
}  // extern "C"

#ifndef KIM_DATA_TYPE_HPP_
#include "KIM_DataType.hpp"
#endif
extern "C"
{
#ifndef KIM_DATA_TYPE_H_
#include "KIM_DataType.h"
#endif
}  // extern "C"

#ifndef KIM_NUMBERING_HPP_
#include "KIM_Numbering.hpp"
#endif
extern "C"
{
#ifndef KIM_NUMBERING_H_
#include "KIM_Numbering.h"
#endif
}  // extern "C"

#ifndef KIM_MODEL_HPP_
#include "KIM_Model.hpp"
#endif
extern "C"
{
#ifndef KIM_MODEL_H_
#include "KIM_Model.h"
#endif
}  // extern "C"

#ifndef KIM_ARGUMENT_NAME_HPP_
#include "KIM_ArgumentName.hpp"
#endif
extern "C"
{
#ifndef KIM_ARGUMENT_NAME_H_
#include "KIM_ArgumentName.h"
#endif
}  // extern "C"

#ifndef KIM_CALL_BACK_NAME_HPP_
#include "KIM_CallBackName.hpp"
#endif
extern "C"
{
#ifndef KIM_CALL_BACK_NAME_H_
#include "KIM_CallBackName.h"
#endif
}  // extern "C"

#ifndef KIM_ATTRIBUTE_HPP_
#include "KIM_Attribute.hpp"
#endif
extern "C"
{
#ifndef KIM_ATTRIBUTE_H_
#include "KIM_Attribute.h"
#endif
}  // extern "C"

#ifndef KIM_UNIT_SYSTEM_HPP_
#include "KIM_UnitSystem.hpp"
#endif
extern "C"
{
#ifndef KIM_UNIT_SYSTEM_H_
#include "KIM_UnitSystem.h"
#endif
}  // extern "C"


namespace
{
KIM_DataType
makeDataTypeC(KIM::DataType const dataType)
{
  KIM_DataType typ;
  KIM_DataType * pTyp = (KIM_DataType *) &dataType;
  typ.dataTypeID = pTyp->dataTypeID;
  return typ;
}

KIM::LanguageName
makeLanguageNameCpp(KIM_LanguageName const languageName)
{
  return KIM::LanguageName(languageName.languageNameID);
}

KIM::SpeciesName makeSpecNameCpp(KIM_SpeciesName const speciesName)
{
  return KIM::SpeciesName(speciesName.speciesNameID);
}

KIM_SpeciesName makeSpecNameC(KIM::SpeciesName const speciesName)
{
  KIM_SpeciesName speciesN = {speciesName.speciesNameID};
  return speciesN;
}

KIM::ArgumentName makeArgumentNameCpp(KIM_ArgumentName const argumentName)
{
  return KIM::ArgumentName(argumentName.argumentNameID);
}

KIM::CallBackName makeCallBackNameCpp(KIM_CallBackName const callBackName)
{
  return KIM::CallBackName(callBackName.callBackNameID);
}

KIM::Attribute makeAttributeCpp(KIM_Attribute const attribute)
{
  return KIM::Attribute(attribute.attributeID);
}

KIM_Attribute const makeAttributeC(KIM::Attribute attribute)
{
  KIM_Attribute attributeC = {attribute.attributeID};
  return attributeC;
}

KIM::LengthUnit makeLengthUnitCpp(KIM_LengthUnit lengthUnit)
{
  return KIM::LengthUnit(lengthUnit.lengthUnitID);
}

KIM::EnergyUnit makeEnergyUnitCpp(KIM_EnergyUnit energyUnit)
{
  return KIM::EnergyUnit(energyUnit.energyUnitID);
}

KIM::ChargeUnit makeChargeUnitCpp(KIM_ChargeUnit chargeUnit)
{
  return KIM::ChargeUnit(chargeUnit.chargeUnitID);
}

KIM::TemperatureUnit makeTemperatureUnitCpp(KIM_TemperatureUnit temperatureUnit)
{
  return KIM::TemperatureUnit(temperatureUnit.temperatureUnitID);
}

KIM::TimeUnit makeTimeUnitCpp(KIM_TimeUnit timeUnit)
{
  return KIM::TimeUnit(timeUnit.timeUnitID);
}

KIM::Numbering makeNumberingCpp(KIM_Numbering numbering)
{
  return KIM::Numbering(numbering.numberingID);
}

KIM::LogVerbosity makeLogVerbosityCpp(KIM_LogVerbosity logVerbosity)
{
  return KIM::LogVerbosity(logVerbosity.logVerbosityID);
}

}  // namespace


extern "C"
{
int KIM_Model_create(KIM_Numbering const numbering,
                     KIM_LengthUnit const requestedLengthUnit,
                     KIM_EnergyUnit const requestedEnergyUnit,
                     KIM_ChargeUnit const requestedChargeUnit,
                     KIM_TemperatureUnit const requestedTemperatureUnit,
                     KIM_TimeUnit const requestedTimeUnit,
                     char const * const modelName,
                     int * const requestedUnitsAccepted,
                     KIM_Model ** const model)
{
  std::string modelNameC(modelName);
  KIM::Model * pmodel;
  int err = KIM::Model::create(
      makeNumberingCpp(numbering),
      makeLengthUnitCpp(requestedLengthUnit),
      makeEnergyUnitCpp(requestedEnergyUnit),
      makeChargeUnitCpp(requestedChargeUnit),
      makeTemperatureUnitCpp(requestedTemperatureUnit),
      makeTimeUnitCpp(requestedTimeUnit),
      modelNameC,
      requestedUnitsAccepted,
      &pmodel);
  if (err)
  {
    return true;
  }
  else
  {
    (*model) = new KIM_Model;
    (*model)->p = (void *) pmodel;
    return false;
  }
}

void KIM_Model_destroy(KIM_Model ** const model)
{
  KIM::Model * pmodel = (KIM::Model *) (*model)->p;
  KIM::Model::destroy(&pmodel);
  delete (*model);
  *model = 0;
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

int KIM_Model_get_argument_attribute(KIM_Model const * const model,
                                     KIM_ArgumentName const argumentName,
                                     KIM_Attribute * const attribute)
{
  KIM::Attribute attributeCpp;
  KIM::Model * pModel = (KIM::Model *) model->p;
  int error = pModel->get_argument_attribute(makeArgumentNameCpp(argumentName),
                                             &attributeCpp);
  if (error)
    return true;
  else
  {
    *attribute = makeAttributeC(attributeCpp);
    return false;
  }
}

int KIM_Model_get_call_back_attribute(KIM_Model const * const model,
                                      KIM_CallBackName const CallBackName,
                                      KIM_Attribute * const attribute)
{
  KIM::Attribute attributeCpp;
  KIM::Model * pModel = (KIM::Model *) model->p;
  int error = pModel->get_call_back_attribute(makeCallBackNameCpp(CallBackName),
                                              &attributeCpp);
  if (error)
    return true;
  else
  {
    *attribute = makeAttributeC(attributeCpp);
    return false;
  }
}

void KIM_Model_get_units(KIM_Model const * const model,
                         KIM_LengthUnit * const lengthUnit,
                         KIM_EnergyUnit * const energyUnit,
                         KIM_ChargeUnit * const chargeUnit,
                         KIM_TemperatureUnit * const temperatureUnit,
                         KIM_TimeUnit * const timeUnit)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  pModel->get_units(
      reinterpret_cast<KIM::LengthUnit *>(lengthUnit),
      reinterpret_cast<KIM::EnergyUnit *>(energyUnit),
      reinterpret_cast<KIM::ChargeUnit *>(chargeUnit),
      reinterpret_cast<KIM::TemperatureUnit *>(temperatureUnit),
      reinterpret_cast<KIM::TimeUnit *>(timeUnit));
}


// *data functions
int KIM_Model_set_data_int(KIM_Model * const model,
                           KIM_ArgumentName const argumentName,
                           int const * const ptr)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  KIM::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pModel->set_data(argN, ptr);
}

int KIM_Model_set_data_double(KIM_Model * const model,
                              KIM_ArgumentName const argumentName,
                              double const * const ptr)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  KIM::ArgumentName argN = makeArgumentNameCpp(argumentName);
  return pModel->set_data(argN, ptr);
}

int KIM_Model_set_call_back(KIM_Model * const model,
                            KIM_CallBackName const callBackName,
                            KIM_LanguageName const languageName,
                            func * const fptr,
                            void const * const dataObject)
{
  KIM::Model * pModel = (KIM::Model *) model->p;
  KIM::CallBackName callBackNameCpp = makeCallBackNameCpp(callBackName);
  KIM::LanguageName langN = makeLanguageNameCpp(languageName);
  return pModel->set_call_back(callBackNameCpp, langN, fptr, dataObject);
}

int KIM_Model_compute(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->compute();
}

int KIM_Model_ClearInfluenceDistanceAndCutofsThenReinitializeModel(
    KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->ClearInfluenceDistanceAndCutoffsThenReinitializeModel();
}

int KIM_Model_get_species_support_and_code(KIM_Model const * const model,
                                           KIM_SpeciesName const speciesName,
                                           int * const speciesIsSupported,
                                           int * const code)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return pmodel->get_species_support_and_code(makeSpecNameCpp(speciesName),
                                              speciesIsSupported, code);
}

void KIM_Model_get_num_params(KIM_Model const * const model,
                              int * const numberOfParameters)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_num_params(numberOfParameters);
}

int KIM_Model_get_parameter_data_type_and_description(
    KIM_Model const * const model, int const index,
    KIM_DataType * const dataType, char const ** const description)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::DataType typ;
  static std::string str;

  KIM::DataType *pTyp;
  if (dataType == 0)
    pTyp = 0;
  else
    pTyp = &typ;

  std::string * pStr;
  if (description == 0)
    pStr = 0;
  else
    pStr = &str;

  int error
      = pmodel->get_parameter_data_type_and_description(index, pTyp, pStr);

  if (error)
    return true;
  else
  {
    if (dataType != 0) *dataType = makeDataTypeC(typ);
    if (description != 0) *description = str.c_str();
    return false;
  }
}

int KIM_Model_get_parameter_int_extent_and_pointer(KIM_Model * const model,
                                                   int const index,
                                                   int * const extent,
                                                   int ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::DataType typ;
  static std::string str;

  int error = pmodel->get_parameter_extent_and_pointer(index, extent, ptr);

  if (error)
    return true;
  else
    return false;
}

int KIM_Model_get_parameter_double_extent_and_pointer(KIM_Model * const model,
                                                      int const index,
                                                      int * const extent,
                                                      double ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  KIM::DataType typ;
  static std::string str;

  int error = pmodel->get_parameter_extent_and_pointer(index, extent, ptr);

  if (error)
    return true;
  else
    return false;
}

void KIM_Model_set_sim_buffer(KIM_Model * const model, void * const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->set_sim_buffer(ptr);
}

void KIM_Model_get_sim_buffer(KIM_Model const * const model, void ** const ptr)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->get_sim_buffer(ptr);
}

char const * const KIM_Model_string(KIM_Model const * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  return (pmodel->string()).c_str();
}

void KIM_Model_SetLogID(KIM_Model * const model, char const * const logID)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->SetLogID(logID);
}

void KIM_Model_PushLogId(KIM_Model * const model,
                         KIM_LogVerbosity const logVerbosity)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->PushLogVerbosity(makeLogVerbosityCpp(logVerbosity));
}

void KIM_Model_PopLogId(KIM_Model * const model)
{
  KIM::Model * pmodel = (KIM::Model *) model->p;
  pmodel->PopLogVerbosity();
}

}  // extern "C"
