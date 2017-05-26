/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2017, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_MODEL_H_
#define KIM_MODEL_H_

#include <stdarg.h>

#ifndef KIM_FUNC_H_
#include "KIM_func.h"
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_NAME_DEFINED_
#define KIM_LOG_VERBOSITY_NAME_DEFINED_
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
typedef struct KIM_Numbering KIM_Numbering;
#endif

#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

#ifndef KIM_DATA_TYPE_DEFINED_
#define KIM_DATA_TYPE_DEFINED_
typedef struct KIM_DataType KIM_DataType;
#endif

#ifndef KIM_ENERGY_UNIT_DEFINED_
#define KIM_ENERGY_UNIT_DEFINED_
typedef struct KIM_EnergyUnit KIM_EnergyUnit;
#endif

#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif

#ifndef KIM_ARGUMENT_NAME_DEFINED_
#define KIM_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ArgumentName KIM_ArgumentName;
#endif

#ifndef KIM_CALL_BACK_NAME_DEFINED_
#define KIM_CALL_BACK_NAME_DEFINED_
typedef struct KIM_CallBackName KIM_CallBackName;
#endif

#ifndef KIM_ATTRIBUTE_DEFINED_
#define KIM_ATTRIBUTE_DEFINED_
typedef struct KIM_Attribute KIM_Attribute;
#endif


struct KIM_Model
{
  void * p;
};

#ifndef KIM_MODEL_DEFINED_
#define KIM_MODEL_DEFINED_
typedef struct KIM_Model KIM_Model;
#endif

int KIM_Model_create(KIM_Numbering const numbering,
                     KIM_LengthUnit const requestedLengthUnit,
                     KIM_EnergyUnit const requestedEnergyUnit,
                     KIM_ChargeUnit const requestedChargeUnit,
                     KIM_TemperatureUnit const requestedTemperatureUnit,
                     KIM_TimeUnit const requestedTimeUnit,
                     char const * const modelName,
                     int * const requestedUnitsAccepted,
                     KIM_Model ** const model);
void KIM_Model_destroy(KIM_Model ** const model);

void KIM_Model_get_influence_distance(KIM_Model const * const model,
                                      double * const influenceDistance);

void KIM_Model_get_cutoffs(KIM_Model const * const model,
                           int * const numberOfCutoffs,
                           double const ** const cutoffs);

int KIM_Model_get_argument_attribute(KIM_Model const * const model,
                                     KIM_ArgumentName const argumentName,
                                     KIM_Attribute * const attribute);
int KIM_Model_get_call_back_attribute(KIM_Model const * const model,
                                      KIM_CallBackName const callBackName,
                                      KIM_Attribute * const attribute);

void KIM_Model_get_units(KIM_Model const * const model,
                         KIM_LengthUnit * const lengthUnit,
                         KIM_EnergyUnit * const energyUnit,
                         KIM_ChargeUnit * const chargeUnit,
                         KIM_TemperatureUnit * const temperatureUnit,
                         KIM_TimeUnit * const timeUnit);


/* *data functions */
int KIM_Model_set_data_int(KIM_Model * const model,
                           KIM_ArgumentName const argumentName,
                           int const * const ptr);

int KIM_Model_set_data_double(KIM_Model * const model,
                              KIM_ArgumentName const argumentName,
                              double const * const ptr);
int KIM_Model_set_call_back(KIM_Model * const model,
                            KIM_CallBackName const callBackName,
                            KIM_LanguageName const languageName,
                            func * const fptr,
                            void const * const dataObject);

int KIM_Model_compute(KIM_Model const * const model);
int KIM_Model_ClearInfluenceDistanceAndCutoffsThenReinitializeModel(
    KIM_Model * const model);

int KIM_Model_get_species_support_and_code(KIM_Model const * const model,
                                           KIM_SpeciesName const speciesName,
                                           int * const speciesIsSupported,
                                           int * const code);

void KIM_Model_get_num_params(KIM_Model const * const model,
                              int * const numberOfParameters);
int KIM_Model_get_parameter_data_type_and_description(
    KIM_Model const * const model, int const index,
    KIM_DataType * const dataType, char const ** const description);
int KIM_Model_get_parameter_int_extent_and_pointer(
    KIM_Model * const model, int const index,
    int * const extent, int ** const ptr);
int KIM_Model_get_parameter_double_extent_and_pointer(
    KIM_Model * const model, int const index,
    int * const extent, double ** const ptr);

void KIM_Model_set_sim_buffer(KIM_Model * const model, void * const ptr);
void KIM_Model_get_sim_buffer(KIM_Model const * const model, void ** const ptr);

char const * const KIM_Model_string(KIM_Model const * const model);

void KIM_Model_SetLogID(KIM_Model * const model, char const * const logID);
void KIM_Model_PushLogVerbosity(KIM_Model * const model,
                                KIM_LogVerbosity const logVerbosity);
void KIM_Model_PopLogVerbosity(KIM_Model * const model);

#endif  /* KIM_MODEL_H_ */
