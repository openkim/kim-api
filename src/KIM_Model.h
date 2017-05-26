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

#ifndef KIM_LANGUAGE_NAME_H_
#include "KIM_LanguageName.h"
#endif

/* Forward declarations */
struct KIM_SpeciesName;
#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

#ifndef KIM_PARAMETER_DATA_TYPE_DEFINED_
#define KIM_PARAMETER_DATA_TYPE_DEFINED_
typedef struct KIM_ParameterDataType KIM_ParameterDataType;
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

#ifndef KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_DEFINED_
#define KIM_COMPUTE_MODEL_COMPUTE_ARGUMENTS_DEFINED_
typedef struct KIM_COMPUTE_ModelComputeArguments
KIM_COMPUTE_ModelComputeArguments;
#endif

struct KIM_Model {
  void * p;
};

#ifndef KIM_MODEL_DEFINED_
#define KIM_MODEL_DEFINED_
typedef struct KIM_Model KIM_Model;
#endif

int KIM_Model_create(char const * const simulatorString,
                     char const * const modelName,
                     KIM_Model ** const model);
void KIM_Model_destroy(KIM_Model ** const model);

int KIM_Model_create_compute_arguments(
    KIM_Model const * const model,
    KIM_COMPUTE_ModelComputeArguments ** const arguments);

void KIM_Model_destroy_compute_arguments(
    KIM_Model const * const model,
    KIM_COMPUTE_ModelComputeArguments ** const arguments);

void KIM_Model_get_influence_distance(KIM_Model const * const model,
                                      double * const influenceDistance);

void KIM_Model_get_cutoffs(KIM_Model const * const model,
                           int * const numberOfCutoffs,
                           double const ** const cutoffs);

void KIM_Model_print(KIM_Model const * const model);

int KIM_Model_compute(
    KIM_Model const * const model,
    KIM_COMPUTE_ModelComputeArguments const * const arguments);
int KIM_Model_reinit(KIM_Model * const model);

void KIM_Model_get_num_model_species(KIM_Model const * const model,
                                     int * const numberOfSpecies);
int KIM_Model_get_model_species(KIM_Model const * const model, int const index,
                                KIM_SpeciesName * const speciesName);

/* @@@ these will go away */
int KIM_Model_get_model_kim_string_length(char const * const modelName,
                                          int * const kimStringLength);
int KIM_Model_get_model_kim_string(char const * const modelName,
                                   char const ** const kimString);


int KIM_Model_get_species_code(KIM_Model const * const model,
                               KIM_SpeciesName const speciesName,
                               int * const code);

void KIM_Model_get_num_params(KIM_Model const * const model,
                              int * const numberOfParameters);
int KIM_Model_get_parameter_data_type(KIM_Model const * const model,
                                      int const index,
                                      KIM_ParameterDataType * const dataType);
int KIM_Model_get_parameter(KIM_Model * const model, int const index,
                            int * const extent, void ** const ptr);
int KIM_Model_get_parameter_description(KIM_Model const * const model,
                                        int const index,
                                        char const ** const description);

void KIM_Model_set_sim_buffer(KIM_Model * const model, void const * const ptr);
void KIM_Model_get_sim_buffer(KIM_Model const * const model, void ** const ptr);

#endif  /* KIM_MODEL_H_ */
