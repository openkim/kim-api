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


#include "old_KIM_API_status.h"

typedef void (func)();


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

struct KIM_COMPUTE_ArgumentName;
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_COMPUTE_ArgumentName KIM_COMPUTE_ArgumentName;
#endif

struct KIM_COMPUTE_LanguageName;
#ifndef KIM_COMPUTE_LANGUAGE_NAME_DEFINED_
#define KIM_COMPUTE_LANGUAGE_NAME_DEFINED_
typedef struct KIM_COMPUTE_LanguageName KIM_COMPUTE_LanguageName;
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

/* *data functions */
int KIM_Model_get_data(KIM_Model const * const model,
                       KIM_COMPUTE_ArgumentName const argumentName,
                       void ** const ptr);
int KIM_Model_set_data(KIM_Model * const model,
                       KIM_COMPUTE_ArgumentName const argumentName,
                       int const extent,
                       void const * const ptr);

/* *method functions */
int KIM_Model_get_method(KIM_Model const * const model,
                         KIM_COMPUTE_ArgumentName const argumentName,
                         KIM_COMPUTE_LanguageName * const languageName,
                         func ** const fptr);
int KIM_Model_set_method(KIM_Model * const model,
                         KIM_COMPUTE_ArgumentName const argumentName,
                         int const extent,
                         KIM_COMPUTE_LanguageName const languageName,
                         func * const fptr);

/* *compute functions */
int KIM_Model_get_compute(KIM_Model const * const model,
                          KIM_COMPUTE_ArgumentName const argumentName,
                          int * const flag);
int KIM_Model_set_compute(KIM_Model * const model,
                          KIM_COMPUTE_ArgumentName const argumentName,
                          int flag);

int KIM_Model_get_size(KIM_Model const * const model,
                       KIM_COMPUTE_ArgumentName const argumentName,
                       int * const size);

void KIM_Model_print(KIM_Model const * const model);

int KIM_Model_compute(KIM_Model const * const model);
int KIM_Model_get_neigh(KIM_Model const * const model, int particleNumber,
                        int * const numberOfNeighbors,
                        int const ** const neighborsOfParticle);
int KIM_Model_init(KIM_Model * const model);
int KIM_Model_reinit(KIM_Model * const model);
int KIM_Model_destroy_model(KIM_Model * const model);

void KIM_Model_get_num_model_species(KIM_Model const * const model,
                                     int * const numberOfSpecies);
int KIM_Model_get_model_species(KIM_Model const * const model, int const index,
                                KIM_SpeciesName * const speciesName);

/* @@@ these will go away */
void KIM_Model_get_num_sim_species(KIM_Model const * const model,
                                   int * const numberOfSpecies);
int KIM_Model_get_sim_species(KIM_Model const * const model, int const index,
                              KIM_SpeciesName * const speciesName);
int KIM_Model_get_model_kim_string_length(char const * const modelName,
                                          int * const kimStringLength);
int KIM_Model_get_model_kim_string(char const * const modelName,
                                   char const ** const kimString);


int KIM_Model_get_species_code(KIM_Model const * const model,
                               KIM_SpeciesName const speciesName,
                               int * const code);
/* @@@ do we keep this mechanism, make it mandatory, or remove? */
int KIM_Model_set_species_code(KIM_Model * const model,
                               KIM_SpeciesName const speciesName,
                               int const code);

void KIM_Model_get_num_params(KIM_Model const * const model,
                              int * const numberOfParameters);
int KIM_Model_get_parameter_data_type(KIM_Model const * const model,
                                      int const index,
                                      KIM_ParameterDataType * const dataType);
int KIM_Model_get_parameter(KIM_Model * const model, int const index,
                            int * const extent, void ** const ptr);
int KIM_Model_set_parameter(KIM_Model * const model, int const index,
                            int const extent, void * const ptr);
int KIM_Model_get_parameter_description(KIM_Model const * const model,
                                        int const index,
                                        char const ** const description);
int KIM_Model_set_parameter_description(KIM_Model * const model,
                                        int const index,
                                        char const * const description);

void KIM_Model_set_model_buffer(KIM_Model * const model,
                                void const * const ptr);
void KIM_Model_get_model_buffer(KIM_Model const * const model,
                                void ** const ptr);
void KIM_Model_set_sim_buffer(KIM_Model * const model, void const * const ptr);
void KIM_Model_get_sim_buffer(KIM_Model const * const model, void ** const ptr);

int KIM_Model_process_dEdr(KIM_Model const * const model, double const de,
                           double const r, double const * const dx, int const i,
                           int const j);

int KIM_Model_process_d2Edr2(KIM_Model const * const model, double const de,
                             double const * const r, double const * const dx,
                             int const * const i, int const * const j);

/* Unit_Handling related routines */
int KIM_Model_get_unit_handling(KIM_Model const * const model,
                                int * const flag);
void KIM_Model_get_unit_length(KIM_Model const * const model,
                               KIM_LengthUnit * const length);
void KIM_Model_get_unit_energy(KIM_Model const * const model,
                               KIM_EnergyUnit * const energy);
void KIM_Model_get_unit_charge(KIM_Model const * const model,
                               KIM_ChargeUnit * const charge);
void KIM_Model_get_unit_temperature(KIM_Model const * const model,
                                    KIM_TemperatureUnit * const temperature);
void KIM_Model_get_unit_time(KIM_Model const * const model,
                             KIM_TimeUnit * const time);
int KIM_Model_convert_to_act_unit(KIM_Model const * const model,
                                  KIM_LengthUnit const length,
                                  KIM_EnergyUnit const energy,
                                  KIM_ChargeUnit const charge,
                                  KIM_TemperatureUnit const temperature,
                                  KIM_TimeUnit const time,
                                  double const length_exponent,
                                  double const energy_exponent,
                                  double const charge_exponent,
                                  double const temperature_exponent,
                                  double const time_exponent,
                                  double * const factor);

#endif  /* KIM_MODEL_H_ */
