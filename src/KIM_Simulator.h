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


#ifndef KIM_SIMULATOR_H_
#define KIM_SIMULATOR_H_

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

struct KIM_COMPUTE_ArgumentName;
#ifndef KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
#define KIM_COMPUTE_ARGUMENT_NAME_DEFINED_
typedef struct KIM_COMPUTE_ArgumentName KIM_COMPUTE_ArgumentName;
#endif


struct KIM_Simulator {
  void * p;
};

#ifndef KIM_SIMULATOR_DEFINED_
#define KIM_SIMULATOR_DEFINED_
typedef struct KIM_Simulator KIM_Simulator;
#endif

void KIM_Simulator_set_influence_distance(KIM_Simulator * const simulator,
                                          double * const influenceDistance);

void KIM_Simulator_set_cutoffs(KIM_Simulator * const simulator,
                               int const numberOfCutoffs,
                               double const * const cutoffs);

/* *method functions */
void KIM_Simulator_set_reinit(KIM_Simulator * const simulator,
                              KIM_LanguageName const languageName,
                              func * const fptr);
void KIM_Simulator_set_destroy(KIM_Simulator * const simulator,
                               KIM_LanguageName const languageName,
                               func * const fptr);
void KIM_Simulator_set_compute_func(KIM_Simulator * const simulator,
                                    KIM_LanguageName const languageName,
                                    func * const fptr);

void KIM_Simulator_print(KIM_Simulator const * const simulator);

void KIM_Simulator_get_num_model_species(KIM_Simulator const * const simulator,
                                         int * const numberOfSpecies);
int KIM_Simulator_get_model_species(KIM_Simulator const * const simulator,
                                    int const index,
                                    KIM_SpeciesName * const speciesName);

/* @@@ these will go away */
void KIM_Simulator_get_num_sim_species(KIM_Simulator const * const simulator,
                                       int * const numberOfSpecies);
int KIM_Simulator_get_sim_species(KIM_Simulator const * const simulator,
                                  int const index,
                                  KIM_SpeciesName * const speciesName);

int KIM_Simulator_get_species_code(KIM_Simulator const * const simulator,
                                   KIM_SpeciesName const speciesName,
                                   int * const code);
/* @@@ do we keep this mechanism, make it mandatory, or remove? */
int KIM_Simulator_set_species_code(KIM_Simulator * const simulator,
                                   KIM_SpeciesName const speciesName,
                                   int const code);

int KIM_Simulator_set_parameter(KIM_Simulator * const simulator,
                                int const index,
                                int const extent, void * const ptr);

int KIM_Simulator_set_parameter_description(KIM_Simulator * const simulator,
                                            int const index,
                                            char const * const description);

void KIM_Simulator_set_model_buffer(KIM_Simulator * const simulator,
                                    void const * const ptr);
void KIM_Simulator_get_model_buffer(KIM_Simulator const * const simulator,
                                    void ** const ptr);

/* Unit_Handling related routines */
int KIM_Simulator_get_unit_handling(KIM_Simulator const * const simulator,
                                    int * const flag);
void KIM_Simulator_get_unit_length(KIM_Simulator const * const simulator,
                                   KIM_LengthUnit * const length);
void KIM_Simulator_get_unit_energy(KIM_Simulator const * const simulator,
                                   KIM_EnergyUnit * const energy);
void KIM_Simulator_get_unit_charge(KIM_Simulator const * const simulator,
                                   KIM_ChargeUnit * const charge);
void KIM_Simulator_get_unit_temperature(
    KIM_Simulator const * const simulator,
    KIM_TemperatureUnit * const temperature);
void KIM_Simulator_get_unit_time(KIM_Simulator const * const simulator,
                                 KIM_TimeUnit * const time);
int KIM_Simulator_convert_to_act_unit(KIM_Simulator const * const simulator,
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

#endif  /* KIM_SIMULATOR_H_ */
