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


#ifndef KIM_MODEL_DRIVER_INITIALIZATION_H_
#define KIM_MODEL_DRIVER_INITIALIZATION_H_

#ifndef KIM_FUNC_H_
#include "KIM_func.h"
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
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

#ifndef KIM_ATTRIBUTE_DEFINED_
#define KIM_ATTRIBUTE_DEFINED_
typedef struct KIM_Attribute KIM_Attribute;
#endif

#ifndef KIM_ARGUMENT_NAME_DEFINED_
#define KIM_ARGUMENT_NAME_DEFINED_
typedef struct KIM_ArgumentName KIM_ArgumentName;
#endif

#ifndef KIM_CALL_BACK_NAME_DEFINED_
#define KIM_CALL_BACK_NAME_DEFINED_
typedef struct KIM_CallBackName KIM_CallBackName;
#endif


struct KIM_ModelDriverInitialization {
  void * p;
};

#ifndef KIM_MODEL_DRIVER_INITIALIZATION_DEFINED_
#define KIM_MODEL_DRIVER_INITIALIZATION_DEFINED_
typedef struct KIM_ModelDriverInitialization KIM_ModelDriverInitialization;
#endif


void KIM_ModelDriverInitialization_get_number_of_parameter_files(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    int * const numberOfParameterFiles);

int KIM_ModelDriverInitialization_get_parameter_file_name(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    int const index, char const ** const parameterFileName);

int KIM_ModelDriverInitialization_set_model_numbering(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_Numbering const numbering);

void KIM_ModelDriverInitialization_set_influence_distance(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    double * const influenceDistance);

void KIM_ModelDriverInitialization_set_cutoffs(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    int const numberOfCutoffs, double const * const cutoffs);

/* *method functions */
int KIM_ModelDriverInitialization_set_reinit(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_LanguageName const languageName, func * const fptr);
int KIM_ModelDriverInitialization_set_destroy(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_LanguageName const languageName, func * const fptr);
int KIM_ModelDriverInitialization_set_compute_func(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_LanguageName const languageName, func * const fptr);

int KIM_ModelDriverInitialization_set_species_code(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_SpeciesName const speciesName, int const code);

int KIM_ModelDriverInitialization_set_argument_attribute(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_ArgumentName const argumentName, KIM_Attribute const attribute);

int KIM_ModelDriverInitialization_set_call_back_attribute(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_CallBackName const callBackName, KIM_Attribute const attribute);

int KIM_ModelDriverInitialization_set_parameter_int(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    int const extent, int * const ptr, char const * const description);

int KIM_ModelDriverInitialization_set_parameter_double(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    int const extent, double * const ptr, char const * const description);

void KIM_ModelDriverInitialization_set_model_buffer(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    void * const ptr);

/* Unit_Handling related routines */
int KIM_ModelDriverInitialization_set_units(
    KIM_ModelDriverInitialization * const modelDriverInitialization,
    KIM_LengthUnit const lengthUnit,
    KIM_EnergyUnit const energyUnit,
    KIM_ChargeUnit const chargeUnit,
    KIM_TemperatureUnit const temperatureUnit,
    KIM_TimeUnit const timeUnit);

int KIM_ModelDriverInitialization_convert_unit(
    KIM_ModelDriverInitialization const * const modelDriverInitialization,
    KIM_LengthUnit const fromLengthUnit,
    KIM_EnergyUnit const fromEnergyUnit,
    KIM_ChargeUnit const fromChargeUnit,
    KIM_TemperatureUnit const fromTemperatureUnit,
    KIM_TimeUnit const fromTimeUnit,
    KIM_LengthUnit const toLengthUnit,
    KIM_EnergyUnit const toEnergyUnit,
    KIM_ChargeUnit const toChargeUnit,
    KIM_TemperatureUnit const toTemperatureUnit,
    KIM_TimeUnit const toTimeUnit,
    double const lengthExponent,
    double const energyExponent,
    double const chargeExponent,
    double const temperatureExponent,
    double const timeExponent,
    double * const conversionFactor);

void KIM_ModelDriverInitialization_Log(
    KIM_ModelDriverInitialization const * const modelDriverInitialization,
    KIM_LogVerbosity const logVerbosity, char const * const message,
    int const lineNumber, char const * const fileName);

char const * const KIM_ModelDriverInitialization_string(
    KIM_ModelDriverInitialization const * const modelDriverInitialization);

#endif  /* KIM_MODEL_DRIVE_INITIALIZATION_H_ */
