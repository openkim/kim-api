/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2022, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */
/* SPDX-License-Identifier: LGPL-2.1-or-later                                 */
/*                                                                            */
/* This library is free software; you can redistribute it and/or              */
/* modify it under the terms of the GNU Lesser General Public                 */
/* License as published by the Free Software Foundation; either               */
/* version 2.1 of the License, or (at your option) any later version.         */
/*                                                                            */
/* This library is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          */
/* Lesser General Public License for more details.                            */
/*                                                                            */
/* You should have received a copy of the GNU Lesser General Public License   */
/* along with this library; if not, write to the Free Software Foundation,    */
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_MODEL_CREATE_H_
#define KIM_MODEL_CREATE_H_

#ifndef KIM_FUNCTION_TYPES_H_
#include "KIM_FunctionTypes.h" /* IWYU pragma: export */
#endif

/* Forward declarations */
#ifndef KIM_LOG_VERBOSITY_DEFINED_
#define KIM_LOG_VERBOSITY_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LogVerbosity KIM_LogVerbosity;
#endif

#ifndef KIM_LANGUAGE_NAME_DEFINED_
#define KIM_LANGUAGE_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LanguageName KIM_LanguageName;
#endif

#ifndef KIM_NUMBERING_DEFINED_
#define KIM_NUMBERING_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_Numbering KIM_Numbering;
#endif

#ifndef KIM_MODEL_ROUTINE_NAME_DEFINED_
#define KIM_MODEL_ROUTINE_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelRoutineName KIM_ModelRoutineName;
#endif

#ifndef KIM_SPECIES_NAME_DEFINED_
#define KIM_SPECIES_NAME_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_SpeciesName KIM_SpeciesName;
#endif

#ifndef KIM_LENGTH_UNIT_DEFINED_
#define KIM_LENGTH_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_LengthUnit KIM_LengthUnit;
#endif

#ifndef KIM_ENERGY_UNIT_DEFINED_
#define KIM_ENERGY_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_EnergyUnit KIM_EnergyUnit;
#endif

#ifndef KIM_CHARGE_UNIT_DEFINED_
#define KIM_CHARGE_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_ChargeUnit KIM_ChargeUnit;
#endif

#ifndef KIM_TEMPERATURE_UNIT_DEFINED_
#define KIM_TEMPERATURE_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_TemperatureUnit KIM_TemperatureUnit;
#endif

#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
/**
 ** \brief Forward declaration.
 **
 ** \since 2.0
 **/
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif


#ifndef KIM_MODEL_CREATE_DEFINED_
#define KIM_MODEL_CREATE_DEFINED_
/**
 ** \brief \copybrief KIM::ModelCreate
 **
 ** \sa KIM::ModelCreate, kim_model_create_module::kim_model_create_handle_type
 **
 ** \since 2.0
 **/
typedef struct KIM_ModelCreate KIM_ModelCreate;
#endif


/**
 ** \brief \copybrief KIM::ModelCreate::SetModelNumbering
 **
 ** \sa KIM::ModelCreate::SetModelNumbering,
 ** kim_model_create_module::kim_set_model_numbering
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_SetModelNumbering(KIM_ModelCreate * const modelCreate,
                                      KIM_Numbering const numbering);

/**
 ** \brief \copybrief KIM::ModelCreate::SetInfluenceDistancePointer
 **
 ** \sa KIM::ModelCreate::SetInfluenceDistancePointer,
 ** kim_model_create_module::kim_set_influence_distance_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelCreate_SetInfluenceDistancePointer(
    KIM_ModelCreate * const modelCreate,
    double const * const influenceDistance);

/**
 ** \brief \copybrief KIM::ModelCreate::SetNeighborListPointers
 **
 ** \sa KIM::ModelCreate::SetNeighborListPointers,
 ** kim_model_create_module::kim_set_neighbor_list_pointers
 **
 ** \since 2.0
 **/
void KIM_ModelCreate_SetNeighborListPointers(
    KIM_ModelCreate * const modelCreate,
    int const numberOfNeighborLists,
    double const * const cutoffs,
    int const * const modelWillNotRequestNeighborsOfNoncontributingParticles);

/**
 ** \brief \copybrief KIM::ModelCreate::SetRoutinePointer
 **
 ** \sa KIM::ModelCreate::SetRoutinePointer,
 ** kim_model_create_module::kim_set_routine_pointer
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_SetRoutinePointer(
    KIM_ModelCreate * const modelCreate,
    KIM_ModelRoutineName const modelRoutineName,
    KIM_LanguageName const languageName,
    int const required,
    KIM_Function * const fptr);

/**
 ** \brief \copybrief KIM::ModelCreate::SetSpeciesCode
 **
 ** \sa KIM::ModelCreate::SetSpeciesCode,
 ** kim_model_create_module::kim_set_species_code
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_SetSpeciesCode(KIM_ModelCreate * const modelCreate,
                                   KIM_SpeciesName const speciesName,
                                   int const code);

/**
 ** \brief \copybrief KIM::ModelCreate::SetParameterPointer
 **
 ** \sa KIM::ModelCreate::SetParameterPointer,
 ** kim_model_create_module::kim_set_parameter_pointer
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_SetParameterPointerInteger(
    KIM_ModelCreate * const modelCreate,
    int const extent,
    int * const ptr,
    char const * const name,
    char const * const description);

/**
 ** \brief \copybrief KIM::ModelCreate::SetParameterPointer
 **
 ** \sa KIM::ModelCreate::SetParameterPointer,
 ** kim_model_create_module::kim_set_parameter_pointer
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_SetParameterPointerDouble(
    KIM_ModelCreate * const modelCreate,
    int const extent,
    double * const ptr,
    char const * const name,
    char const * const description);

/**
 ** \brief \copybrief KIM::ModelCreate::SetModelBufferPointer
 **
 ** \sa KIM::ModelCreate::SetModelBufferPointer,
 ** kim_model_create_module::kim_set_model_buffer_pointer
 **
 ** \since 2.0
 **/
void KIM_ModelCreate_SetModelBufferPointer(KIM_ModelCreate * const modelCreate,
                                           void * const ptr);

/**
 ** \brief \copybrief KIM::ModelCreate::SetUnits
 **
 ** \sa KIM::ModelCreate::SetUnits, kim_model_create_module::kim_set_units
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_SetUnits(KIM_ModelCreate * const modelCreate,
                             KIM_LengthUnit const lengthUnit,
                             KIM_EnergyUnit const energyUnit,
                             KIM_ChargeUnit const chargeUnit,
                             KIM_TemperatureUnit const temperatureUnit,
                             KIM_TimeUnit const timeUnit);

/**
 ** \brief \copybrief KIM::ModelCreate::ConvertUnit
 **
 ** \sa KIM::ModelCreate::ConvertUnit,
 ** kim_model_create_module::kim_convert_unit
 **
 ** \since 2.0
 **/
int KIM_ModelCreate_ConvertUnit(KIM_LengthUnit const fromLengthUnit,
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

/**
 ** \brief \copybrief KIM::ModelCreate::LogEntry
 **
 ** \sa KIM::ModelCreate::LogEntry, kim_model_create_module::kim_log_entry
 **
 ** \since 2.0
 **/
void KIM_ModelCreate_LogEntry(KIM_ModelCreate const * const modelCreate,
                              KIM_LogVerbosity const logVerbosity,
                              char const * const message,
                              int const lineNumber,
                              char const * const fileName);

/**
 ** \brief \copybrief KIM::ModelCreate::ToString
 **
 ** \sa KIM::ModelCreate::ToString, kim_model_create_module::kim_to_string
 **
 ** \since 2.0
 **/
char const *
KIM_ModelCreate_ToString(KIM_ModelCreate const * const modelCreate);

#endif /* KIM_MODEL_CREATE_H_ */
